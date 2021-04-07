{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vulkyrie.Resource
  ( GenericResource
  , auto
  , manual
  , auto_

  , MetaResource
  , destroy
  , create
  , metaResource

  , Resource (..)
  , region

  , ResourceOwner
  , resourceOwner
  , withResourceOwner
  , takeOwnership

  , resourceContext
  , takeContextOwnership

  , onDestroy
  , autoDestroyCreate
  , autoDestroyCreateWithUnmask
  , inverseDestruction
  ) where

import           Control.Monad
import           Data.Either
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (pack, Text)
import           Data.Typeable
import           GHC.Stack (HasCallStack, prettyCallStack, callStack)
import           UnliftIO.Concurrent
import           UnliftIO.Exception
    ( SomeException, Exception(..), finally, mask, mask_, try, throwIO, uninterruptibleMask_ )
import           UnliftIO.IORef

import           Vulkyrie.Program
import           Vulkyrie.Utils (catchAsync, throwAsyncIO)

class GenericResource res a where
  -- | Creates an action for use inside of region (see region below)
  auto :: res a -> Prog OpenResourceContext a
  -- | Creates a destructor action along with allocating the resource.
  --
  --   Has to be called within a masked scope. Pass the restore, see UnliftIO.Exception.mask.
  manual :: (forall b. Prog r b -> Prog r b) -> res a -> Prog r (Prog r' (), a)

auto_ :: GenericResource res a => res a -> Prog OpenResourceContext ()
auto_ = void . auto

-- | A MetaResource is only meant to correctly destroy resources that it created itself.
--   It can create and destroy multiple values though.
--
--   The closure used to create a MetaResource typically retains some of the
--   creation parameters, like the appropriate VkDevice. Thereby it gives the
--   freedom of not (necessarily) carrying around all information needed for
--   destruction along with the resource value.
data MetaResource a = MetaResource
  { destroy :: forall r. a -> Prog r ()
  , create :: forall r. Prog r a
  }

-- | Creates a MetaResource
metaResource :: (forall r. a -> Prog r ()) -- ^ destroy resource
             -> (forall r. Prog r a)        -- ^ create resource
             -> MetaResource a
metaResource destroy create = MetaResource {..}
{-# INLINE metaResource #-}

instance GenericResource MetaResource a where
  auto MetaResource{..} = autoDestroyCreate destroy create
  {-# INLINE auto #-}

  manual _ MetaResource{..} = do
    x <- create
    pure (destroy x, x)
  {-# INLINE manual #-}





-- | Enforces explicitness at the use site.
--
-- Prog OpenResourceContext should never ever occur in top-level type signatures
-- (outside of this file) by convention. Resource can be converted back via
-- auto, same treatment on the use site as any other GenericResource.
newtype Resource a = Resource (Prog OpenResourceContext a)

instance GenericResource Resource a where
  auto (Resource res) = res
  {-# INLINE auto #-}

  manual restore (Resource res) = do
    ctx <- makeResourceContext
    let ResourceContext{ destructorsVar } = ctx
    a <-
      restore (withResourceContext ctx res)
        `catchAsync` (\e -> readIORef destructorsVar >>= cleanup "manual" (Just e) >> throwAsyncIO e)
    return (readIORef destructorsVar >>= cleanup "manual" Nothing, a)
  {-# INLINE manual #-}

makeResourceContext :: Prog r ResourceContext
makeResourceContext = do
  destructorsVar <- newIORef []
  return ResourceContext{..}

-- | Establishes a scope for automatic resource destruction.
--
-- Warning: The destructor calls happen within mask, but not
-- uninterruptibleMask. This is a compromise to make the chance of getting stuck
-- in destructors slightly less, but might lead to incomplete destruction. Not
-- sure if this is the right decision.
region :: HasCallStack => Prog OpenResourceContext a -> Prog r a
region res = do
  let regionSrc = pack $ prettyCallStack callStack
  ctx@ResourceContext{ destructorsVar } <- makeResourceContext
  mask $ \restore -> do
    a <- restore (withResourceContext ctx res)
      `catchAsync` (\e -> readIORef destructorsVar >>= cleanup regionSrc (Just e) >> throwAsyncIO e)
    readIORef destructorsVar >>= cleanup regionSrc Nothing
    return a

data CleanupException = CleanupException
  { regionSrc :: Text
    -- ^ where the resource was registered, for error messages
  , exceptionCausingCleanup :: Maybe SomeException
  , exceptionsDuringCleanup :: NonEmpty SomeException
  }
  deriving (Show, Typeable)
instance Exception CleanupException

-- | Runs the given destructors within a masked context.
cleanup :: Text -> Maybe SomeException -> [Prog ClosedResourceContext ()] -> Prog r ()
cleanup regionSrc origException destructors =
  closedProgram $
    lefts <$> mapM try destructors >>= \case
      e : es -> throwIO (CleanupException regionSrc origException (e :| es))
      [] -> return ()

-- | For non-local registering of resources accross threads.
data ResourceOwner =
  ResourceOwner
  { destructorsMVar :: MVar [Prog ClosedResourceContext ()]
  }

-- | For non-local registering of resources accross threads.
resourceOwner :: HasCallStack => Resource ResourceOwner
resourceOwner = Resource $ do
  let regionSrc = pack $ prettyCallStack callStack
  autoDestroyCreate
    (\ResourceOwner{ destructorsMVar } ->
      uninterruptibleMask_ (takeMVar destructorsMVar) >>= cleanup regionSrc Nothing)
    (ResourceOwner <$> newMVar [])

-- | For non-local registering of resources accross threads.
--
-- Like a region, but after successful completion it registers the resource
-- destructors with the given ResourceOwner. This is thread-safe.
--
-- Warning: Only AFTER the action passed to withResourceOwner finishes, the
-- destructors get registered at the resource owner. Beware of ordering issues
-- when having multiple threads use withResourceOwner with the same owner to
-- create resources that depend on each other.
withResourceOwner :: HasCallStack => ResourceOwner -> Prog OpenResourceContext a -> Prog r a
withResourceOwner ResourceOwner{ destructorsMVar } res = do
  let regionSrc = pack $ prettyCallStack callStack
  ctx@ResourceContext{ destructorsVar } <- makeResourceContext
  mask $ \restore -> do
    a <- restore (withResourceContext ctx res)
      `catchAsync` (\e -> readIORef destructorsVar >>= cleanup regionSrc (Just e) >> throwAsyncIO e)
    destructors <- readIORef destructorsVar
    uninterruptibleMask_ $ modifyMVar_ destructorsMVar (pure . (destructors <>) )
    return a

-- | Re-registers owned resources to the current region.
--
-- Beware: This only takes ownership of resources that are owned by the resource
-- owner at the time of this call. Further resources can be registered with the
-- resource owner after that.
takeOwnership :: ResourceOwner -> Prog OpenResourceContext ()
takeOwnership ResourceOwner{ destructorsMVar = consumedDestructorsMVar } =
  uninterruptibleMask_ $ do
    destructors <- takeMVar consumedDestructorsMVar
    putMVar consumedDestructorsMVar []
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (destructors <>)

-- | Makes a sub-resource-context as a resource.
--
-- Like `ResourceOwner`, but not thread-safe.
resourceContext :: HasCallStack => Resource ResourceContext
resourceContext = Resource $ do
  let regionSrc = pack $ prettyCallStack callStack
  autoDestroyCreate
    (\ResourceContext{ destructorsVar } ->
      readIORef destructorsVar >>= cleanup regionSrc Nothing)
    makeResourceContext

-- | Re-registers resources to the current region.
--
-- Like `takeOwnership`, but not thread-safe.
takeContextOwnership :: ResourceContext -> Prog OpenResourceContext ()
takeContextOwnership ResourceContext{ destructorsVar = consumedDestructorsVar } =
  mask_ $ do
    destructors <- readIORef consumedDestructorsVar
    atomicWriteIORef consumedDestructorsVar []
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (destructors <>)



-- | Exception-safe allocation of an elementary resource.
--
-- Exceptions are also masked during allocation, like with bracket and friends.
-- Do not use this for composite resources.
autoDestroyCreate :: (forall r. a -> Prog r ()) -- ^ free resource
                  -> (forall r. Prog r a) -- ^ allocate resource
                  -> Prog OpenResourceContext a
autoDestroyCreate free alloc =
  mask_ $ do
    a <- alloc
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (free a :)
    return a
{-# INLINE autoDestroyCreate #-}

autoDestroyCreateWithUnmask :: (forall r. a -> Prog r ()) -- ^ free resource
                            -> (forall r. (Prog r b -> Prog r b) -> Prog r a) -- ^ allocate resource
                            -> Prog OpenResourceContext a
autoDestroyCreateWithUnmask free alloc =
  mask $ \unmask -> do
    a <- alloc unmask
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (free a :)
    return a
{-# INLINE autoDestroyCreateWithUnmask #-}

-- TODO: fix for nested resources
-- | Reverses destruction order.
--
-- Beware: This does not deal with nested resources correctly, as it reverses a
-- flat list of all destructors currently.
inverseDestruction :: Prog OpenResourceContext a -> Prog OpenResourceContext a
inverseDestruction res = do
  ResourceContext outerDestrVar <- askResourceContext
  mask $ \restore -> do
    ctx <- makeResourceContext
    let ResourceContext{ destructorsVar } = ctx
    restore (withResourceContext ctx res)
      `finally` do
        destructors <- readIORef destructorsVar
        modifyIORef' outerDestrVar (reverse destructors <>)

{-
asym :: Resource a -> (a -> Resource b) -> Resource b
asym ra rb =
  mask $ \restore -> do
    ResourceContext{ destructorsVar } <- askResourceContext
    (destroyA, a) <- manual restore ra
    (destroyB, b) <- manual restore (rb a)
      `finally` modifyIORef' destructorsVar (destroyA <>)
    modifyIORef' destructorsVar ((destroyA <> destroyB) <>)
    return b
-}

-- | Runs given action when destroying the resource. Destruction order is bottom to top.
--
--  Never use this if there is a corresponding creation action. If an exception
--  hits between creation and onDestroy, your resource will not be destroyed. Use
--  autoDestroyCreate or metaResource instead.
--
--  This can be useful for parts of the resource cleanup that only become
--  relevant once the resource has been created and is in active use.
onDestroy :: Prog ClosedResourceContext () -> Prog OpenResourceContext ()
onDestroy prog = do
  ResourceContext{ destructorsVar } <- askResourceContext
  modifyIORef' destructorsVar (prog :)
{-# INLINE onDestroy #-}