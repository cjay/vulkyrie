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

  , composeResource

    -- * Resource Monad
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
  , elementaryResource
  , inverseDestruction
  ) where

import           Control.Monad
import           Data.Either
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Typeable
import           UnliftIO.Exception
    ( SomeException, Exception(..), finally, mask, mask_, try, throwIO )
import           UnliftIO.IORef

import           Vulkyrie.Program
import           Vulkyrie.Utils (catchAsync, throwAsyncIO)
import GHC.Stack (HasCallStack, prettyCallStack, callStack)
import Data.Text (pack, Text)
import UnliftIO.Concurrent

class GenericResource res a where
  -- | Creates an action for use inside of region (see region below)
  auto :: res a -> Prog OpenResourceContext a
  -- | Creates a destructor action along with allocating the resource.
  --
  --   Has to be called within a masked scope. Pass the restore, see UnliftIO.Exception.mask.
  manual :: (forall b. Prog r b -> Prog r b) -> res a -> Prog r (Prog r' (), a)

auto_ :: GenericResource res a => res a -> Prog OpenResourceContext ()
auto_ = void . auto

instance GenericResource (Prog OpenResourceContext) a where
  auto = id
  {-# INLINE auto #-}

  manual restore res = do
    ctx <- makeResourceContext
    let ResourceContext{ destructorsVar } = ctx
    a <-
      restore (withResourceContext ctx res)
        `catchAsync` (\e -> readIORef destructorsVar >>= cleanup "manual" (Just e) >> throwAsyncIO e)
    return (readIORef destructorsVar >>= cleanup "manual" Nothing, a)
  {-# INLINE manual #-}

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

  manual restore MetaResource{..} = manual restore (autoDestroyCreate destroy create)
  {-# INLINE manual #-}


-- | A bit more polymorphic than (>>=) of Resource.
--
--   Note that the resulting Resource is a GenericResource as well and can be further chained using composeResource.
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 a -> (a -> res2 b) -> Prog OpenResourceContext b
composeResource ma fmb = auto ma >>= auto . fmb
{-# INLINE composeResource #-}





-- | Enforces explicitness at the use site.
--
-- Prog OpenResourceContext should never ever occur in top-level type signatures
-- (outside of this file) by convention. Resource can be converted back via
-- auto, same treatment on the use site as any other GenericResource.
newtype Resource a = Resource (Prog OpenResourceContext a)

instance GenericResource Resource a where
  auto (Resource res) = auto res
  {-# INLINE auto #-}
  manual restore (Resource res) = manual restore res
  {-# INLINE manual #-}

makeResourceContext :: Prog r ResourceContext
makeResourceContext = do
  destructorsVar <- newIORef []
  return ResourceContext{..}

-- | region establishes a scope for automatic resource destruction
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

data ResourceOwner =
  ResourceOwner
  { destructorsMVar :: MVar [Prog ClosedResourceContext ()]
  }

resourceOwner :: HasCallStack => Resource ResourceOwner
resourceOwner = Resource $ do
  let regionSrc = pack $ prettyCallStack callStack
  autoDestroyCreate
    (\ResourceOwner{ destructorsMVar } ->
      takeMVar destructorsMVar >>= cleanup regionSrc Nothing)
    (ResourceOwner <$> newMVar [])

-- | Non-local registering of resources accross threads.
--
-- Like a region, but after successful completion it registers the resource
-- destructors with the given ResourceOwner. This is thread-safe.
--
-- Warning: Only AFTER withResourceOwner finishes, the destructors get
-- registered there. Beware of ordering issues when having multiple threads use
-- withResourceOwner with the same owner to create resources that depend on each
-- other.
withResourceOwner :: HasCallStack => ResourceOwner -> Prog OpenResourceContext a -> Prog r a
withResourceOwner ResourceOwner{ destructorsMVar } res = do
  let regionSrc = pack $ prettyCallStack callStack
  ctx@ResourceContext{ destructorsVar } <- makeResourceContext
  mask $ \restore -> do
    a <- restore (withResourceContext ctx res)
      `catchAsync` (\e -> readIORef destructorsVar >>= cleanup regionSrc (Just e) >> throwAsyncIO e)
    destructors <- readIORef destructorsVar
    modifyMVar_ destructorsMVar (pure . (destructors <>) )
    return a

takeOwnership :: ResourceOwner -> Prog OpenResourceContext ()
takeOwnership ResourceOwner{ destructorsMVar = consumedDestructorsMVar } =
  mask_ $ do
    destructors <- takeMVar consumedDestructorsMVar
    putMVar consumedDestructorsMVar []
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (destructors ++)

resourceContext :: HasCallStack => Resource ResourceContext
resourceContext = Resource $ do
  let regionSrc = pack $ prettyCallStack callStack
  autoDestroyCreate
    (\ResourceContext{ destructorsVar } ->
      readIORef destructorsVar >>= cleanup regionSrc Nothing)
    makeResourceContext

takeContextOwnership :: ResourceContext -> Prog OpenResourceContext ()
takeContextOwnership ResourceContext{ destructorsVar = consumedDestructorsVar } =
  mask_ $ do
    destructors <- readIORef consumedDestructorsVar
    atomicWriteIORef consumedDestructorsVar []
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (destructors ++)



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

autoDestroyCreateWithUnmask :: (forall r. a -> Prog r ()) -- ^ free resource
                            -> (forall r. (Prog r b -> Prog r b) -> Prog r a) -- ^ allocate resource
                            -> Prog OpenResourceContext a
autoDestroyCreateWithUnmask free alloc = do
  ResourceContext{ destructorsVar } <- askResourceContext
  mask $ \unmask -> do
    a <- alloc unmask
    modifyIORef' destructorsVar (free a :)
    return a

-- TODO: this might behave unexpectedly with nested resources, as all destructor calls are reversed, even within nested ones
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

-- | Runs given program when destroying the resource. Destruction order is bottom to top.
--
--   Never use this to deallocate, that way you get no exception masking! Use elementaryResource!
onDestroy :: Prog ClosedResourceContext () -> Prog OpenResourceContext ()
onDestroy prog = do
  ResourceContext{ destructorsVar } <- askResourceContext
  modifyIORef' destructorsVar (prog :)
{-# INLINE onDestroy #-}

-- | Resource version of autoDestroyCreate
elementaryResource :: (forall r. a -> Prog r ()) -- ^ free resource
                   -> (forall r. Prog r a) -- ^ allocate resource
                   -> Resource a
elementaryResource free alloc = Resource $ autoDestroyCreate free alloc
