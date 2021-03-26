{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vulkyrie.Resource
  ( GenericResource
  , auto
  , manual

  , MetaResource
  , destroy
  , create
  , metaResource

  , composeResource

    -- * Resource Monad
  , Resource (..)
  , region
  , onDestroy
  , autoDestroyCreate
  , elementaryResource
  , inverseDestruction
  , cleanup
  ) where

import           Data.Either
import           Data.Typeable
import           UnliftIO.Concurrent
import UnliftIO.Exception
    ( SomeException, Exception, catch, finally, mask, mask_, throwIO, try )
import           UnliftIO.IORef

import           Data.List.NonEmpty (NonEmpty(..))
import           Vulkyrie.Program

class GenericResource res a where
  -- | Creates an action for use inside of region (see region below)
  auto :: res a -> Prog ResourceContext a
  -- | Creates a destructor action along with allocating the resource.
  --
  --   Has to be called within a masked scope. Pass the restore, see UnliftIO.Exception.mask.
  manual :: (forall b. Prog r b -> Prog r b) -> res a -> Prog r (Prog r' (), a)

instance GenericResource (Prog ResourceContext) a where
  auto = id
  {-# INLINE auto #-}

  manual restore res = do
    ctx <- makeResourceContext
    let ResourceContext{ destructorsVar } = ctx
    a <-
      restore (withResourceContext ctx res)
        `catch` (\e -> readIORef destructorsVar >>= withResourceContext () . cleanup (Just e) >> throwIO e)
    return (readIORef destructorsVar >>= withResourceContext () . cleanup Nothing, a)
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
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 a -> (a -> res2 b) -> Prog ResourceContext b
composeResource ma fmb = auto ma >>= auto . fmb
{-# INLINE composeResource #-}





data ResourceContext =
  ResourceContext
  { destructorsVar :: IORef [Prog () ()]
  }

-- | Enforces explicitness at the use site.
--
-- Prog ResourceContext should never ever occur in top-level type signatures
-- (outside of this file) by convention. Resource can be converted back via
-- auto, same treatment on the use site as any other GenericResource.
newtype Resource a = Resource (Prog ResourceContext a)

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
region :: Prog ResourceContext a -> Prog r a
region res = withResourceContext () $ do
  ctx@ResourceContext{ destructorsVar } <- makeResourceContext
  mask $ \restore -> do
    a <- restore (withResourceContext ctx res)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
    readIORef destructorsVar >>= cleanup Nothing
    return a

-- TODO: this might behave unexpectedly with nested resources, as all destructor calls are reversed, even within nested ones
inverseDestruction :: Prog ResourceContext a -> Prog ResourceContext a
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
onDestroy :: Prog () () -> Prog ResourceContext ()
onDestroy prog = do
  ResourceContext{ destructorsVar } <- askResourceContext
  modifyIORef' destructorsVar (prog :)
{-# INLINE onDestroy #-}

-- | Exception-safe allocation of an elementary resource.
--
-- Exceptions are also masked during allocation, like with bracket and friends.
-- Do not use this for composite resources.
autoDestroyCreate :: (forall r. a -> Prog r ()) -- ^ free resource
                  -> (forall r. Prog r a) -- ^ allocate resource
                  -> Prog ResourceContext a
autoDestroyCreate free alloc =
  mask_ $ do
    a <- alloc
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (free a :)
    return a

-- | Resource version of autoDestroyCreate
elementaryResource :: (forall r. a -> Prog r ()) -- ^ free resource
                   -> (forall r. Prog r a) -- ^ allocate resource
                   -> Resource a
elementaryResource free alloc = Resource $ autoDestroyCreate free alloc

data CleanupException = CleanupException
  { exceptionCausingCleanup :: Maybe SomeException
  , exceptionsDuringCleanup :: NonEmpty SomeException
  }
  deriving (Show, Typeable)
instance Exception CleanupException

cleanup :: Maybe SomeException -> [Prog r ()] -> Prog r ()
cleanup origException destructors =
  lefts <$> mapM try destructors >>= \case
    e : es -> throwIO (CleanupException origException (e :| es))
    [] -> return ()


-- TODO letting owning thread know that child thread exited?
-- TODO this can't clean up threads recursively
threadRes :: (forall r. Prog r ()) -> Resource ThreadId
threadRes prog = elementaryResource killThread (forkIO prog)