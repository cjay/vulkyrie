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
  , resource
  , alloc
  , free

  , BasicResource
  , basicResource

  , composeResource

    -- * Resource Monad
  , Resource
  , runResource
  , onDestroy
  , allocResource
  , locally
  , inverseDestruction
  , manually
  , cleanup
  ) where

import           Data.Either
import           Data.Typeable
import           Graphics.Vulkan.Core_1_0
import           UnliftIO.Concurrent
import UnliftIO.Exception
    ( SomeException, Exception, catch, finally, mask, throwIO, try )
import           UnliftIO.IORef

import           Data.List.NonEmpty (NonEmpty(..))
import           Vulkyrie.Program

class GenericResource res a where
  -- | Creates a Program.Resource that handles automatic deallocation.
  auto :: res a -> Resource a
  -- | Creates a destructor action along with allocating the resource.
  --
  --   Has to be called within a masked scope. Pass the restore. See Program.resourceMask.
  manual :: (forall b. Prog r b -> Prog r b) -> res a -> Prog r (Prog r' (), a)

instance GenericResource (Prog ResourceContext) a where
  auto = id
  {-# INLINE auto #-}
  manual = manually
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

-- | Creates a MetaResource. Drop in replacement for Vulkyrie.Program.allocResource.
metaResource :: (forall r. a -> Prog r ()) -- ^ destroy resource
             -> (forall r. Prog r a)        -- ^ create resource
             -> MetaResource a
metaResource destroy create = MetaResource {..}
{-# INLINE metaResource #-}

instance GenericResource MetaResource a where
  auto MetaResource{..} = allocResource destroy create
  {-# INLINE auto #-}

  manual restore MetaResource{..} = manually restore (allocResource destroy create)
  {-# INLINE manual #-}

-- | Need this to use MetaResources in the Resource monad
resource :: MetaResource a -> Resource a
resource ma = allocResource (destroy ma) (create ma)
{-# INLINE resource #-}


-- | Things that allow alloc and free synonyms to create and destroy.
--
--   This is purely for the right wording. The only instances are
--   VkCommandBuffer, VkDescriptorSet and VkDeviceMemory. These are the only
--   original Vulkan types that have a vkAlloc.. and vkFree.. function.
class AllocFree a where
  -- | Synonym for destroy
  free :: MetaResource a -> (a -> Prog r ())
  free = destroy
  {-# INLINE free #-}

  -- | Synonym for create
  alloc :: MetaResource a -> Prog r a
  alloc = create
  {-# INLINE alloc #-}

instance AllocFree VkCommandBuffer
instance AllocFree VkDescriptorSet
instance AllocFree VkDeviceMemory
instance (AllocFree a) => AllocFree [a]


-- TODO should change BasicResource to be able to access destroy directly
-- | Like MetaResource, but is meant to destroy any resources of the correct type.
--
--   Take care not to capture creation parameters in the destruction action.
type BasicResource = MetaResource

-- | Creates a BasicResource. Drop in replacement for Vulkyrie.Program.allocResource.
basicResource :: (forall r. a -> Prog r ()) -- ^ destroy resource
              -> (forall r. Prog r a)        -- ^ create resource
              -> BasicResource a
basicResource = metaResource


-- | A bit more polymorphic than (>>=) of Resource.
--
--   Note that the resulting Resource is a GenericResource as well and can be further chained using composeResource.
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 a -> (a -> res2 b) -> Resource b
composeResource ma fmb = auto ma >>= auto . fmb
{-# INLINE composeResource #-}





data ResourceContext =
  ResourceContext
  { destructorsVar :: IORef [Prog () ()]
  }

-- | Resources enable automatic deallocation when the scope ends. Scopes are
-- created with either locally or runResource.
--
-- Not exposing MonadUnliftIO to prevent forkIO and friends from messing with
-- the IORef (use threadRes instead). MonadUnliftIO can be accessed by unwrapping
-- the newtype.
-- newtype Resource a = Resource (ReaderT ResourceContext Program a)
--   deriving (Functor, Applicative, Monad, MonadLogger, MonadIO)
type Resource a = Prog ResourceContext a


makeResourceContext :: Prog r ResourceContext
makeResourceContext = do
  destructorsVar <- newIORef []
  return ResourceContext{..}


runResource :: Resource a -> Prog r a
runResource res = makeResourceContext >>= flip runRes res

-- | Consumes the context. Would need to write empty list to it for reuse.
runRes :: ResourceContext -> Resource a -> Prog r a
runRes ctx res = withResourceContext () $ do
  let ResourceContext{ destructorsVar } = ctx
  mask $ \restore -> do
    a <- restore (withResourceContext ctx res)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
    readIORef destructorsVar >>= cleanup Nothing
    return a

locally :: Resource a -> Resource a
locally = runResource

-- TODO: this might behave unexpectedly with nested resources, as all destructor calls are reversed, even within nested ones
inverseDestruction :: Resource a -> Resource a
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
  resourceMask $ \restore -> do
    ResourceContext{ destructorsVar } <- askResourceContext
    (destroyA, a) <- manually restore ra
    (destroyB, b) <- manually restore (rb a)
      `finally` modifyIORef' destructorsVar (destroyA <>)
    modifyIORef' destructorsVar ((destroyA <> destroyB) <>)
    return b
-}

-- | Has to be called within a masked scope. Pass the restore. Returns
-- destructor action along with result.
manually :: (forall b. Prog r b -> Prog r b) -> Resource a -> Prog r (Prog r' (), a)
manually restore res = do
  ctx <- makeResourceContext
  let ResourceContext{ destructorsVar } = ctx
  a <-
    restore (withResourceContext ctx res)
      `catch` (\e -> readIORef destructorsVar >>= withResourceContext () . cleanup (Just e) >> throwIO e)
  return (readIORef destructorsVar >>= withResourceContext () . cleanup Nothing, a)

-- | Runs given program when destroying the resource. Destruction order is bottom to top.
--
--   Never use this to deallocate, that way you get no exception masking! Use allocResource!
onDestroy :: Prog () () -> Resource ()
onDestroy prog = do
  ResourceContext{ destructorsVar } <- askResourceContext
  modifyIORef' destructorsVar (prog :)
{-# INLINE onDestroy #-}

-- TODO maybe masking during allocation should be default. bracket does it etc
-- | Exception-safe. Allows async exceptions during allocation, so it's fine if
-- the allocation is a complex Resource.
allocResource :: (forall r. a -> Prog r ()) -- ^ free resource
              -> (forall r. Prog r a) -- ^ allocate resource
              -> Resource a
allocResource free alloc =
  -- using unwrapped Resource to get MonadUnliftIO for mask
  mask $ \restore -> do
    a <- restore $ alloc
    ResourceContext{ destructorsVar } <- askResourceContext
    modifyIORef' destructorsVar (free a :)
    return a

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
threadRes prog = allocResource killThread (forkIO prog)