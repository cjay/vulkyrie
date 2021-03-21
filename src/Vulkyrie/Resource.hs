{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vulkyrie.Resource
  ( GenericResource
  , auto
  , manual
  , Resource

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
  , askRegion
  , runResource
  , onDestroy
  , liftProg
  , allocResource
  , locally
  , resourceMask
  , inverseDestruction
  , manually
  , cleanup
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack hiding (logDebug, logInfo, logWarn, logError)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Either
import           Data.Typeable
import           Graphics.Vulkan.Core_1_0
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Data.List.NonEmpty (NonEmpty(..))
import           Vulkyrie.Program

class GenericResource res a where
  -- | Creates a Program.Resource that handles automatic deallocation.
  auto :: res a -> Resource a
  -- | Creates a destructor action along with allocating the resource.
  --
  --   Has to be called within a masked scope. Pass the restore. See Program.resourceMask.
  manual :: (forall b. Program b -> Program b) -> res a -> Program (Program (), a)

instance GenericResource Resource a where
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
  { destroy :: a -> Program ()
  , create :: Program a
  }

-- | Creates a MetaResource. Drop in replacement for Vulkyrie.Program.allocResource.
metaResource :: (a -> Program ()) -- ^ destroy resource
             -> Program a        -- ^ create resource
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
  free :: MetaResource a -> (a -> Program ())
  free = destroy
  {-# INLINE free #-}

  -- | Synonym for create
  alloc :: MetaResource a -> Program a
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
basicResource :: (a -> Program ()) -- ^ destroy resource
              -> Program a        -- ^ create resource
              -> BasicResource a
basicResource = metaResource


-- | A bit more polymorphic than (>>=) of Resource.
--
--   Note that the resulting Resource is a GenericResource as well and can be further chained using composeResource.
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 a -> (a -> res2 b) -> Resource b
composeResource ma fmb = auto ma >>= auto . fmb
{-# INLINE composeResource #-}





data ResourceContext
  = ResourceContext
  { destructorsVar :: IORef [Program ()]
  }

-- | Resources enable automatic deallocation when the scope ends. Scopes are
-- created with either locally or runResource.
--
-- Not exposing MonadUnliftIO to prevent forkIO and friends from messing with
-- the IORef (use threadRes instead). MonadUnliftIO can be accessed by unwrapping
-- the newtype.
newtype Resource a = Resource (ReaderT ResourceContext Program a)
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO)

makeResourceContext :: Program ResourceContext
makeResourceContext = do
  destructorsVar <- newIORef []
  return ResourceContext{..}

askResourceContext :: Resource ResourceContext
askResourceContext = Resource ask

inContext :: ResourceContext -> Resource a -> Resource a
inContext ctx (Resource res) = liftProg $ runReaderT res ctx

askRegion :: Resource (Resource a -> Resource a)
askRegion = inContext <$> askResourceContext


runResource :: Resource a -> Program a
runResource res = makeResourceContext >>= flip runRes res

-- | Consumes the context. Would need to write empty list to it for reuse.
runRes :: ResourceContext -> Resource a -> Program a
runRes ctx (Resource res) = do
  let ResourceContext{ destructorsVar } = ctx
  mask $ \restore -> do
    a <- restore (runReaderT res ctx)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
    readIORef destructorsVar >>= cleanup Nothing
    return a

locally :: Resource a -> Resource a
locally = liftProg . runResource

resourceMask :: ((forall a. Program a -> Program a) -> Resource b) -> Resource b
resourceMask act = Resource $ do
  ctx <- ask
  lift $ mask $ \restore -> do
    let Resource res = act restore
    runReaderT res ctx

-- TODO: this might behave unexpectedly with nested resources, as all destructor calls are reversed, even within nested ones
inverseDestruction :: Resource a -> Resource a
inverseDestruction (Resource res) = do
  ResourceContext outerDestrVar <- askResourceContext
  liftProg $ mask $ \restore -> do
    ctx <- makeResourceContext
    let ResourceContext{ destructorsVar } = ctx
    restore (runReaderT res ctx)
      `finally` do
        destructors <- readIORef destructorsVar
        modifyIORef' outerDestrVar (reverse destructors <>)

{-
asym :: Resource a -> (a -> Resource b) -> Resource b
asym ra rb =
  resourceMask $ \restore -> do
    ResourceContext{ destructorsVar } <- askResourceContext
    (destroyA, a) <- liftProg $ manually restore ra
    (destroyB, b) <- liftProg $ manually restore (rb a)
      `finally` modifyIORef' destructorsVar (destroyA <>)
    modifyIORef' destructorsVar ((destroyA <> destroyB) <>)
    return b
-}

-- | Has to be called within a masked scope. Pass the restore. Returns
-- destructor action along with result.
manually :: (forall a. Program a -> Program a) -> Resource b -> Program (Program (), b)
manually restore (Resource res) = do
  ctx <- makeResourceContext
  let ResourceContext{ destructorsVar } = ctx
  a <-
    restore (runReaderT res ctx)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
  return (readIORef destructorsVar >>= cleanup Nothing, a)

-- | Runs given program when destroying the resource. Destruction order is bottom to top.
--
--   Never use this to deallocate, that way you get no exception masking! Use allocResource!
onDestroy :: Program () -> Resource ()
onDestroy prog = do
  ResourceContext{ destructorsVar } <- askResourceContext
  modifyIORef' destructorsVar (prog :)
{-# INLINE onDestroy #-}

liftProg :: Program a -> Resource a
liftProg prog = Resource $ lift prog

-- TODO maybe masking during allocation should be default. bracket does it etc
-- | Exception-safe. Allows async exceptions during allocation, so it's fine if
-- the allocation is a complex Resource.
allocResource :: (a -> Program ()) -- ^ free resource
              -> Program a -- ^ allocate resource
              -> Resource a
allocResource free alloc =
  -- using unwrapped Resource to get MonadUnliftIO for mask
  Resource $ mask $ \restore -> do
    a <- restore $ lift alloc
    ResourceContext{ destructorsVar } <- ask
    modifyIORef' destructorsVar (free a :)
    return a

data CleanupException = CleanupException
  { exceptionCausingCleanup :: Maybe SomeException
  , exceptionsDuringCleanup :: NonEmpty SomeException
  }
  deriving (Show, Typeable)
instance Exception CleanupException

cleanup :: Maybe SomeException -> [Program ()] -> Program ()
cleanup origException destructors =
  lefts <$> mapM try destructors >>= \case
    e : es -> throwIO (CleanupException origException (e :| es))
    [] -> return ()


-- TODO letting owning thread know that child thread exited?
-- TODO this can't clean up threads recursively
threadRes :: Program () -> Resource ThreadId
threadRes prog = allocResource killThread (forkIO prog)