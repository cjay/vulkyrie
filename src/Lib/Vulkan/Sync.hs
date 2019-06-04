{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Lib.Vulkan.Sync
  ( metaSemaphore
  , metaFence

  , FencePool
  , metaFencePool
  , resetFences
  , acquireFence
  , releaseFence

  , MasterSemaphorePool
  , metaMasterSemaphorePool
  , mspAcquireSemaphores
  , mspReleaseSemaphores

  , SemaphorePool
  , metaSemaphorePool
  , acquireSemaphores
  , releaseSemaphores
  , semaphoreRestockOpportunity
  ) where

import           Control.Monad
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq
import           GHC.Exts                       (fromList, toList)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import qualified Foreign.Marshal.Array          as Foreign
import           Lib.MetaResource
import           Lib.MonadIO.IORef
import           Lib.MonadIO.MVar
import           Lib.MonadIO.Thread
import           Lib.Program
import           Lib.Program.Foreign


metaSemaphore :: VkDevice -> MetaResource r VkSemaphore
metaSemaphore dev =
  metaResource
    (liftIO .  flip (vkDestroySemaphore dev) VK_NULL)
    $ allocaPeek $ \sPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
      ) $ \ciPtr -> runVk $ vkCreateSemaphore dev ciPtr VK_NULL sPtr


metaFence :: VkDevice -> Bool -> MetaResource r VkFence
metaFence dev signaled =
  metaResource
    (liftIO .  flip (vkDestroyFence dev) VK_NULL)
    $ allocaPeek $ \sPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" (if signaled then VK_FENCE_CREATE_SIGNALED_BIT else 0)
      ) $ \ciPtr -> runVk $ vkCreateFence dev ciPtr VK_NULL sPtr


-- arbitrary value
initialFenceNum :: Int
initialFenceNum = 5

-- | Designed for one thread to get fences from the pool and many threads to
--   return them to the pool.
data FencePool = FencePool
  { usedFences  :: MVar [VkFence]
  , freshFences :: IORef [VkFence]
  , mFence      :: forall r. MetaResource r VkFence
  , dev         :: VkDevice
  }

metaFencePool :: VkDevice -> MetaResource r FencePool
metaFencePool device =
  metaResource
  (\FencePool{..} -> do
      fs <- takeMVar usedFences
      touchIORef freshFences
      fs' <- readIORef freshFences
      sequence_ $ destroy mFence <$> (fs ++ fs')
  )
  (do
      let mFence = metaFence device False
      usedFences <- newMVar []
      initialFences <- sequence $ replicate initialFenceNum (create mFence)
      freshFences <- newIORef initialFences
      return FencePool{dev=device, ..}
  )

-- | Resets used fences and moves them to the fresh fences list. Not thread-safe.
--
--   Make sure that acquireFence can't be called at the same time.
resetFences :: FencePool -> Program r ()
resetFences FencePool{..} = do
  fences <- takeMVar usedFences
  putMVar usedFences []
  when (not $ null fences) $ do
    runVk $ Foreign.withArrayLen fences $ \len ptr ->
      vkResetFences dev (fromIntegral len) ptr
    modifyIORef' freshFences (++ fences)

-- | Acquire a fence from the fence pool. Not thread-safe.
acquireFence :: FencePool -> Program r VkFence
acquireFence FencePool{..} = do
  -- first try freshFences to avoid thread synchronization
  (readIORef freshFences) >>= \case
    f:rest -> writeIORef freshFences rest >> return f
    [] -> do
      reclaimed <- takeMVar usedFences
      runVk $ Foreign.withArrayLen reclaimed $ \len ptr ->
        vkResetFences dev (fromIntegral len) ptr
      putMVar usedFences []
      case reclaimed of
        f:rest -> writeIORef freshFences rest >> return f
        []     -> create mFence

-- | Release a fence back to the fence pool. Thread-safe.
--
--   It can only be acquired again after calling `resetFences`.
releaseFence :: FencePool -> VkFence -> Program r ()
releaseFence FencePool{..} fence = do
  ufs <- takeMVar usedFences
  putMVar usedFences (fence:ufs)



data MasterSemaphorePool = MasterSemaphorePool
  { mspSemaphores    :: MVar (Seq VkSemaphore)
  , mspMetaSemaphore :: forall r. MetaResource r VkSemaphore
  }

metaMasterSemaphorePool :: VkDevice -> MetaResource r MasterSemaphorePool
metaMasterSemaphorePool device =
  metaResource
  (\MasterSemaphorePool{..} -> do
      sems <- takeMVar mspSemaphores
      sequence_ $ destroy mspMetaSemaphore <$> sems
  )
  (do
      let mspMetaSemaphore = metaSemaphore device
      mspSemaphores <- newMVar mempty
      return MasterSemaphorePool{..}
  )

mspAcquireSemaphores :: MasterSemaphorePool -> Int -> Program r (Seq VkSemaphore)
mspAcquireSemaphores MasterSemaphorePool{..} num = do
  sems <- takeMVar mspSemaphores
  let (taken, rest) = Seq.splitAt num sems
  putMVar mspSemaphores rest
  let needed = num - length taken
  new <- Seq.replicateA needed (create mspMetaSemaphore)
  return $ taken <> new

mspReleaseSemaphores :: MasterSemaphorePool -> [VkSemaphore] -> Program r ()
mspReleaseSemaphores MasterSemaphorePool{..} sems = do
  have <- takeMVar mspSemaphores
  putMVar mspSemaphores $ have <> fromList sems



-- arbitrary value
initialSemaphoreNum :: Int
initialSemaphoreNum = 5

-- | Not thread-safe.
data SemaphorePool = SemaphorePool
  { semaphores          :: IORef (Seq VkSemaphore)
  , masterSemaphorePool :: MasterSemaphorePool
  , acquiredCount       :: IORef Int
  , maxAcquiredCount    :: IORef Int
  }

metaSemaphorePool :: MasterSemaphorePool -> MetaResource r SemaphorePool
metaSemaphorePool msp =
  metaResource
  (\SemaphorePool{..} -> do
      sems <- readIORef semaphores
      mspReleaseSemaphores msp $ toList sems
  )
  (do
      initialSemaphores <- mspAcquireSemaphores msp initialSemaphoreNum
      semaphores <- newIORef initialSemaphores
      acquiredCount <- newIORef 0
      maxAcquiredCount <- newIORef 0
      return SemaphorePool{masterSemaphorePool=msp, ..}
  )


-- | Gives explicit opportunity to restock from the MasterSemaphorePool
semaphoreRestockOpportunity :: SemaphorePool -> Program r ()
semaphoreRestockOpportunity SemaphorePool{..} = do
  count <- readIORef acquiredCount
  maxCount <- readIORef maxAcquiredCount
  let maxCount' = max maxCount count
  writeIORef maxAcquiredCount maxCount'
  writeIORef acquiredCount 0

  sems <- readIORef semaphores
  let needed = maxCount' - length sems
  newSems <- mspAcquireSemaphores masterSemaphorePool needed
  writeIORef semaphores $ sems <> newSems


-- | Semaphores are outomatically released to the MasterSemaphorePool by ManagedQueue
acquireSemaphores :: SemaphorePool -> Int -> Program r [VkSemaphore]
acquireSemaphores SemaphorePool{..} num = do
  count <- readIORef acquiredCount
  writeIORef acquiredCount (count + num)

  sems <- readIORef semaphores
  let (taken, rest) = Seq.splitAt num sems
      needed = num - length taken
      wanted = if needed > 0 then needed + initialSemaphoreNum else 0

  newSems <- if wanted > 0
             then mspAcquireSemaphores masterSemaphorePool wanted
             else return mempty
  let (completion, rest') = Seq.splitAt needed newSems

  writeIORef semaphores $ rest' <> rest
  return $ toList $ taken <> completion

releaseSemaphores :: SemaphorePool -> [VkSemaphore] -> Program r ()
releaseSemaphores SemaphorePool{ masterSemaphorePool } sems =
  mspReleaseSemaphores masterSemaphorePool sems