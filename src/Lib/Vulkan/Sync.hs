{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Lib.Vulkan.Sync
  ( metaSemaphore
  , metaFence
  , FencePool
  , metaFencePool
  , recycleFencesAsync
  , allocFence
  , freeFence
  ) where

import           Control.Concurrent
import           Data.IORef
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Lib.MetaResource
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


initialFenceNum :: Int
initialFenceNum = 5

-- | Designed for one thread to get fences from the pool and many threads to
--   return them to the pool.
data FencePool = FencePool
  { usedFences  :: MVar [VkFence]
  , freshFences :: IORef [VkFence]
  , mFence      :: forall r. MetaResource r VkFence
  }

metaFencePool :: VkDevice -> MetaResource r FencePool
metaFencePool dev =
  let mFence = metaFence dev False
  in metaResource
  (\FencePool{..} -> do
      mfs <- liftIO $ tryTakeMVar usedFences
      let fs = maybe [] id mfs
      fs' <- liftIO $ readIORef freshFences
      sequence_ $ destroy mFence <$> (fs ++ fs')
  )
  (do
      usedFences <- liftIO $ newMVar []
      initialFences <- sequence $ replicate initialFenceNum (create mFence)
      freshFences <- liftIO $ newIORef initialFences
      return FencePool{..}
  )

-- | Moves used fences to the fresh fences list. Not thread-safe.
--
--   Wait for the returned MVar before calling allocFence or freeFence!
recycleFencesAsync :: FencePool -> IO (MVar ())
recycleFencesAsync FencePool{..} = do
  sync <- newEmptyMVar
  _ <- forkIO $ do
    fences <- takeMVar usedFences
    putMVar usedFences []
    modifyIORef freshFences (++ fences)
    putMVar sync ()
  return sync

-- | Acquire a fence from the fence pool. Not thread-safe.
allocFence :: FencePool -> Program r VkFence
allocFence FencePool{..} = do
  -- first try freshFences to avoid thread synchronization
  (liftIO $ readIORef freshFences) >>= \case
    f:rest -> liftIO $ writeIORef freshFences rest >> return f
    [] -> do
      recycled <- liftIO $ takeMVar usedFences
      liftIO $ putMVar usedFences []
      case recycled of
        f:rest -> liftIO $ writeIORef freshFences rest >> return f
        []     -> create mFence

-- | Give a fence back to the fence pool. Thread-safe.
freeFence :: FencePool -> VkFence -> IO ()
freeFence FencePool{..} fence = do
  ufs <- takeMVar usedFences
  putMVar usedFences (fence:ufs)

