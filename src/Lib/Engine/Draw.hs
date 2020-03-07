{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Lib.Engine.Draw
  ( RenderData (..)
  , drawFrame
  ) where

import           Control.Concurrent.Event             (Event)
import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import           Data.Maybe
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Lib.MonadIO.IORef
import           Lib.MonadIO.MVar
import           Lib.MonadIO.Thread
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Command
import           Lib.Vulkan.Device
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Sync


data RenderData
  = RenderData
  { swapInfo                  :: SwapchainInfo
  , queues                    :: DevQueues
  , imgIndexPtr               :: Ptr Word32
  , frameIndexRef             :: IORef Int
  , renderFinishedSems        :: [VkSemaphore]
  , nextSems                  :: MVar [(VkSemaphore, VkPipelineStageFlags)]
  , frameFinishedEvent        :: Event
  , queueEvents               :: [IORef QueueEvent]
    -- ^ signals completion of a frame to deallocators
  , frameOnQueueVars          :: [MVar ()]
    -- ^ one per frame-in-flight

  -- , memories                  :: [VkDeviceMemory]
  --   -- ^ one per frame-in-flight
  -- , memoryMutator             :: forall r. VkDeviceMemory -> Program r ()
  --   -- ^ to execute on memories[*imgIndexPtr] before drawing
  -- , descrSetMutator        :: forall r. VkDescriptorSet -> Program r ()
  --   -- ^ update per-frame uniforms
  , recCmdBuffer              :: forall r. VkCommandBuffer -> VkFramebuffer -> Program r ()
    -- ^ record cmdBuf

  -- , frameDescrSets            :: [VkDescriptorSet]
  --   -- ^ one per frame-in-flight
  , framebuffers              :: [VkFramebuffer]
    -- ^ one per swapchain image
  , maxFramesInFlight         :: Int
  }


drawFrame :: EngineCapability -> RenderData -> Program r Bool
drawFrame EngineCapability{ dev, semPool, cmdCap, cmdQueue } RenderData{..} = do
    frameIndex <- readIORef frameIndexRef
    isOnQueue <-
      isJust <$> tryTakeMVar (frameOnQueueVars !! frameIndex)
    -- could be not on queue because of retry due to VK_ERROR_OUT_OF_DATE_KHR below
    oldEvent <- readIORef (queueEvents !! frameIndex)
    when isOnQueue $ do
      wait oldEvent
      liftIO $ Event.signal frameFinishedEvent
      -- could also take current time here to measure frametimes

    let SwapchainInfo { swapchain } = swapInfo
        DevQueues { presentQueue } = queues

    imageAvailSem <- head <$> acquireSemaphores semPool 1
    let renderFinishedSem = renderFinishedSems !! frameIndex
    -- Acquiring an image from the swapchain
    -- Can throw VK_ERROR_OUT_OF_DATE_KHR
    runVk ( vkAcquireNextImageKHR
            dev swapchain maxBound
            imageAvailSem VK_NULL_HANDLE imgIndexPtr
          ) `catchError`
      ( \err -> do
          releaseSemaphores semPool [imageAvailSem]
          throwError err
      )

    imgIndex <- fromIntegral <$> peek imgIndexPtr

    --memoryMutator (memories !! frameIndex)

    nextS <- takeMVar nextSems
    putMVar nextSems []

    nextEvent <- postWith cmdCap cmdQueue
      ((imageAvailSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) : nextS)
      [renderFinishedSem] $ \cmdBuf -> do
      -- let frameDescrSet = frameDescrSets !! frameIndex
      let framebuffer = framebuffers !! imgIndex
      -- descrSetMutator frameDescrSet
      recCmdBuffer cmdBuf framebuffer

    -- Complication because multiple queues are used:
    -- Using submitNotify instead of submit to block until vkQueueSubmit is
    -- done, because the renderFinishedSem semaphore needs to be signaled, or have
    -- an associated semaphore signal operation previously submitted for
    -- execution, before it is waited on via vkQueuePresentKHR below.
    -- TODO maybe manage both queues together to avoid blocking here
    _ <- submitNotify cmdQueue
    writeIORef (queueEvents !! frameIndex) nextEvent
    putMVar (frameOnQueueVars !! frameIndex) ()

    -- Presentation
    let presentInfo = createVk @VkPresentInfoKHR
          $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
          &* set @"pNext" VK_NULL
          &* set @"pImageIndices" imgIndexPtr
          &* set        @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [renderFinishedSem]
          &* set        @"swapchainCount" 1
          &* setListRef @"pSwapchains"    [swapchain]

    -- doing this before vkQueuePresentKHR because that might throw VK_ERROR_OUT_OF_DATE_KHR
    writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

    withVkPtr presentInfo $
      -- Can throw VK_ERROR_OUT_OF_DATE_KHR
      runVk . vkQueuePresentKHR presentQueue

    (== VK_SUBOPTIMAL_KHR) . currentStatus <$> get
