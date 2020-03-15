{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Engine.Draw
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

import           Vulkyrie.MonadIO.IORef
import           Vulkyrie.MonadIO.MVar
import           Vulkyrie.MonadIO.Thread
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync


data RenderData
  = RenderData
  { swapchain                 :: VkSwapchainKHR
  , presentQueue              :: VkQueue
    -- ^ Presentation queue. Not necessarily the same as the graphics queue(s).
  , imgIndexPtr               :: Ptr Word32
    -- ^ Swapchain image index. Written by vkAcquireNextImageKHR, read by vkQueuePresentKHR.
  , frameIndexRef             :: IORef Int
    -- ^ Between 0 and maxFramesInFlight. Increments and wraps.
    --   Index for frameOnQueueVars, queueEvents, and renderFinishedSems.
  , renderFinishedSems        :: [VkSemaphore]
    -- ^ Signals completion of graphics queue submission. One per frame-in-flight.
  , nextSems                  :: MVar [(VkSemaphore, VkPipelineStageFlags)]
    -- ^ Additional semaphores that the graphics queue submission needs to wait on.
  , frameFinishedEvent        :: Event
    -- ^ Gets signalled
  , queueEvents               :: [IORef QueueEvent]
    -- ^ Signals completion of a frame to deallocators. One per frame-in-flight, no reuse.
  , frameOnQueueVars          :: [MVar ()]
    -- ^ One per frame-in-flight

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
    -- ^ allowed number of unfinished submitted frames on the graphics queue
  }


drawFrame :: EngineCapability -> RenderData -> Program r Bool
drawFrame EngineCapability{ dev, semPool, cmdCap, cmdQueue } RenderData{..} = do
    frameIndex <- readIORef frameIndexRef
    isOnQueue <-
      isJust <$> tryTakeMVar (frameOnQueueVars !! frameIndex)
    -- could be not on queue because of retry due to VK_ERROR_OUT_OF_DATE_KHR below
    -- TODO "holes" in the queue probably result in having less frames in flight while resizing windows
    oldEvent <- readIORef (queueEvents !! frameIndex)
    when isOnQueue $ do
      wait oldEvent
      -- TODO It's suboptimal for frametime measurements that the wait and
      -- signal happens here instead of earlier. At least considering the
      -- submitNotify blocking call below, and postWith which seems to block
      -- while recording.
      liftIO $ Event.signal frameFinishedEvent
      -- could also take current time here to measure frametimes

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

    -- This intentionally blocks while recording the command buffer.
    -- There can still be multiple frames already submitted.
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
