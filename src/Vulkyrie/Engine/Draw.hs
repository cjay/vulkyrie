{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Engine.Draw
  ( RenderData (..)
  , drawFrame
  ) where

import           Control.Concurrent.Event             (Event)
import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image

import           Vulkyrie.MonadIO.Chan
import           Vulkyrie.MonadIO.MVar
import           Vulkyrie.MonadIO.Thread
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Utils
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync


data RenderData
  = RenderData
  { swapchainVar              :: MVar VkSwapchainKHR
    -- ^ Swapchain in MVar to synchronize host access for vkAcquireNextImageKHR
  , presentQueue              :: ManagedPresentQueue
    -- ^ Presentation queue. Not necessarily the same as the graphics queue(s).
  , swapImgTokens             :: NatTokenVar
    -- ^ Tokens for acquired swapchain images.
    -- max acquired images = len of swapchain - VkSurfaceCapabilitiesKHR::minImageCount + 1
  , renderFinishedSems        :: Chan VkSemaphore
    -- ^ Signals completion of graphics queue submission to the presentation
    --   engine. One per frame-in-flight. Additionally serves as frame tokens to
    --   respect maxFramesInFlight.
  , nextSems                  :: MVar [(VkSemaphore, VkPipelineStageFlags)]
    -- ^ Additional semaphores that the graphics queue submission needs to wait on.
  , frameFinishedEvent        :: Event
    -- ^ Gets signalled every time a frame has finished rendering.
  , recCmdBuffer              :: forall r. VkCommandBuffer -> VkFramebuffer -> Program r ()
    -- ^ record cmdBuf
  , framebuffers              :: [VkFramebuffer]
    -- ^ one per swapchain image
  }


drawFrame :: EngineCapability -> RenderData -> Program r Bool
drawFrame EngineCapability{ dev, semPool, cmdCap, cmdQueue } RenderData{..} = do
    imageAvailSem <- head <$> acquireSemaphores semPool 1

    liftIO $ acquireToken swapImgTokens
    swapchain <- takeMVar swapchainVar
    -- Acquiring an image from the swapchain
    -- Can throw VK_ERROR_OUT_OF_DATE_KHR
    imgIndex <- allocaPeek $ \imgIndexPtr -> runVk
      ( vkAcquireNextImageKHR
            dev swapchain maxBound
            imageAvailSem VK_NULL_HANDLE imgIndexPtr
      ) `catchError`
      ( \err -> do
          releaseSemaphores semPool [imageAvailSem]
          putMVar swapchainVar swapchain
          liftIO $ releaseToken swapImgTokens
          throwError err
      )
    putMVar swapchainVar swapchain

    nextSems_ <- takeMVar nextSems
    putMVar nextSems []

    let framebuffer = framebuffers !! fromIntegral imgIndex
    -- The renderFinishedSems channel also serves to limit the frames-in-flight to maxFramesInFlight
    renderFinishedSem <- readChan renderFinishedSems
    -- This intentionally blocks while recording the command buffer.
    -- There can still be multiple frames already submitted.
    nextEvent <- postWith cmdCap cmdQueue
      ((imageAvailSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) : nextSems_)
      [renderFinishedSem] $ \cmdBuf ->
        recCmdBuffer cmdBuf framebuffer

    -- VK_ERROR_OUT_OF_DATE_KHR is supressed when presenting here
    let presentAction = present presentQueue [(swapchainVar, imgIndex)] [renderFinishedSem]
    submitPresent cmdQueue presentQueue presentAction (liftIO $ releaseToken swapImgTokens)

    void $ forkProg $ do
      waitDone nextEvent
      writeChan renderFinishedSems renderFinishedSem
      liftIO $ Event.signal frameFinishedEvent

    -- TODO vkGetSwapchainStatusKHR not found, bug in vulkan-api?
    -- runVk $ vkGetSwapchainStatusKHR dev swapchain
    (== VK_SUBOPTIMAL_KHR) . currentStatus <$> get
