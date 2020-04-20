{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Engine.Draw
  ( RenderFun
  , RenderData (..)
  , drawFrame
  ) where

import           Control.Concurrent.Event             (Event)
import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image
import           UnliftIO.Concurrent
import           UnliftIO.Exception

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Utils
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync

-- | Callback type for rendering into swapchain framebuffers.
--
--   Returned QueueEvent signals when the framebuffer has been written.
type RenderFun =
     VkFramebuffer
  -- ^ framebuffer to render to
  -> [(VkSemaphore, VkPipelineStageFlags)]
  -- ^ semaphores with stages to wait on
  -> [VkSemaphore]
  -- ^ semaphores to signal
  -> Program QueueEvent


data RenderData
  = RenderData
  { swapchainVar              :: MVar (Maybe VkSwapchainKHR)
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
  , renderFun                 :: RenderFun
    -- ^ render function of application, with wait semaphores and signal semaphores
  , framebuffers              :: [VkFramebuffer]
    -- ^ one per swapchain image
  }


drawFrame :: EngineCapability -> RenderData -> Program Bool
drawFrame EngineCapability{ dev, semPool, cmdQueue } RenderData{..} = do
    imageAvailSem <- head <$> acquireSemaphores semPool 1

    liftIO $ acquireToken swapImgTokens
    swapchain <- takeMVar swapchainVar >>= \case
      Just sc -> return sc
      Nothing -> error "unexpected Nothing in swapchain slot"
    -- Acquiring an image from the swapchain
    -- Can throw VK_ERROR_OUT_OF_DATE_KHR
    -- TODO: validation layer complaint about too many acquired images is
    -- probably not justified, waiting for vulkan spec to be clarified
    imgIndex <- allocaPeek $ \imgIndexPtr -> runAndCatchVk
      ( vkAcquireNextImageKHR
            dev swapchain maxBound
            imageAvailSem VK_NULL_HANDLE imgIndexPtr
      )
      ( \(err :: VulkanException) -> do
          releaseSemaphores semPool [imageAvailSem]
          putMVar swapchainVar (Just swapchain)
          liftIO $ releaseToken swapImgTokens
          throwIO err
      )
    putMVar swapchainVar (Just swapchain)

    nextSems_ <- takeMVar nextSems
    putMVar nextSems []

    let framebuffer = framebuffers !! fromIntegral imgIndex
    -- The renderFinishedSems channel also serves to limit the frames-in-flight to maxFramesInFlight
    renderFinishedSem <- readChan renderFinishedSems
    -- This intentionally blocks while recording the command buffer.
    -- There can still be multiple frames already submitted.
    nextEvent <- renderFun framebuffer
      ((imageAvailSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) : nextSems_)
      [renderFinishedSem]

    -- VK_ERROR_OUT_OF_DATE_KHR is supressed when presenting here, because ManagedQueue can't do anything with that
    let presentAction = present presentQueue [(swapchainVar, imgIndex)] [renderFinishedSem]
    submitPresent cmdQueue presentQueue presentAction (liftIO $ releaseToken swapImgTokens)

    void $ forkProg $ do
      waitDone nextEvent
      writeChan renderFinishedSems renderFinishedSem
      liftIO $ Event.signal frameFinishedEvent

    -- TODO vkGetSwapchainStatusKHR not found, bug in vulkan-api?
    -- result <- runVkResult $ vkGetSwapchainStatusKHR dev swapchain
    -- return (result == VK_SUBOPTIMAL_KHR)
    return False
