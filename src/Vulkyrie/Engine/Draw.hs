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
-- import           Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Vulkyrie.Concurrent
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
  -> forall r. Prog r QueueEvent


data RenderData
  = RenderData
  { swapchainVar              :: MVar VkSwapchainKHR
    -- ^ Swapchain in MVar to synchronize host access for vkAcquireNextImageKHR
  , swapOutdated              :: IORef Bool
    -- ^ Gets set to true if presentation faild with VK_ERROR_OUT_OF_DATE_KHR
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
  , frameFinishedThreadOwner  :: ThreadOwner
    -- ^ Owns the threads that wait for frame finish to release sems
  , renderFun                 :: RenderFun
    -- ^ render function of application, with wait semaphores and signal semaphores
  , framebuffers              :: [VkFramebuffer]
    -- ^ one per swapchain image
  }


drawFrame :: EngineCapability -> RenderData -> Prog r Bool
drawFrame EngineCapability{ dev, semPool, cmdQueue } RenderData{..} = do
    imageAvailSem <- head <$> acquireSemaphores semPool 1

    liftIO $ acquireToken swapImgTokens
    -- A present that failed probably doesn't unacquire the image, for that
    -- reason we HAVE to bail out here, as the vkAcquireNextImageKHR would
    -- probably be an API violation if too many images are acquired, and indeed
    -- the validation layers complain when that happens. "probably", because
    -- acquiredness is not very well defined in the Vulkan spec.
    -- See also:
    -- - https://github.com/KhronosGroup/Vulkan-ValidationLayers/issues/1696
    -- - https://github.com/KhronosGroup/Vulkan-Docs/issues/1242
    readIORef swapOutdated >>= flip when
      (do
        releaseSemaphores semPool [imageAvailSem]
        liftIO $ releaseToken swapImgTokens
        throwIO $ VulkanException VK_ERROR_OUT_OF_DATE_KHR
      )
    (imgIndex, status) <- withMVar swapchainVar
      (\swapchain ->
        -- Acquiring an image from the swapchain
        -- Can throw VK_ERROR_OUT_OF_DATE_KHR
        allocaPeekRet $ \imgIndexPtr -> runAndCatchVk
          ( vkAcquireNextImageKHR
                dev swapchain maxBound
                imageAvailSem VK_NULL_HANDLE imgIndexPtr
          )
          ( \(err :: VulkanException) -> do
              releaseSemaphores semPool [imageAvailSem]
              liftIO $ releaseToken swapImgTokens
              throwIO err
          )
      )

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
    submitPresent cmdQueue presentQueue presentAction $ \result -> do
      if result < VK_SUCCESS && result /= VK_ERROR_OUT_OF_DATE_KHR then
        throwIO $ VulkanException result
      else do
        -- The token release is kind of fake when out-of-date, but needed to
        -- avoid deadlock in the next drawFrame call. The swapOutdated update
        -- then tells that call what is going on.

        -- TODO: It would be better to throw an exception at the asyncRedo
        -- thread to abort things asap, but that needs a redesign of the
        -- swapchain handling, and has to wait until everything is resourcified
        -- to handle async exceptions gracefully.
        when (result == VK_ERROR_OUT_OF_DATE_KHR) $ atomicWriteIORef swapOutdated True
        liftIO $ releaseToken swapImgTokens

    void $ ownedThread frameFinishedThreadOwner $ do
      waitDone nextEvent
      writeChan renderFinishedSems renderFinishedSem
      liftIO $ Event.signal frameFinishedEvent

    -- TODO It would be nice to get a fresher swapchain status here, but
    -- vkGetSwapchainStatusKHR not found, bug in vulkan-api?
    -- result <- runVkResult $ vkGetSwapchainStatusKHR dev swapchain

    -- if it's VK_ERROR_OUT_OF_DATE_KHR instead, an exception will be thrown above
    return (status == VK_SUBOPTIMAL_KHR)
