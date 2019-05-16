{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE Strict              #-}
module Lib.Vulkan.Drawing
  ( RenderData (..)
  , createFramebuffers
  , recordCommandBuffer
  , createFrameSemaphores
  , createFrameFences
  , drawFrame
  , maxFramesInFlight
  ) where

import           Control.Concurrent.Event                 (Event)
import qualified Control.Concurrent.Event                 as Event
import           Control.Concurrent.MVar
import           Control.Monad                            (forM_, when)
import           Data.IORef
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame

import           Lib.MetaResource
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Device
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Sync


maxFramesInFlight :: Int
maxFramesInFlight = 2


createFramebuffers :: VkDevice
                   -> VkRenderPass
                   -> SwapchainInfo
                   -> [VkImageView]
                   -> VkImageView
                   -> VkImageView
                   -> Program r [VkFramebuffer]
createFramebuffers dev renderPass SwapchainInfo{ swapExtent } swapImgViews depthImgView colorImgView =
    allocResource
      (liftIO . mapM_  (\fb -> vkDestroyFramebuffer dev fb VK_NULL) )
      (mapM createFB swapImgViews)
  where
    createFB swapImgView =
      let fbci = createVk @VkFramebufferCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" 0
            &* set @"renderPass" renderPass
            &* setListCountAndRef @"attachmentCount" @"pAttachments" [colorImgView, depthImgView, swapImgView]
            &* set @"width" (getField @"width" swapExtent)
            &* set @"height" (getField @"height" swapExtent)
            &* set @"layers" 1
      in allocaPeek $ \fbPtr -> withVkPtr fbci $ \fbciPtr ->
          runVk $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr


recordCommandBuffer :: VkPipeline
                    -> VkRenderPass
                    -> VkPipelineLayout
                    -> SwapchainInfo
                    -> VkBuffer -- vertex data
                    -> [(Word32, VkBuffer)] -- nr of indices and index data
                    -> VkCommandBuffer
                    -> VkFramebuffer
                    -> VkDescriptorSet
                    -> [VkDescriptorSet]
                    -> Program r ()
recordCommandBuffer
    pipeline rpass pipelineLayout SwapchainInfo{ swapExtent }
    vertexBuffer indexBuffers
    cmdBuffer framebuffer frameDescrSet materialDescrSets = do
  vertexBufArr <- newArrayRes [vertexBuffer]
  vertexOffArr <- newArrayRes [0]
  -- begin commands
  let cmdBufBeginInfo = createVk @VkCommandBufferBeginInfo
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT

  withVkPtr cmdBufBeginInfo
    $ runVk . vkBeginCommandBuffer cmdBuffer

  -- render pass
  let renderPassBeginInfo = createVk @VkRenderPassBeginInfo
        $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
        &* set @"pNext" VK_NULL
        &* set @"renderPass" rpass
        &* set @"framebuffer" framebuffer
        &* setVk @"renderArea"
            (  setVk @"offset"
                ( set @"x" 0 &* set @"y" 0 )
            &* set @"extent" swapExtent
            )
        -- TODO only the first command buffer should clear
        &* setListCountAndRef @"clearValueCount" @"pClearValues"
            [ ( createVk @VkClearValue
                $ setVk @"color"
                  $ setVec @"float32" (vec4 0 0 0.2 1)
              )
            , ( createVk @VkClearValue
                $ setVk @"depthStencil"
                  $  set @"depth" 1.0
                  &* set @"stencil" 0
              )
            ]

  withVkPtr renderPassBeginInfo $ \rpbiPtr ->
    liftIO $ vkCmdBeginRenderPass cmdBuffer rpbiPtr VK_SUBPASS_CONTENTS_INLINE

  -- basic drawing commands
  liftIO $ vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
  liftIO $ vkCmdBindVertexBuffers cmdBuffer 0 1 vertexBufArr vertexOffArr

  -- TODO memleak?
  frameDsPtr <- newArrayRes [frameDescrSet]
  liftIO $ vkCmdBindDescriptorSets cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 frameDsPtr 0 VK_NULL

  forM_ (zip materialDescrSets indexBuffers) $ \(descrSet, (nIndices, indexBuffer)) -> do
    liftIO $ vkCmdBindIndexBuffer cmdBuffer indexBuffer 0 VK_INDEX_TYPE_UINT32
    -- TODO memleak?
    dsPtr <- newArrayRes [descrSet]
    liftIO $ vkCmdBindDescriptorSets cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 1 1 dsPtr 0 VK_NULL
    liftIO $ vkCmdDrawIndexed cmdBuffer nIndices 1 0 0 0

  -- finishing up
  liftIO $ vkCmdEndRenderPass cmdBuffer
  runVk $ vkEndCommandBuffer cmdBuffer


createFrameSemaphores :: VkDevice -> Program r (Ptr VkSemaphore)
createFrameSemaphores dev = newArrayRes =<< (sequence $ replicate maxFramesInFlight (auto $ metaSemaphore dev))


createFrameFences :: VkDevice -> Program r (Ptr VkFence)
createFrameFences dev = newArrayRes =<< (sequence $ replicate maxFramesInFlight (auto $ metaFence dev True))


data RenderData
  = RenderData
  { dev                :: VkDevice
  , swapInfo           :: SwapchainInfo
  , queues             :: DevQueues
  , imgIndexPtr        :: Ptr Word32
  , frameIndexRef      :: IORef Int
  , renderFinishedSems :: Ptr VkSemaphore
    -- ^ one per frame-in-flight
  , imageAvailableSems :: Ptr VkSemaphore
    -- ^ one per frame-in-flight
  , inFlightFences     :: Ptr VkFence
    -- ^ one per frame-in-flight
  , frameFinishedEvent :: Event
    -- ^ signals completion of a frame to deallocators
  , frameOnQueueVars   :: [MVar ()]
    -- ^ one per frame-in-flight
  , memories           :: Ptr VkDeviceMemory
    -- ^ one per frame-in-flight
  , memoryMutator      :: forall r. VkDeviceMemory -> Program r ()
    -- ^ to execute on memories[*imgIndexPtr] before drawing
  -- , descrSetMutator    :: forall r. VkDescriptorSet -> Program r ()
    -- ^ update per-frame uniforms
  , dynCmdBuffers      :: [VkCommandBuffer]
    -- ^ one per frame-in-flight. recorded every frame.
  , recCmdBuffer       :: forall r. VkCommandBuffer -> VkFramebuffer -> VkDescriptorSet -> [VkDescriptorSet] -> Program r ()
    -- ^ update dynCmdBuffer
  , frameDescrSets     :: [VkDescriptorSet]
    -- ^ one per frame-in-flight
  , materialDescrSetsPerFrame :: [[VkDescriptorSet]]
    -- ^ one list of material descriptor sets per frame-in-flight
  , framebuffers       :: [VkFramebuffer]
    -- ^ one per swapchain image
  }


drawFrame :: RenderData -> Program r Bool
drawFrame RenderData {..} = do
    frameIndex <- liftIO $ readIORef frameIndexRef
    let inFlightFencePtr = inFlightFences `ptrAtIndex` frameIndex
    isOnQueue <- liftIO $
      maybe False (const True) <$> tryTakeMVar (frameOnQueueVars !! frameIndex)
    -- could be not on queue because of retry due to VK_ERROR_OUT_OF_DATE_KHR below
    when isOnQueue $ do
      runVk $ vkWaitForFences dev 1 inFlightFencePtr VK_TRUE (maxBound :: Word64)
      -- could also take current time here to measure frametimes
      liftIO $ Event.signal frameFinishedEvent

    let SwapchainInfo {..} = swapInfo
        DevQueues {..} = queues

    imageAvailable <- peek (imageAvailableSems `ptrAtIndex` frameIndex)
    renderFinished <- peek (renderFinishedSems `ptrAtIndex` frameIndex)
    inFlightFence <- peek inFlightFencePtr
    -- Acquiring an image from the swapchain
    -- Can throw VK_ERROR_OUT_OF_DATE_KHR
    runVk $ vkAcquireNextImageKHR
          dev swapchain maxBound
          imageAvailable VK_NULL_HANDLE imgIndexPtr
    imgIndex <- fromIntegral <$> peek imgIndexPtr
    let memoryPtr = memories `ptrAtIndex` frameIndex
    mem <- peek memoryPtr
    memoryMutator mem

    let dynCmdBuffer = dynCmdBuffers !! frameIndex
        frameDescrSet = frameDescrSets !! frameIndex
        materialDescrSets = materialDescrSetsPerFrame !! frameIndex
        framebuffer = framebuffers !! imgIndex
    -- descrSetMutator frameDescrSet
    recCmdBuffer dynCmdBuffer framebuffer frameDescrSet materialDescrSets
    dynCmdBufPtr <- newArrayRes [dynCmdBuffer]

    -- Submitting the command buffer
    let submitInfo = createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores"   [imageAvailable]
          &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          &* set @"commandBufferCount" 1
          &* set @"pCommandBuffers" dynCmdBufPtr
          &* set @"signalSemaphoreCount" 1
          &* setListRef @"pSignalSemaphores" [renderFinished]

    runVk $ vkResetFences dev 1 inFlightFencePtr
    withVkPtr submitInfo $ \siPtr ->
      runVk $ vkQueueSubmit graphicsQueue 1 siPtr inFlightFence
    liftIO $ putMVar (frameOnQueueVars !! frameIndex) ()

    -- Presentation
    let presentInfo = createVk @VkPresentInfoKHR
          $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
          &* set @"pNext" VK_NULL
          &* set @"pImageIndices" imgIndexPtr
          &* set        @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [renderFinished]
          &* set        @"swapchainCount" 1
          &* setListRef @"pSwapchains"    [swapchain]

    -- doing this before vkQueuePresentKHR because that might throw VK_ERROR_OUT_OF_DATE_KHR
    liftIO $ writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

    withVkPtr presentInfo $
      -- Can throw VK_ERROR_OUT_OF_DATE_KHR
      runVk . vkQueuePresentKHR presentQueue
    isSuboptimal <- (== VK_SUBOPTIMAL_KHR) . currentStatus <$> get

    return isSuboptimal
