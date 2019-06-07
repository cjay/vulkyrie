{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Strict           #-}
module Lib.Vulkan.Drawing
  ( RenderData (..)
  , createFramebuffers
  , recordCommandBuffer
  , createFrameSemaphores
  , drawFrame
  , maxFramesInFlight
  ) where

import           Control.Concurrent.Event                 (Event)
import qualified Control.Concurrent.Event                 as Event
import           Control.Monad                            (forM_, when)
import           Foreign.Ptr                              (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame
import           Numeric.DataFrame.IO

import           Lib.MetaResource
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
                    -> [VkDescriptorSet]
                    -> Mat44f
                    -> Program r ()
recordCommandBuffer
    pipeline rpass pipelineLayout SwapchainInfo{ swapExtent }
    vertexBuffer indexBuffers
    cmdBuf framebuffer materialDescrSets trans = do

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
    liftIO $ vkCmdBeginRenderPass cmdBuf rpbiPtr VK_SUBPASS_CONTENTS_INLINE

  -- basic drawing commands
  liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
  locally $ do
    vertexBufArr <- newArrayRes [vertexBuffer]
    vertexOffArr <- newArrayRes [0]
    liftIO $ vkCmdBindVertexBuffers cmdBuf
      0 1 -- first binding, binding count
      vertexBufArr vertexOffArr

  -- locally $ do
  --   frameDsPtr <- newArrayRes [frameDescrSet]
  --   liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
  --     -- first set, set count, sets, dyn offset count, dyn offsets
  --     0 1 frameDsPtr 0 VK_NULL

  let transList = [trans, trans %* (translate3 $ vec3 1 1 1)]
  forM_ (zip3 materialDescrSets indexBuffers transList) $ \(descrSet, (nIndices, indexBuffer), mat) -> do
    pushMatrix cmdBuf pipelineLayout mat
    liftIO $ vkCmdBindIndexBuffer cmdBuf indexBuffer 0 VK_INDEX_TYPE_UINT32 -- offset 0 bytes
    locally $ do
      dsPtr <- newArrayRes [descrSet]
      liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
        -- first set, set count, sets, dyn offset count, dyn offsets
        1 1 dsPtr 0 VK_NULL
    liftIO $ vkCmdDrawIndexed cmdBuf
      nIndices 1 0 0 0 -- index count, instance count, first index, vertex offset, first instance

  -- finishing up
  liftIO $ vkCmdEndRenderPass cmdBuf


pushMatrix :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Program r ()
pushMatrix cmdBuf pipelineLayout df = do
  liftIO $ thawPinDataFrame df >>= (flip withDataFramePtr $ \ptr ->
    vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 (castPtr ptr))


createFrameSemaphores :: VkDevice -> Program r [VkSemaphore]
createFrameSemaphores dev = sequence $ replicate maxFramesInFlight (auto $ metaSemaphore dev)


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
    -- ^ one per frame-in-flight
  -- , memoryMutator             :: forall r. VkDeviceMemory -> Program r ()
    -- ^ to execute on memories[*imgIndexPtr] before drawing
  -- , descrSetMutator        :: forall r. VkDescriptorSet -> Program r ()
    -- ^ update per-frame uniforms
  , recCmdBuffer              :: forall r. VkCommandBuffer -> VkFramebuffer -> [VkDescriptorSet] -> Mat44f -> Program r ()
    -- ^ update cmdBuf
  , getMvpMatrix              :: forall r. Program r Mat44f
  -- , frameDescrSets            :: [VkDescriptorSet]
    -- ^ one per frame-in-flight
  , materialDescrSetsPerFrame :: [[VkDescriptorSet]]
    -- ^ one list of material descriptor sets per frame-in-flight
  , framebuffers              :: [VkFramebuffer]
    -- ^ one per swapchain image
  }


drawFrame :: EngineCapability -> RenderData -> Program r Bool
drawFrame EngineCapability{..} RenderData{..} = do
    frameIndex <- readIORef frameIndexRef
    isOnQueue <-
      maybe False (const True) <$> tryTakeMVar (frameOnQueueVars !! frameIndex)
    -- could be not on queue because of retry due to VK_ERROR_OUT_OF_DATE_KHR below
    oldEvent <- readIORef (queueEvents !! frameIndex)
    when isOnQueue $ do
      waitForQueue oldEvent
      liftIO $ Event.signal frameFinishedEvent
      -- could also take current time here to measure frametimes

    let SwapchainInfo {..} = swapInfo
        DevQueues {..} = queues

    imageAvailable <- head <$> acquireSemaphores semPool 1
    let renderFinished = (renderFinishedSems !! frameIndex)
    -- Acquiring an image from the swapchain
    -- Can throw VK_ERROR_OUT_OF_DATE_KHR
    (runVk $ vkAcquireNextImageKHR
          dev swapchain maxBound
          imageAvailable VK_NULL_HANDLE imgIndexPtr) `catchError`
      ( \err -> do
          releaseSemaphores semPool [imageAvailable]
          throwError err
      )

    imgIndex <- fromIntegral <$> peek imgIndexPtr

    --memoryMutator (memories !! frameIndex)

    nextS <- takeMVar nextSems
    putMVar nextSems []

    trans <- getMvpMatrix
    nextEvent <- postWith cmdCap cmdQueue
      ((imageAvailable, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) : nextS)
      [renderFinished] $ \cmdBuf -> do
      -- let frameDescrSet = frameDescrSets !! frameIndex
      let materialDescrSets = materialDescrSetsPerFrame !! frameIndex
          framebuffer = framebuffers !! imgIndex
      -- descrSetMutator frameDescrSet
      recCmdBuffer cmdBuf framebuffer materialDescrSets trans

    -- Complication because multiple queues are used:
    -- Using submitNotify instead of submit to block until vkQueueSubmit is
    -- done, because the renderFinished semaphore needs to be signaled, or have
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
          &* setListRef @"pWaitSemaphores" [renderFinished]
          &* set        @"swapchainCount" 1
          &* setListRef @"pSwapchains"    [swapchain]

    -- doing this before vkQueuePresentKHR because that might throw VK_ERROR_OUT_OF_DATE_KHR
    writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

    withVkPtr presentInfo $
      -- Can throw VK_ERROR_OUT_OF_DATE_KHR
      runVk . vkQueuePresentKHR presentQueue
    isSuboptimal <- (== VK_SUBOPTIMAL_KHR) . currentStatus <$> get

    return isSuboptimal
