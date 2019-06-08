{-# LANGUAGE Strict #-}
module Lib.Vulkan.Engine.Simple3D
  ( BufferLoc(..)
  , DescrBindInfo(..)
  , frameSetId
  , materialSetId
  , objectSetId
  , Object(..)
  , recordObject
  , recordAll
  ) where

import           Control.Monad
import           Foreign.Ptr                              (castPtr)
import           Numeric.DataFrame
import           Numeric.DataFrame.IO

import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Lib.MonadIO.IORef
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Command
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Sync


data BufferLoc = BufferLoc
  { buffer       :: VkBuffer
  , bufferOffset :: VkDeviceSize
  }


data DescrBindInfo = DescrBindInfo
  { descrSet      :: VkDescriptorSet
  , dynamicOffset :: Maybe Word32
  }


frameSetId :: Word32
frameSetId = 0

materialSetId :: Word32
materialSetId = 1

objectSetId :: Word32
objectSetId = 2


data Object = Object
  {
    -- modelMatrix      :: Mat44f
    -- objectBindInfo   :: [DescrBindInfo] -- one per frame in flight
    materialBindInfo :: DescrBindInfo
  , vertexBufferLoc  :: BufferLoc
  , indexBufferLoc   :: BufferLoc
  , firstIndex       :: Word32
  , indexCount       :: Word32
  -- hopefully never needed, especially not negative:
  -- , vertexOffset :: Int32
  -- not yet:
  -- , pipeline :: VkPipeline
  }


bindDescrSet :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> DescrBindInfo -> Program r ()
bindDescrSet cmdBuf pipelineLayout descrSetId DescrBindInfo{..} = locally $ do
  descrSetPtr <- newArrayRes [descrSet]
  let descrSetCnt = 1
  (dynOffCnt, dynOffPtr) <- case dynamicOffset of
    Just offset -> do
      offsetPtr <- newArrayRes [offset]
      return (1, offsetPtr)
    Nothing -> return (0, VK_NULL)
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr


-- | Update push constants: transformation matrix
pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Program r ()
pushTransform cmdBuf pipelineLayout df = do
  liftIO $ thawPinDataFrame df >>= (flip withDataFramePtr $ \ptr ->
    vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 (castPtr ptr))


recordObject :: VkPipelineLayout -> VkCommandBuffer -> Mat44f -> Object -> Program r ()
recordObject pipelineLayout cmdBuf transform Object{..} = do
  -- not yet:
  -- liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
  locally $ do
    let BufferLoc{..} = vertexBufferLoc
    vertexBufArr <- newArrayRes [buffer]
    vertexOffArr <- newArrayRes [bufferOffset]
    liftIO $ vkCmdBindVertexBuffers cmdBuf
      0 1 -- first binding, binding count
      vertexBufArr vertexOffArr

  locally $ do
    let BufferLoc{..} = indexBufferLoc
    liftIO $ vkCmdBindIndexBuffer cmdBuf buffer bufferOffset VK_INDEX_TYPE_UINT32

  pushTransform cmdBuf pipelineLayout $ transform

  bindDescrSet cmdBuf pipelineLayout materialSetId materialBindInfo

  liftIO $ vkCmdDrawIndexed cmdBuf
    indexCount 1 firstIndex 0 0 -- index count, instance count, first index, vertex offset, first instance


recordAll :: VkPipeline
          -> VkRenderPass
          -> VkPipelineLayout
          -> VkExtent2D
          -> [Object]
          -> IORef [Mat44f]
          -> VkCommandBuffer
          -> VkFramebuffer
          -> Mat44f
          -> Program r ()
recordAll
    pipeline rpass pipelineLayout swapExtent objects objTransformsRef
    cmdBuf framebuffer viewProjTransform = do

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

  -- locally $ do
  --   frameDsPtr <- newArrayRes [frameDescrSet]
  --   liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
  --     -- first set, set count, sets, dyn offset count, dyn offsets
  --     frameSetId 1 frameDsPtr 0 VK_NULL

  objTransforms <- readIORef objTransformsRef

  forM_ (zip objTransforms objects) $ \(objTransform, object) -> do
    recordObject pipelineLayout cmdBuf (viewProjTransform %* objTransform) object

  -- finishing up
  liftIO $ vkCmdEndRenderPass cmdBuf
