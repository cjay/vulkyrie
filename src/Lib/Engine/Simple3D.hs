{-# LANGUAGE Strict #-}
module Lib.Engine.Simple3D
  ( BufferLoc(..)
  , DescrBindInfo(..)
  , frameSetId
  , materialSetId
  , objectSetId
  , Object(..)
  , recordObject
  , RenderContext(..)
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
import           Lib.Program
import           Lib.Program.Foreign


data BufferLoc = BufferLoc
  { buffer       :: VkBuffer
  , bufferOffset :: VkDeviceSize
  }


data DescrBindInfo = DescrBindInfo
  { descrSet       :: VkDescriptorSet
  , dynamicOffsets :: [Word32]
  }


-- Id of per-frame descriptor set
frameSetId :: Word32
frameSetId = 0

-- Id of per-material descriptor set
materialSetId :: Word32
materialSetId = 1

-- Id of per-object descriptor set
objectSetId :: Word32
objectSetId = 2


data Object = Object
  { modelMatrix      :: Mat44f
    -- ^ placement in world space
  , materialBindInfo :: DescrBindInfo
  , vertexBufferLoc  :: BufferLoc
  , indexBufferLoc   :: BufferLoc
  , firstIndex       :: Word32
  , indexCount       :: Word32
  -- not yet:
  -- , pipeline :: VkPipeline
  }


bindDescrSet :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> DescrBindInfo -> Program r ()
bindDescrSet cmdBuf pipelineLayout descrSetId DescrBindInfo{..} = locally $ do
  descrSetPtr <- newArrayRes [descrSet]
  let descrSetCnt = 1
  let dynOffCnt = fromIntegral $ length dynamicOffsets
  dynOffPtr <- newArrayRes dynamicOffsets
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr


viewProjMatrix :: VkExtent2D -> Program r Mat44f
viewProjMatrix extent = do
  let width = getField @"width" extent
  let height = getField @"height" extent
  let aspectRatio = fromIntegral width / fromIntegral height
  let view = lookAt (vec3 0 0 (-1)) (vec3 2 2 2) (vec3 0 0 0)
  let proj = perspective 0.1 20 (45/360*2*pi) aspectRatio
  return $ view %* proj


-- | Update push constants: transformation matrix
pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Program r ()
pushTransform cmdBuf pipelineLayout df = do
  liftIO $ thawPinDataFrame df >>= (flip withDataFramePtr $ \ptr ->
    vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 (castPtr ptr))

{-      not in use
-- | Update push constants: texture index
pushTexIndex :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> Program r ()
pushTexIndex cmdBuf pipelineLayout texIndex = alloca $ \ptr -> do
    poke ptr texIndex
    liftIO $ vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_FRAGMENT_BIT 64 4 (castPtr ptr)
-}

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

  pushTransform cmdBuf pipelineLayout transform

  bindDescrSet cmdBuf pipelineLayout materialSetId materialBindInfo

  liftIO $ vkCmdDrawIndexed cmdBuf
    indexCount 1 firstIndex 0 0 -- index count, instance count, first index, vertex offset, first instance


data RenderContext
  = RenderContext
  { pipeline       :: VkPipeline
  , renderPass     :: VkRenderPass
  , pipelineLayout :: VkPipelineLayout
  , extent         :: VkExtent2D
  }

recordAll :: RenderContext
          -> [Object]
          -> VkCommandBuffer
          -> VkFramebuffer
          -> Program r ()
recordAll
    RenderContext{..} objects cmdBuf framebuffer = do

  -- render pass
  let renderPassBeginInfo = createVk @VkRenderPassBeginInfo
        $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
        &* set @"pNext" VK_NULL
        &* set @"renderPass" renderPass
        &* set @"framebuffer" framebuffer
        &* setVk @"renderArea"
            (  setVk @"offset"
                ( set @"x" 0 &* set @"y" 0 )
            &* set @"extent" extent
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

  viewProjTransform <- viewProjMatrix extent
  -- objTransforms <- readIORef objTransformsRef

  forM_ objects $ \object ->
    recordObject pipelineLayout cmdBuf (modelMatrix object %* viewProjTransform) object

  -- finishing up
  liftIO $ vkCmdEndRenderPass cmdBuf
