{-# LANGUAGE Strict #-}
module Vulkyrie.Engine.Simple3D
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
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Vulkan.Default.RenderPass


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


bindDescrSet :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> DescrBindInfo -> Program ()
bindDescrSet cmdBuf pipelineLayout descrSetId DescrBindInfo{..} = locally $ do
  descrSetPtr <- newArrayRes [descrSet]
  let descrSetCnt = 1
  let dynOffCnt = fromIntegral $ length dynamicOffsets
  dynOffPtr <- newArrayRes dynamicOffsets
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr


-- | Update push constants: transformation matrix
pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Program ()
pushTransform cmdBuf pipelineLayout df = do
  liftIO $ thawPinDataFrame df >>= (flip withDataFramePtr $ \ptr ->
    vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 (castPtr ptr))

{-      not in use
-- | Update push constants: texture index
pushTexIndex :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> Program ()
pushTexIndex cmdBuf pipelineLayout texIndex = alloca $ \ptr -> do
    poke ptr texIndex
    liftIO $ vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_FRAGMENT_BIT 64 4 (castPtr ptr)
-}

recordObject :: VkPipelineLayout -> VkCommandBuffer -> Mat44f -> Object -> Program ()
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
          -> Mat44f
          -> [Object]
          -> VkCommandBuffer
          -> VkFramebuffer
          -> Program ()
recordAll
    RenderContext{..} viewProjTransform objects cmdBuf framebuffer = do

  let renderPassBeginInfo = createRenderPassBeginInfo renderPass framebuffer extent
  withVkPtr renderPassBeginInfo $ \rpbiPtr ->
    liftIO $ vkCmdBeginRenderPass cmdBuf rpbiPtr VK_SUBPASS_CONTENTS_INLINE

  -- basic drawing commands
  liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline

  -- locally $ do
  --   frameDsPtr <- newArrayRes [frameDescrSet]
  --   liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
  --     -- first set, set count, sets, dyn offset count, dyn offsets
  --     frameSetId 1 frameDsPtr 0 VK_NULL

  -- objTransforms <- readIORef objTransformsRef

  forM_ objects $ \object ->
    recordObject pipelineLayout cmdBuf (modelMatrix object %* viewProjTransform) object

  -- finishing up
  liftIO $ vkCmdEndRenderPass cmdBuf
