{-# LANGUAGE Strict #-}
module Vulkyrie.Engine.Simple2D
  ( DescrBindInfo(..)
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
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Default.RenderPass


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
  }


bindDescrSet :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> DescrBindInfo -> Prog r ()
bindDescrSet cmdBuf pipelineLayout descrSetId DescrBindInfo{..} = region $ do
  descrSetPtr <- auto $ newArrayRes [descrSet]
  let descrSetCnt = 1
  let dynOffCnt = fromIntegral $ length dynamicOffsets
  dynOffPtr <- auto $ newArrayRes dynamicOffsets
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr


-- | Update push constants: transformation matrix
pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Prog r ()
pushTransform cmdBuf pipelineLayout df =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_VERTEX_BIT 0 64 . castPtr)

{-      not in use
-- | Update push constants: texture index
pushTexIndex :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> Program ()
pushTexIndex cmdBuf pipelineLayout texIndex = alloca $ \ptr -> do
    poke ptr texIndex
    liftIO $ vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_FRAGMENT_BIT 64 4 (castPtr ptr)
-}

recordObject :: VkPipelineLayout -> VkCommandBuffer -> Mat44f -> Object -> Prog r ()
recordObject pipelineLayout cmdBuf transform Object{..} = do
  -- not yet:
  -- liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline

  pushTransform cmdBuf pipelineLayout transform

  bindDescrSet cmdBuf pipelineLayout materialSetId materialBindInfo

  liftIO $ vkCmdDraw cmdBuf
    6 1 0 0 -- vertex count, instance count, first vertex, first instance


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
          -> Prog r ()
recordAll
    RenderContext{..} viewProjTransform objects cmdBuf framebuffer = do

  -- render pass
  let renderPassBeginInfo = createRenderPassBeginInfo renderPass framebuffer extent
  withVkPtr renderPassBeginInfo $ \rpbiPtr ->
    liftIO $ vkCmdBeginRenderPass cmdBuf rpbiPtr VK_SUBPASS_CONTENTS_INLINE

  -- basic drawing commands
  liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline

  -- region $ do
  --   frameDsPtr <- newArrayRes [frameDescrSet]
  --   liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
  --     -- first set, set count, sets, dyn offset count, dyn offsets
  --     frameSetId 1 frameDsPtr 0 VK_NULL

  -- objTransforms <- readIORef objTransformsRef

  forM_ objects $ \object ->
    recordObject pipelineLayout cmdBuf (modelMatrix object %* viewProjTransform) object

  -- finishing up
  liftIO $ vkCmdEndRenderPass cmdBuf
