{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Vulkyrie.Engine.Pipeline.Sprite where

import           Numeric.DataFrame
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.PushConstant
import Vulkyrie.Vulkan.Engine
import Vulkyrie.Engine.Pipeline
import Vulkyrie.Vulkan.Shader (shaderFile)
import Vulkyrie.Vulkan.Descriptor
import Vulkyrie.Vulkan.PipelineLayout
import Vulkyrie.Vulkan.Default.Pipeline (createGraphicsPipeline)
import Data.Tagged (Tagged(Tagged))

-- | Handle for type-directed selection of pipelines
data Pipeline mode = Pipeline (Mode mode)
type AlphaBlendPipeline = Pipeline AlphaBlend
type AlphaDiscardPipeline = Pipeline AlphaDiscard
type OpaquePipeline = Pipeline Opaque

data Mode mode where
   AlphaBlend :: Mode AlphaBlend
   AlphaDiscard :: Mode AlphaDiscard
   Opaque :: Mode Opaque

-- | Alpha blend on, depth write off
--
-- Requires everything behind it to be drawn already.
data AlphaBlend

-- | Like Opaque, but discards fragments where alpha < 1.0 to prevent depth writes there
--
-- Having discard in a shader prevents certain optimizations in hardware. The
-- cost of this has to be weighted against the cost of sorting the draw calls by
-- depth.
data AlphaDiscard

-- | Alpha blend off, depth write on
--
-- Can be drawn in any order relative to anything else.
data Opaque

type Fields =
  [ "transform" ::: Mat44f,
    "pos" ::: Vec3f,
    "turns" ::: Scalar Float,
    "center" ::: Vec2f,
    "size" ::: Vec2f,
    "uvPos" ::: Vec2f,
    "uvSize" ::: Vec2f
  ]

pushTransform :: Mat44f -> PlCmd (Pipeline m) r ()
pushTransform = pushField @Fields @"transform" VK_SHADER_STAGE_VERTEX_BIT

pushPos :: Vec3f -> PlCmd (Pipeline m) r ()
pushPos = pushField @Fields @"pos" VK_SHADER_STAGE_VERTEX_BIT

pushSize :: Vec2f -> PlCmd (Pipeline m) r ()
pushSize = pushField @Fields @"size" VK_SHADER_STAGE_VERTEX_BIT

pushCenter :: Vec2f -> PlCmd (Pipeline m) r ()
pushCenter = pushField @Fields @"center" VK_SHADER_STAGE_VERTEX_BIT

pushTurns :: Scalar Float -> PlCmd (Pipeline m) r ()
pushTurns = pushField @Fields @"turns" VK_SHADER_STAGE_VERTEX_BIT

pushUVPos :: Vec2f -> PlCmd (Pipeline m) r ()
pushUVPos = pushField @Fields @"uvPos" VK_SHADER_STAGE_VERTEX_BIT

pushUVSize :: Vec2f -> PlCmd (Pipeline m) r ()
pushUVSize = pushField @Fields @"uvSize" VK_SHADER_STAGE_VERTEX_BIT

fragPath :: Mode mode -> FilePath
fragPath AlphaDiscard = "shaders/single_sampler_discard_alpha.frag.spv"
fragPath _ = "shaders/single_sampler.frag.spv"

loadShaders :: Mode mode -> EngineCapability -> Resource [VkPipelineShaderStageCreateInfo]
loadShaders mode EngineCapability{ dev } = Resource $ do
  shaderVert <- auto $ shaderFile dev VK_SHADER_STAGE_VERTEX_BIT "shaders/sprites.vert.spv"
  shaderFrag <- auto $ shaderFile dev VK_SHADER_STAGE_FRAGMENT_BIT (fragPath mode)
  return [shaderVert, shaderFrag]

makePipelineLayout :: VkDevice -> Resource (VkDescriptorSetLayout, VkPipelineLayout)
makePipelineLayout dev = Resource $ do
  frameDSL <- auto $ createDescriptorSetLayout dev [uniformBinding 0]
  -- TODO automate bind ids
  materialDSL <- auto $ createDescriptorSetLayout dev [samplerBinding 0]
  let (offset, len) = wholeRange @Fields
  pipelineLayout <- auto $ createPipelineLayout dev
    -- descriptor set numbers 0,1,..
    [frameDSL, materialDSL]
    -- push constant ranges
    [ pushConstantRange VK_SHADER_STAGE_VERTEX_BIT offset len
    ]

  -- (transObjMems, transObjBufs) <- unzip <$> uboCreateBuffers pdev dev transObjSize maxFramesInFlight
  -- descriptorBufferInfos <- mapM (uboBufferInfo transObjSize) transObjBufs

  -- frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL

  -- forM_ (zip descriptorBufferInfos frameDescrSets) $
    -- \(bufInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [bufInfo] []

  return (materialDSL, pipelineLayout)

enableAlpha :: Mode mode -> Bool
enableAlpha AlphaBlend = True
enableAlpha _ = False

loadPipeline :: Mode mode -> EngineCapability -> Resource (Tagged (Pipeline mode) ProtoPipeline, VkDescriptorSetLayout)
loadPipeline mode cap@EngineCapability{ dev } = Resource $ do
  shaderStages <- auto $ loadShaders mode cap
  (materialDSL, pipelineLayout) <- auto $ makePipelineLayout dev
  let createPipeline renderPass swapExtent msaaSamples =
        createGraphicsPipeline dev swapExtent
          [] []
          shaderStages
          renderPass
          pipelineLayout
          msaaSamples
          (enableAlpha mode)
  return (Tagged ProtoPipeline{ pipelineLayout, createPipeline }, materialDSL)

draw :: PlCmd (Pipeline m) r ()
draw = plCmd $ \_ cmdBuf ->
  liftIO $ vkCmdDraw cmdBuf
    6 1 0 0 -- vertex count, instance count, first vertex, first instance