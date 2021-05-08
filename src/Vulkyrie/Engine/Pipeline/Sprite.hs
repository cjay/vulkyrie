{-# LANGUAGE TypeOperators #-}
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
data Pipeline

type Fields =
  [ "transform" ::: Mat44f,
    "pos" ::: Vec2f,
    "size" ::: Vec2f,
    "uvPos" ::: Vec2f,
    "uvSize" ::: Vec2f
  ]

pushTransform :: Mat44f -> PlCmd r ()
pushTransform = pushField @Fields @"transform" VK_SHADER_STAGE_VERTEX_BIT

pushPos :: Vec2f -> PlCmd r ()
pushPos = pushField @Fields @"pos" VK_SHADER_STAGE_VERTEX_BIT

pushSize :: Vec2f -> PlCmd r ()
pushSize = pushField @Fields @"size" VK_SHADER_STAGE_VERTEX_BIT

pushUVPos :: Vec2f -> PlCmd r ()
pushUVPos = pushField @Fields @"uvPos" VK_SHADER_STAGE_VERTEX_BIT

pushUVSize :: Vec2f -> PlCmd r ()
pushUVSize = pushField @Fields @"uvSize" VK_SHADER_STAGE_VERTEX_BIT


loadShaders :: EngineCapability -> Resource [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = Resource $ do
  shaderVert <- auto $ shaderFile dev VK_SHADER_STAGE_VERTEX_BIT "shaders/sprites.vert.spv"
  shaderFrag <- auto $ shaderFile dev VK_SHADER_STAGE_FRAGMENT_BIT "shaders/single_sampler.frag.spv"

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


loadPipeline :: EngineCapability -> Resource (Tagged Pipeline ProtoPipeline, VkDescriptorSetLayout)
loadPipeline cap@EngineCapability{ dev } = Resource $ do
  shaderStages <- auto $ loadShaders cap
  (materialDSL, pipelineLayout) <- auto $ makePipelineLayout dev
  let createPipeline renderPass swapExtent msaaSamples =
        createGraphicsPipeline dev swapExtent
          [] []
          shaderStages
          renderPass
          pipelineLayout
          msaaSamples
          True
  return (Tagged ProtoPipeline{ pipelineLayout, createPipeline }, materialDSL)

draw :: PlCmd r ()
draw = plCmd $ \_ cmdBuf ->
  liftIO $ vkCmdDraw cmdBuf
    6 1 0 0 -- vertex count, instance count, first vertex, first instance