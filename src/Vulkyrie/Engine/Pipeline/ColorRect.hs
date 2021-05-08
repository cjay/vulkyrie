{-# LANGUAGE TypeOperators #-}
module Vulkyrie.Engine.Pipeline.ColorRect where

import           Numeric.DataFrame
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.PushConstant
import Vulkyrie.Vulkan.Engine
import Vulkyrie.Engine.Pipeline
import Vulkyrie.Vulkan.Shader (shaderFile)
import Vulkyrie.Vulkan.PipelineLayout
import Vulkyrie.Vulkan.Default.Pipeline (createGraphicsPipeline)
import Data.Tagged (Tagged(Tagged))

-- | Handle for type-directed selection of pipelines
data Pipeline

type Fields =
  [ "transform" ::: Mat44f,
    "color" ::: Vec4f,
    "pos" ::: Vec2f,
    "size" ::: Vec2f,
    "center" ::: Vec2f,
    "turns" ::: Scalar Float
  ]

pushTransform :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Prog r ()
pushTransform = pushField @Fields @"transform" VK_SHADER_STAGE_VERTEX_BIT

pushPos :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Prog r ()
pushPos = pushField @Fields @"pos" VK_SHADER_STAGE_VERTEX_BIT

pushSize :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Prog r ()
pushSize = pushField @Fields @"size" VK_SHADER_STAGE_VERTEX_BIT

pushCenter :: VkCommandBuffer -> VkPipelineLayout -> Vec2f -> Prog r ()
pushCenter = pushField @Fields @"center" VK_SHADER_STAGE_VERTEX_BIT

pushTurns :: VkCommandBuffer -> VkPipelineLayout -> Scalar Float -> Prog r ()
pushTurns = pushField @Fields @"turns" VK_SHADER_STAGE_VERTEX_BIT

pushColor :: VkCommandBuffer -> VkPipelineLayout -> Vec4f -> Prog r ()
pushColor = pushField @Fields @"color" VK_SHADER_STAGE_VERTEX_BIT

loadShaders :: EngineCapability -> Resource [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = Resource $ do
  shaderVert <- auto $ shaderFile dev VK_SHADER_STAGE_VERTEX_BIT "shaders/color_rect.vert.spv"
  shaderFrag <- auto $ shaderFile dev VK_SHADER_STAGE_FRAGMENT_BIT "shaders/pass_color.frag.spv"
  return [shaderVert, shaderFrag]

makePipelineLayout :: VkDevice -> Resource VkPipelineLayout
makePipelineLayout dev = Resource $ do
  let (offset, len) = wholeRange @Fields
  pipelineLayout <- auto $ createPipelineLayout dev
    -- descriptor set numbers 0,1,..
    []
    -- push constant ranges
    [ pushConstantRange VK_SHADER_STAGE_VERTEX_BIT offset len
    ]
  return pipelineLayout


loadPipeline :: EngineCapability -> Resource (Tagged Pipeline ProtoPipeline)
loadPipeline cap@EngineCapability{ dev } = Resource $ do
  shaderStages <- auto $ loadShaders cap
  pipelineLayout <- auto $ makePipelineLayout dev
  let createPipeline renderPass swapExtent msaaSamples =
        createGraphicsPipeline dev swapExtent
          [] []
          shaderStages
          renderPass
          pipelineLayout
          msaaSamples
          True
  return $ Tagged ProtoPipeline{ pipelineLayout, createPipeline }

draw :: VkCommandBuffer -> Prog r ()
draw cmdBuf =
  liftIO $ vkCmdDraw cmdBuf
    6 1 0 0 -- vertex count, instance count, first vertex, first instance