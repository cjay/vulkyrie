{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
module Vulkyrie.Vulkan.Default.Pipeline
  ( createGraphicsPipeline
  ) where

import           Data.Bits
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource


createGraphicsPipeline :: VkDevice
                       -> VkExtent2D
                       -> [VkVertexInputBindingDescription]
                       -> [VkVertexInputAttributeDescription]
                       -> [VkPipelineShaderStageCreateInfo]
                       -> VkRenderPass
                       -> VkPipelineLayout
                       -> VkSampleCountFlagBits
                       -> Bool
                       -> Resource VkPipeline
createGraphicsPipeline
    dev swapExtent bindDescs attrDescs shaderDescs renderPass pipelineLayout msaaSamples enableAlpha =
  let -- vertex input
      vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* setListCountAndRef @"vertexBindingDescriptionCount" @"pVertexBindingDescriptions" bindDescs
        &* setListCountAndRef @"vertexAttributeDescriptionCount" @"pVertexAttributeDescriptions" attrDescs

      -- input assembly
      inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* set @"primitiveRestartEnable" VK_FALSE

      -- viewports and scissors
      viewPort = createVk @VkViewport
        $  set @"x" 0
        &* set @"y" 0
        &* set @"width" (fromIntegral $ getField @"width" swapExtent)
        &* set @"height" (fromIntegral $ getField @"height" swapExtent)
        &* set @"minDepth" 0
        &* set @"maxDepth" 1

      scissor = createVk @VkRect2D
        $  set   @"extent" swapExtent
        &* setVk @"offset" ( set @"x" 0 &* set @"y" 0 )

      viewPortState = createVk @VkPipelineViewportStateCreateInfo
        $ set @"sType"
          VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"viewportCount" 1
        &* setVkRef @"pViewports" viewPort
        &* set @"scissorCount" 1
        &* setVkRef @"pScissors" scissor

      -- rasterizer
      rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"depthClampEnable" VK_FALSE
        &* set @"rasterizerDiscardEnable" VK_FALSE
        &* set @"polygonMode" VK_POLYGON_MODE_FILL
        &* set @"cullMode" VK_CULL_MODE_BACK_BIT
        &* set @"frontFace" VK_FRONT_FACE_CLOCKWISE
        &* set @"depthBiasEnable" VK_FALSE
        &* set @"depthBiasConstantFactor" 0
        &* set @"depthBiasClamp" 0
        &* set @"depthBiasSlopeFactor" 0
        &* set @"lineWidth" 1.0

      -- multisampling
      multisampling = createVk @VkPipelineMultisampleStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"sampleShadingEnable" VK_FALSE
        &* set @"rasterizationSamples" msaaSamples
        &* set @"minSampleShading" 1.0 -- Optional
        &* set @"pSampleMask" VK_NULL -- Optional
        &* set @"alphaToCoverageEnable" VK_FALSE -- Optional
        &* set @"alphaToOneEnable" VK_FALSE -- Optional

      -- one of these for each color attachment
      colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
        $  set @"colorWriteMask"
            (   VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT )
        &* set @"blendEnable" (if enableAlpha then VK_TRUE else VK_FALSE)
        &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_SRC_ALPHA -- Optional
        &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA -- Optional
        &* set @"colorBlendOp" VK_BLEND_OP_ADD -- Optional
        &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE -- Optional
        &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO -- Optional
        &* set @"alphaBlendOp" VK_BLEND_OP_ADD -- Optional

      colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"logicOpEnable" VK_FALSE
        &* set @"logicOp" VK_LOGIC_OP_COPY -- Optional
        &* set @"attachmentCount" 1
        &* setVkRef @"pAttachments" colorBlendAttachment
        &* setAt @"blendConstants" @0 0.0 -- Optional
        &* setAt @"blendConstants" @1 0.0 -- Optional
        &* setAt @"blendConstants" @2 0.0 -- Optional
        &* setAt @"blendConstants" @3 0.0 -- Optional

      depthStencilState = createVk @VkPipelineDepthStencilStateCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"depthTestEnable" (if enableAlpha then VK_FALSE else VK_TRUE)
        &* set @"depthWriteEnable" (if enableAlpha then VK_FALSE else VK_TRUE)
        &* set @"depthCompareOp" VK_COMPARE_OP_LESS
        &* set @"depthBoundsTestEnable" VK_FALSE
        &* set @"minDepthBounds" 0.0
        &* set @"maxDepthBounds" 1.0
        &* set @"stencilTestEnable" VK_FALSE
        &* setVk @"front"
            (  set @"failOp" VK_STENCIL_OP_KEEP
            &* set @"passOp" VK_STENCIL_OP_KEEP
            &* set @"depthFailOp" VK_STENCIL_OP_KEEP
            &* set @"compareOp" VK_COMPARE_OP_NEVER
            &* set @"compareMask" 0
            &* set @"writeMask" 0
            &* set @"reference" 0
            )
        &* setVk @"back"
            (  set @"failOp" VK_STENCIL_OP_KEEP
            &* set @"passOp" VK_STENCIL_OP_KEEP
            &* set @"depthFailOp" VK_STENCIL_OP_KEEP
            &* set @"compareOp" VK_COMPARE_OP_NEVER
            &* set @"compareMask" 0
            &* set @"writeMask" 0
            &* set @"reference" 0
            )

    -- finally, create pipeline!
  in Resource $ do
    let gpCreateInfo = createVk @VkGraphicsPipelineCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* setListCountAndRef @"stageCount" @"pStages" shaderDescs
          &* setVkRef @"pVertexInputState" vertexInputInfo
          &* setVkRef @"pInputAssemblyState" inputAssembly
          &* set @"pTessellationState" VK_NULL
          &* setVkRef @"pViewportState" viewPortState
          &* setVkRef @"pRasterizationState" rasterizer
          &* setVkRef @"pMultisampleState" multisampling
          &* setVkRef @"pDepthStencilState" depthStencilState
          &* setVkRef @"pColorBlendState" colorBlending
          &* set @"pDynamicState" VK_NULL
          &* set @"layout" pipelineLayout
          &* set @"renderPass" renderPass
          &* set @"subpass" 0
          &* set @"basePipelineHandle" VK_NULL_HANDLE
          &* set @"basePipelineIndex" (-1)

    auto $ metaResource
      (\gp -> liftIO $ vkDestroyPipeline dev gp VK_NULL) $
      withVkPtr gpCreateInfo $ \gpciPtr -> allocaPeek $
        runVk . vkCreateGraphicsPipelines dev VK_NULL 1 gpciPtr VK_NULL
