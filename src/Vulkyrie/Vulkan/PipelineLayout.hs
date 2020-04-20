{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
module Vulkyrie.Vulkan.PipelineLayout
  ( pushConstantRange
  , createPipelineLayout
  ) where

import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource


pushConstantRange :: VkShaderStageFlags
                  -> Word32
                  -> Word32
                  -> VkPushConstantRange
pushConstantRange stageFlags offset size = createVk
  $  set @"stageFlags" stageFlags
  &* set @"offset" offset
  &* set @"size" size


createPipelineLayout :: VkDevice
                     -> [VkDescriptorSetLayout]
                     -> [VkPushConstantRange]
                     -> Resource VkPipelineLayout
createPipelineLayout dev descrSetLayouts pushConstRanges = do
  let plCreateInfo = createVk @VkPipelineLayoutCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        -- the sequence of descr set layouts determines the set numbers
        &* setListCountAndRef @"setLayoutCount" @"pSetLayouts" descrSetLayouts   -- Optional
        &* setListCountAndRef @"pushConstantRangeCount" @"pPushConstantRanges" pushConstRanges -- Optional
  resource $ metaResource
    (\pl -> liftIO $ vkDestroyPipelineLayout dev pl VK_NULL) $
    withVkPtr plCreateInfo $ \plciPtr -> allocaPeek $
      runVk . vkCreatePipelineLayout dev plciPtr VK_NULL
