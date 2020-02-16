{-# LANGUAGE Strict #-}
module Lib.Vulkan.Shader
  ( createVkShaderStageCI
  , createVulkanShaderModule
  , specializationInfo
  ) where

import           Foreign.Ptr                    (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program
import           Lib.Program.Foreign



createVkShaderStageCI :: VkDevice
                      -> (CSize, Ptr Word32)
                      -> VkShaderStageFlagBits
                      -> Maybe VkSpecializationInfo
                      -> Program r VkPipelineShaderStageCreateInfo
createVkShaderStageCI dev shaderCode stageBit maySpecInfo = do
    let specInfo = maybe (specializationInfo [] 0 VK_NULL) id maySpecInfo
    shaderModule <- createVulkanShaderModule dev shaderCode
    return $ createVk @VkPipelineShaderStageCreateInfo
          $  set @"sType"  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
          &* set @"pNext"  VK_NULL
          &* set @"stage"  stageBit
          &* set @"module" shaderModule
          &* setStrRef @"pName" "main"
          &* setVkRef @"pSpecializationInfo" specInfo


createVulkanShaderModule :: VkDevice
                         -> (CSize, Ptr Word32)
                         -> Program r VkShaderModule
createVulkanShaderModule dev (codeSize, codePtr) =
    allocResource
      (\sm -> liftIO $ vkDestroyShaderModule dev sm VK_NULL) $
      withVkPtr smCreateInfo $ \smciPtr -> allocaPeek $
        runVk . vkCreateShaderModule dev smciPtr VK_NULL
  where
    smCreateInfo = createVk @VkShaderModuleCreateInfo
      $  set @"sType"    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
      &* set @"pNext"    VK_NULL
      &* set @"codeSize" codeSize
      &* set @"pCode"    codePtr
      &* set @"flags"    VK_ZERO_FLAGS


specializationInfo :: [VkSpecializationMapEntry] -> CSize -> Ptr Void -> VkSpecializationInfo
specializationInfo mapEntries dataSize pData = createVk
  $  setListCountAndRef @"mapEntryCount" @"pMapEntries" mapEntries
  &* set @"dataSize" dataSize
  &* set @"pData" pData

{-
-- example spec info
fooSpecInfo :: Word32 -> Program r VkSpecializationInfo
fooSpecInfo foo = do
  -- TODO restrict lifetime
  ptr <- mallocRes
  poke ptr foo
  return $ specializationInfo specMap 4 (castPtr ptr)
  where
  specMap =
    [ createVk
      $  set @"constantID" 0
      &* set @"offset" 0
      &* set @"size" 4
    ]
-}
