{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict        #-}
module Lib.Vulkan.UniformBufferObject
  ( uboCreateBuffers
  , uboBufferInfo
  , uboUpdate
  ) where

import           Data.Bits                      ((.|.))
import           Foreign.Ptr                    (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Numeric.DataFrame

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Buffer
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Memory



uboCreateBuffers
  :: EngineCapability
  -> VkDeviceSize -- ^ size of uniform buffer object
  -> Int          -- ^ frames in flight
  -> Program r [(MemoryLoc, VkBuffer)]
uboCreateBuffers ecap size n = do
      sequence $ replicate n $ createBuffer ecap size
         VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
         ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )


uboBufferInfo :: VkDeviceSize -> VkBuffer -> Program r VkDescriptorBufferInfo
uboBufferInfo size uniformBuffer = return $ createVk @VkDescriptorBufferInfo
        $  set @"buffer" uniformBuffer
        &* set @"offset" 0
        &* set @"range" size


uboUpdate :: PrimBytes o => VkDevice -> VkDeviceSize -> VkDeviceMemory -> o -> Program r ()
uboUpdate device size mem ubo = do
      uboPtr <- allocaPeek $
        runVk . vkMapMemory device mem 0 size 0
      poke (castPtr uboPtr) (scalar ubo)
      liftIO $ vkUnmapMemory device mem
