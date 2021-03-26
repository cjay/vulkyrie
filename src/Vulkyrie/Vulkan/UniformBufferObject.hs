{-# LANGUAGE Strict        #-}
module Vulkyrie.Vulkan.UniformBufferObject
  ( uboCreateBuffers
  , uboBufferInfo
  , uboUpdate
  ) where

import           Control.Monad
import           Data.Bits                      ((.|.))
import           Foreign.Ptr                    (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Numeric.DataFrame

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Buffer
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Memory



uboCreateBuffers
  :: EngineCapability
  -> VkDeviceSize -- ^ size of uniform buffer object
  -> Int          -- ^ frames in flight
  -> Resource [(MemoryLoc, VkBuffer)]
uboCreateBuffers ecap size n =
      replicateM n $ createBuffer ecap size
         VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
         ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )


uboBufferInfo :: VkDeviceSize -> VkBuffer -> Prog r VkDescriptorBufferInfo
uboBufferInfo size uniformBuffer = return $ createVk @VkDescriptorBufferInfo
        $  set @"buffer" uniformBuffer
        &* set @"offset" 0
        &* set @"range" size


uboUpdate :: PrimBytes o => VkDevice -> VkDeviceSize -> VkDeviceMemory -> o -> Prog r ()
uboUpdate device size mem ubo = do
      uboPtr <- allocaPeek $
        runVk . vkMapMemory device mem 0 size VK_ZERO_FLAGS
      poke (castPtr uboPtr) (scalar ubo)
      liftIO $ vkUnmapMemory device mem
