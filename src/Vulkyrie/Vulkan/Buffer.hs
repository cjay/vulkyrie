{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Buffer
  ( createBuffer
  , copyBuffer
  , findMemoryType
  ) where

import           Control.Monad.IO.Unlift
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Memory


createBuffer :: EngineCapability
             -> VkDeviceSize
             -> VkBufferUsageFlags
             -> VkMemoryPropertyFlags
             -> Resource (MemoryLoc, VkBuffer)
createBuffer EngineCapability{dev, memPool} bSize bUsage bMemPropFlags =
    let bufferInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" bSize
          &* set @"usage" bUsage
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL
        metaBuffer = metaResource
            (\vb -> liftIO $ vkDestroyBuffer dev vb VK_NULL) $
            withVkPtr bufferInfo $ \biPtr -> allocaPeek $
              runVk . vkCreateBuffer dev biPtr VK_NULL
    -- releasing the buffer before releasing the memory that is bound to it
    in Resource $ inverseDestruction $ do
      buf <- auto metaBuffer
      memLoc <- auto $ allocBindBufferMem memPool bMemPropFlags buf
      return (memLoc, buf)

copyBuffer :: VkCommandBuffer -> VkBuffer -> VkBuffer -> VkDeviceSize -> Prog r ()
copyBuffer cmdBuf srcBuffer dstBuffer bSize = do
  let copyRegion = createVk @VkBufferCopy
        $  set @"srcOffset" 0
        &* set @"dstOffset" 0
        &* set @"size" bSize
  withVkPtr copyRegion $ liftIO . vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1
