{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Buffer
  ( createBuffer
  , copyBuffer
  , findMemoryType
  ) where

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
             -> Resource r (MemoryLoc, VkBuffer)
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
    in do
      (destroyBuf, buf) <- onCreate $ manual metaBuffer
      -- TODO resource instead of creation, with actual Resource
      memLoc <- onCreate $ allocBindBufferMem memPool bMemPropFlags buf
      -- The buf will be released before the memory
      onDestroy destroyBuf

      return (memLoc, buf)


copyBuffer :: VkCommandBuffer -> VkBuffer -> VkBuffer -> VkDeviceSize -> Program r ()
copyBuffer cmdBuf srcBuffer dstBuffer bSize = do
  let copyRegion = createVk @VkBufferCopy
        $  set @"srcOffset" 0
        &* set @"dstOffset" 0
        &* set @"size" bSize
  withVkPtr copyRegion $ liftIO . vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1
