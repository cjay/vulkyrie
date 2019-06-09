{-# LANGUAGE Strict #-}
module Lib.Vulkan.Buffer
  ( createBuffer
  , copyBuffer
  , findMemoryType
  ) where

import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Memory


createBuffer :: EngineCapability
             -> VkDeviceSize
             -> VkBufferUsageFlags
             -> VkMemoryPropertyFlags
             -> Program r (MemoryLoc, VkBuffer)
createBuffer EngineCapability{dev, memPool} bSize bUsage bMemPropFlags = do
    let bufferInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" bSize
          &* set @"usage" bUsage
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL
    (buf, freeBufLater) <- allocResource'
      (\vb -> liftIO $ vkDestroyBuffer dev vb VK_NULL) $
      withVkPtr bufferInfo $ \biPtr -> allocaPeek $
        runVk . vkCreateBuffer dev biPtr VK_NULL

    memLoc <- allocBindBufferMem memPool bMemPropFlags buf
    -- The buf will be released before release of any of the resources
    -- allocated above, but after release on any allocations below.
    freeBufLater

    return (memLoc, buf)


copyBuffer :: VkCommandBuffer -> VkBuffer -> VkBuffer -> VkDeviceSize -> Program r ()
copyBuffer cmdBuf srcBuffer dstBuffer bSize = do
  let copyRegion = createVk @VkBufferCopy
        $  set @"srcOffset" 0
        &* set @"dstOffset" 0
        &* set @"size" bSize
  withVkPtr copyRegion $ liftIO . vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1


