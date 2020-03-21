{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.VertexBuffer
  ( createVertexBuffer
  , createIndexBuffer
  ) where

import           Data.Bits
import           Foreign.Ptr              (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Numeric.DataFrame

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Buffer
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Memory
import           Vulkyrie.Vulkan.Queue


createVertexBuffer :: (PrimBytes v)
                   => EngineCapability
                   -> DataFrame v '[XN 3]
                      -- ^ A collection of at least three vertices
                   -> Resource r (QueueEvent, VkBuffer)
createVertexBuffer ecap@EngineCapability{ dev, cmdCap, cmdQueue } (XFrame vertices) = do

    let bSize = bSizeOf vertices

    (_, vertexBuf) <-
      createBuffer ecap bSize
        ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT )
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    onCreate $ do
      finishedEvent <- postWith cmdCap cmdQueue [] [] $ \cmdBuf -> do
        (stagingMem, stagingBuf) <-
          auto $ createBuffer ecap bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

        -- copy data
        stagingDataPtr <- allocaPeek $
          runVk . vkMapMemory dev (memory stagingMem) (memoryOffset stagingMem) bSize VK_ZERO_FLAGS
        poke (castPtr stagingDataPtr) vertices
        liftIO $ vkUnmapMemory dev (memory stagingMem)
        copyBuffer cmdBuf stagingBuf vertexBuf bSize

      return (finishedEvent, vertexBuf)


createIndexBuffer :: EngineCapability
                  -> DataFrame Word32 '[XN 3]
                     -- ^ A collection of at least three indices
                  -> Resource r (QueueEvent, VkBuffer)
createIndexBuffer ecap@EngineCapability{ dev, cmdCap, cmdQueue } (XFrame indices) = do

    let bSize = bSizeOf indices

    (_, vertexBuf) <-
      createBuffer ecap bSize
        ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT )
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    onCreate $ do
      finishedEvent <- postWith cmdCap cmdQueue [] [] $ \cmdBuf -> do
        (stagingMem, stagingBuf) <-
          auto $ createBuffer ecap bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

        -- copy data
        stagingDataPtr <- allocaPeek $
          runVk . vkMapMemory dev (memory stagingMem) (memoryOffset stagingMem) bSize VK_ZERO_FLAGS
        poke (castPtr stagingDataPtr) indices
        liftIO $ vkUnmapMemory dev (memory stagingMem)
        copyBuffer cmdBuf stagingBuf vertexBuf bSize

      return (finishedEvent, vertexBuf)
