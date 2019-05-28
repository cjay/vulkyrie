{-# LANGUAGE Strict #-}
module Lib.Vulkan.VertexBuffer
  ( createVertexBuffer
  , createIndexBuffer
  ) where

import           Data.Bits
import           Foreign.Ptr              (castPtr)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Numeric.DataFrame
import           Numeric.Dimensions

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Buffer
import           Lib.Vulkan.Command
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Vertex
import           Lib.Vulkan.Sync


createVertexBuffer :: EngineCapability
                   -> DataFrame Vertex '[XN 3]
                      -- ^ A collection of at least three vertices
                   -> Program r (VkSemaphore, VkBuffer)
createVertexBuffer EngineCapability{..} (XFrame vertices) = do

    let bSize = fromIntegral $ bSizeOf vertices

    (_, vertexBuf) <-
      createBuffer pdev dev bSize
        ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT )
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    sem <- head <$> acquireSemaphores semPool 1
    -- Use "locally" to destroy temporary staging buffer after data copy is complete
    withCmdBuf cmdCap cmdQueue [] [sem] $ \cmdBuf -> do
      (stagingMem, stagingBuf) <-
        createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
          ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

      -- copy data
      stagingDataPtr <- allocaPeek $
        runVk . vkMapMemory dev stagingMem 0 bSize 0
      poke (castPtr stagingDataPtr) vertices
      liftIO $ vkUnmapMemory dev stagingMem
      copyBuffer cmdBuf stagingBuf vertexBuf bSize

    return (sem, vertexBuf)


createIndexBuffer :: EngineCapability
                  -> DataFrame Word32 '[XN 3]
                     -- ^ A collection of at least three indices
                  -> Program r (VkSemaphore, VkBuffer)
createIndexBuffer EngineCapability{..} (XFrame indices) = do

    let bSize = fromIntegral $ bSizeOf indices

    (_, vertexBuf) <-
      createBuffer pdev dev bSize
        ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT )
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    sem <- head <$> acquireSemaphores semPool 1
    -- Use "locally" to destroy temporary staging buffer after data copy is complete
    withCmdBuf cmdCap cmdQueue [] [sem] $ \cmdBuf -> do
      (stagingMem, stagingBuf) <-
        createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
          ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

      -- copy data
      stagingDataPtr <- allocaPeek $
        runVk . vkMapMemory dev stagingMem 0 bSize 0
      poke (castPtr stagingDataPtr) indices
      liftIO $ vkUnmapMemory dev stagingMem
      copyBuffer cmdBuf stagingBuf vertexBuf bSize

    return (sem, vertexBuf)
