{-# LANGUAGE Strict #-}
module Lib.Vulkan.Memory
  ( findMemoryType
  , MemTypeIndex(..)
  , MemoryLoc(..)
  , allocBindImageMem
  , allocBindBufferMem
  , MemoryPool
  , metaMemoryPool
  , allocMem
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Coerce
import qualified Data.Vector.Mutable.Dynamic              as VMD
import qualified Data.Vector.Unboxed                      as VU
import qualified Data.Vector.Unboxed.Mutable              as VUM
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame

import           Lib.MetaResource
import           Lib.Program
import           Lib.Program.Foreign


newtype MemTypeIndex = MemTypeIndex Word32

data MemoryLoc = MemoryLoc
  { memory       :: VkDeviceMemory
  , memoryOffset :: VkDeviceSize
  }


-- | Return an index of a memory type for a device
findMemoryType :: VkPhysicalDeviceMemoryProperties
               -> Word32 -- ^ type filter bitfield
               -> VkMemoryPropertyFlags
                  -- ^ desired memory properties
               -> Program r MemTypeIndex
findMemoryType memProps typeFilter properties = do
  let mtCount = getField @"memoryTypeCount" memProps
      memTypes = getVec @"memoryTypes" memProps
      go i | i == mtCount = throwVkMsg "Failed to find suitable memory type!"
           | otherwise = tryType i
      tryType i =
        if testBit typeFilter (fromIntegral i)
           && ( getField @"propertyFlags"
                (ixOff (fromIntegral i) memTypes)
                .&. properties
              ) == properties
        then return $ MemTypeIndex i
        else go (i+1)
  go 0


allocBindImageMem :: MemoryPool -> VkMemoryPropertyFlags -> VkImage -> Program r MemoryLoc
allocBindImageMem memPool@MemoryPool{dev, memProps} propFlags image = do
  memRequirements <- allocaPeek $ \reqsPtr ->
    liftIO $ vkGetImageMemoryRequirements dev image reqsPtr

  memType <- findMemoryType memProps
    (getField @"memoryTypeBits" memRequirements) propFlags

  loc@MemoryLoc{memory, memoryOffset} <-
    allocMem memPool memType (getField @"size" memRequirements) (getField @"alignment" memRequirements)
  runVk $ vkBindImageMemory dev image memory memoryOffset

  return loc


allocBindBufferMem :: MemoryPool -> VkMemoryPropertyFlags -> VkBuffer -> Program r MemoryLoc
allocBindBufferMem memPool@MemoryPool{dev, memProps} propFlags buffer = do
  memRequirements <- allocaPeek $ \reqsPtr ->
    liftIO $ vkGetBufferMemoryRequirements dev buffer reqsPtr

  memType <- findMemoryType memProps
    (getField @"memoryTypeBits" memRequirements) propFlags

  loc@MemoryLoc{memory, memoryOffset} <-
    allocMem memPool memType (getField @"size" memRequirements) (getField @"alignment" memRequirements)
  runVk $ vkBindBufferMemory dev buffer memory memoryOffset

  return loc


memoryChunkSize :: VkDeviceSize
memoryChunkSize = 128 * 1024 * 1024


-- | Very naive memory pool. Not thread-safe.
--
--   Helps with low maxMemoryAllocationCount and amortizes vkAllocateMemory time
--   across smaller allocations.
--
--   Can't free or defragment. Only one chunk size. Wastes free memory of
--   current chunk if it needs to start a new one, unless it needs to allocate
--   more than the chunk size, in which case the current chunk is left alone and
--   the allocation is passed through.
data MemoryPool = MemoryPool
  -- one vector entry for each memory type index
  { currentChunk :: VUM.MVector (PrimState IO) Word64 -- 32x VkDeviceMemory
  , usedSize     :: VUM.MVector (PrimState IO) Word64 -- 32x VkDeviceSize
  , memProps     :: VkPhysicalDeviceMemoryProperties
  , dev          :: VkDevice
  -- TODO next improvement: counter of users per occupied chunk. decrease on
  -- free. free when 0. need hashmap from chunk to counter.
  , occupied     :: VMD.MVector (PrimState IO) Word64 -- Nx VkDeviceMemory
  }


metaMemoryPool :: VkPhysicalDevice
               -> VkDevice
               -> MetaResource r MemoryPool
metaMemoryPool pdev dev =
  metaResource
  (\MemoryPool{ currentChunk, occupied } -> do
      occ <- liftIO $ VMD.unsafeFreeze occupied
      forM_ occ $ \chunk -> liftIO $ vkFreeMemory dev (coerce chunk) VK_NULL

      cur <- liftIO $ VU.unsafeFreeze currentChunk
      forM_ (VU.toList cur) $ \chunk -> liftIO $ vkFreeMemory dev (coerce chunk) VK_NULL
  )
  (do
      currentChunk <- liftIO $ VUM.replicate 32 0
      usedSize <- liftIO $ VUM.replicate 32 0
      memProps <- allocaPeek $ liftIO . vkGetPhysicalDeviceMemoryProperties pdev
      occupied <- liftIO $ VMD.new 0
      return MemoryPool{..}
  )


allocMem :: MemoryPool
         -> MemTypeIndex
         -> VkDeviceSize -- ^ requested size in bytes
         -> VkDeviceSize -- ^ requested alignment
         -> Program r MemoryLoc
allocMem MemoryPool{..} (MemTypeIndex memTypeIndex) requestSize alignment = do
  let vectIndex = fromIntegral memTypeIndex :: Int
      size = max requestSize memoryChunkSize
      allocate = do
        let allocInfo = createVk @VkMemoryAllocateInfo
              $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
              &* set @"pNext" VK_NULL
              &* set @"allocationSize" size
              &* set @"memoryTypeIndex" memTypeIndex
        withVkPtr allocInfo $ \aiPtr -> allocaPeek $
          runVk . vkAllocateMemory dev aiPtr VK_NULL

  chunk <- liftIO $ VUM.read currentChunk vectIndex

  if chunk == 0 then do
    newChunk <- allocate
    if size > requestSize then do
      liftIO $ VUM.write currentChunk vectIndex $ coerce newChunk
      liftIO $ VUM.write usedSize vectIndex $ coerce requestSize
    else do
      liftIO $ VMD.pushBack occupied $ coerce newChunk
    return $ MemoryLoc newChunk 0

  else do
    used <- liftIO $ coerce <$> VUM.read usedSize vectIndex
    let overhang = used `mod` alignment
        padding = (alignment - overhang) `mod` alignment
        paddedUsed = used + padding
        freeSize = memoryChunkSize - paddedUsed

    if requestSize <= freeSize then do
      liftIO $ VUM.write usedSize vectIndex $ coerce (paddedUsed + requestSize)
      return $ MemoryLoc (coerce chunk) paddedUsed

    else do
      newChunk <- allocate
      if size > requestSize then do
        liftIO $ VMD.pushBack occupied chunk
        liftIO $ VUM.write currentChunk vectIndex $ coerce newChunk
        liftIO $ VUM.write usedSize vectIndex $ coerce requestSize
      else do
        liftIO $ VMD.pushBack occupied $ coerce newChunk
      return $ MemoryLoc newChunk 0

