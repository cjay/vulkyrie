{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Vulkyrie.Vulkan.Memory
  ( findMemoryType
  , MemTypeIndex(..)
  , MemoryLoc(..)
  , allocBindImageMem
  , allocBindBufferMem
  , MemoryPool
  , createMemoryPool
  , allocMem
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Vector.Mutable as MVector
import           Data.Vector.Mutable (MVector)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame ( ixOff )
import           UnliftIO.Exception

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import Data.Maybe
import Safe (headMay)
import Data.Foldable (toList)
import Data.Foldable (foldl')

iterateMaybe :: forall a. (a -> Maybe a) -> Maybe a -> [a]
iterateMaybe f ma = maybe [] go ma
  where
    go a = a : maybe [] go (f a)

rawAlloc :: VkDevice -> MemTypeIndex -> VkDeviceSize -> Prog r VkDeviceMemory
rawAlloc dev (MemTypeIndex memTypeIndex) size = do
  let allocInfo = createVk @VkMemoryAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"allocationSize" size
        &* set @"memoryTypeIndex" memTypeIndex
  withVkPtr allocInfo $ \aiPtr -> allocaPeek $
    runVk . vkAllocateMemory dev aiPtr VK_NULL

rawFree :: VkDevice -> VkDeviceMemory -> Prog r ()
rawFree dev mem = liftIO $ vkFreeMemory dev mem VK_NULL

newtype MemTypeIndex = MemTypeIndex Word32

-- | Return an index of a memory type for a device
findMemoryType :: VkPhysicalDeviceMemoryProperties
               -> Word32 -- ^ type filter bitfield
               -> VkMemoryPropertyFlags
                  -- ^ desired memory properties
               -> Prog r MemTypeIndex
findMemoryType memProps typeFilter properties = do
  let mtCount = getField @"memoryTypeCount" memProps
      memTypes = getVec @"memoryTypes" memProps
      go i | i == mtCount = throwString "Failed to find suitable memory type!"
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


data MemoryLoc = MemoryLoc
  { memory       :: VkDeviceMemory
  , memoryOffset :: VkDeviceSize
  , memorySize   :: VkDeviceSize
  , allocSize    :: VkDeviceSize
  }

-- TODO unify with allocBindBufferMem
allocBindImageMem :: MemoryPool -> VkMemoryPropertyFlags -> VkImage -> Resource MemoryLoc
allocBindImageMem memPool@MemoryPool{dev, memProps} propFlags image = Resource $ do
  memRequirements <- allocaPeek $ \reqsPtr ->
    liftIO $ vkGetImageMemoryRequirements dev image reqsPtr

  memType <- findMemoryType memProps
    (getField @"memoryTypeBits" memRequirements) propFlags

  loc@MemoryLoc{memory, memoryOffset} <- auto $
    allocMem memPool memType (getField @"size" memRequirements) (getField @"alignment" memRequirements)
  runVk $ vkBindImageMemory dev image memory memoryOffset

  return loc


allocBindBufferMem :: MemoryPool -> VkMemoryPropertyFlags -> VkBuffer -> Resource MemoryLoc
allocBindBufferMem memPool@MemoryPool{dev, memProps} propFlags buffer = Resource $ do
  memRequirements <- allocaPeek $ \reqsPtr ->
    liftIO $ vkGetBufferMemoryRequirements dev buffer reqsPtr

  memType <- findMemoryType memProps
    (getField @"memoryTypeBits" memRequirements) propFlags

  loc@MemoryLoc{memory, memoryOffset} <- auto $
    allocMem memPool memType (getField @"size" memRequirements) (getField @"alignment" memRequirements)
  runVk $ vkBindBufferMemory dev buffer memory memoryOffset

  return loc


-- TODO determine best size at runtime
memoryChunkSize :: VkDeviceSize
memoryChunkSize = 128 * 1024 * 1024


-- | Best-fit sub-allocating memory pool. Not thread-safe.
--
--   Helps with low maxMemoryAllocationCount and amortizes vkAllocateMemory time
--   across smaller allocations.
data MemoryPool = MemoryPool
  { dev      :: VkDevice
  , memProps :: VkPhysicalDeviceMemoryProperties
  , bufferImageGranularity :: VkDeviceSize
  , pools    :: MVector (PrimState IO) MemPool
    -- ^ one vector entry for each memory type index
  }

createMemoryPool :: VkPhysicalDevice
                 -> VkDevice
                 -> Resource MemoryPool
createMemoryPool pdev dev = Resource $
  do
    memProps <- allocaPeek $ liftIO . vkGetPhysicalDeviceMemoryProperties pdev
    let memTypeCount = getField @"memoryTypeCount" memProps
    devProps <- allocaPeek $ \propsPtr ->
      liftIO $ vkGetPhysicalDeviceProperties pdev propsPtr
    let limits = getField @"limits" devProps
    let bufferImageGranularity = getField @"bufferImageGranularity" limits

    pools <- liftIO $ MVector.new $ fromIntegral memTypeCount
    forM_ [0..(memTypeCount - 1)] $ \index -> do
      let pool = makeMemPool dev (MemTypeIndex index)
      liftIO $ MVector.write pools (fromIntegral index) pool

    onDestroy $ forM_ [0..(memTypeCount - 1)] $ \index -> do
      pool <- liftIO $ MVector.read pools (fromIntegral index)
      destroyMemPool pool

    pure MemoryPool{..}

allocMem :: MemoryPool
         -> MemTypeIndex
         -> VkDeviceSize -- ^ requested size in bytes
         -> VkDeviceSize -- ^ requested alignment
         -> MetaResource MemoryLoc
allocMem MemoryPool{..} (MemTypeIndex memTypeIndex) requestSize requestAlignment =
  -- For real world bufferImageGranularity see
  -- https://vulkan.gpuinfo.org/displaydevicelimit.php?name=bufferImageGranularity&platform=windows.
  -- In order to respect the granularity, Vulkan Memory Allocator keeps track of
  -- all allocations in its data structures and checks neighboring allocations
  -- for their linearity when allocating. Simply bumping the alignment makes
  -- this unnecessary. On modern hardware with a granularity of 1024 this should
  -- lead to only about 512 bytes of loss on average per allocation. Only on
  -- some older NVidia hardware it would be 16KB loss.

  -- No exceptions to this alignment can be made, even if you happen to know
  -- that the neighboring allocations are known to be linear/non-linear, because
  -- neighbors could be freed and reallocated with different linearity. So
  -- either the info about neighbors is always available to check, or the
  -- alignment has to be always respected.

  -- To avoid the loss, you could sub-allocate from a MemoryLoc and chose to
  -- only place non-linear/linear memory resources there.
  let alignment = max requestAlignment bufferImageGranularity
  in
  metaResource
    (\loc -> do
      let index = fromIntegral memTypeIndex
      pool <- liftIO $ MVector.read pools index
      pool' <- free pool loc
      liftIO $ MVector.write pools index pool'
    )
    (do
      let index = fromIntegral memTypeIndex
      pool <- liftIO $ MVector.read pools index
      (pool', memLoc) <- alloc pool requestSize alignment
      liftIO $ MVector.write pools index pool'
      pure memLoc
    )


-- | A raw VkDeviceMemory to be sub-allocated
data Block = Block
  { mem       :: VkDeviceMemory
  , blockSize :: VkDeviceSize
  , spaceMap  :: Map VkDeviceSize Chunk
  } deriving Show

-- The key type to lookup blocks.
-- VkDeviceMemory is a non-dispatchable handle, Vulkan spec says such handles are unique per VkDevice.
type BlockId = VkDeviceMemory

-- | A free region inside a Block.
--
-- Default ordering is by address.
data Chunk = Chunk
  { blockId   :: BlockId
  , chunkOffset    :: VkDeviceSize
  , chunkSize :: VkDeviceSize
  } deriving (Eq, Ord, Show)

chunkEnd :: Chunk -> VkDeviceSize
chunkEnd Chunk{..} = chunkOffset + chunkSize

chunkStart :: Chunk -> VkDeviceSize
chunkStart Chunk{..} = chunkOffset

data MemPool = MemPool
  { dev           :: VkDevice
  , memTypeIndex  :: MemTypeIndex
  , blocks        :: Map BlockId Block
  , sizeMap       :: Map VkDeviceSize (Set Chunk)
  }

-- Update with chunk change. New chunk HAS to have the same offset.
updatePool :: MemPool -> (Chunk, Maybe Chunk) -> MemPool
updatePool pool@MemPool{ blocks, sizeMap } (oldChunk, mayNewChunk) =
  let Chunk{ blockId, chunkSize, chunkOffset } = oldChunk
      precond = case mayNewChunk of
        Just Chunk{ chunkOffset = newOffset } -> newOffset == chunkOffset
        Nothing -> True
      blocks' = Map.adjust upd blockId blocks
        where
          upd block@Block{ spaceMap } =
            let spaceMap' = Map.update (const mayNewChunk) chunkOffset spaceMap
              in block{ spaceMap = spaceMap' }
      sizeMap' = Map.update del chunkSize sizeMap
        where
          del cs =
            let cs' = Set.delete oldChunk cs
             in if null cs' then Nothing else Just cs'
      sizeMap'' = case mayNewChunk of
        Nothing -> sizeMap'
        Just newChunk@Chunk{ chunkSize = newChunkSize } -> Map.alter (Just . ins) newChunkSize sizeMap'
          where ins Nothing = Set.singleton newChunk
                ins (Just cs) = Set.insert newChunk cs
    in assert precond $ pool{ blocks = blocks', sizeMap = sizeMap'' }

updatePoolFreshBlock :: MemPool -> Block -> Maybe Chunk -> MemPool
updatePoolFreshBlock pool@MemPool{ blocks, sizeMap } block mayNewChunk =
  let Block{ mem = blockId, spaceMap } = block
      spaceMap' = case mayNewChunk of
        Nothing -> spaceMap'
        Just newChunk@Chunk{ chunkOffset } -> Map.insert chunkOffset newChunk spaceMap
      block' = block{ spaceMap = spaceMap' }
      blocks' = Map.insert blockId block' blocks
      sizeMap' = case mayNewChunk of
        Nothing -> sizeMap
        Just newChunk@Chunk{ chunkSize = newChunkSize } -> Map.alter (Just . ins) newChunkSize sizeMap
          where ins Nothing = Set.singleton newChunk
                ins (Just cs) = Set.insert newChunk cs
    in pool{ blocks = blocks', sizeMap = sizeMap' }

-- TODO maybe reuse/extend updatePool
free :: MemPool -> MemoryLoc -> Prog r MemPool
free pool@MemPool{..} MemoryLoc{..} =
  let allocStart = memoryOffset
      allocEnd = memoryOffset + allocSize
      block@Block{ spaceMap, blockSize, mem } = blocks Map.! memory
      leftChunk = snd <$> Map.lookupGT allocStart spaceMap
      rightChunk = snd <$> Map.lookupLT allocStart spaceMap
      mergeLeft = leftChunk >>= \chunk ->
        if chunkEnd chunk == allocStart
        then pure $ (chunk, chunkStart chunk)
        else Nothing
      mergeRight = rightChunk >>= \chunk ->
        if chunkStart chunk == allocEnd
        then pure $ (chunk, chunkEnd chunk)
        else Nothing
      chunksToReplace = map fst $ catMaybes [mergeLeft, mergeRight]
      freeStart = fromMaybe allocStart (snd <$> mergeLeft)
      freeEnd = fromMaybe allocEnd (snd <$> mergeRight)
      newChunkSize = freeEnd - freeStart
      blockBecomesEmpty = freeStart == 0 && newChunkSize == blockSize
      newChunk = Chunk { blockId = memory, chunkOffset = freeStart, chunkSize = newChunkSize }
      spaceMapDel = foldl' del spaceMap chunksToReplace
        where del smap chunk = Map.delete (chunkStart chunk) smap
      spaceMapUpd = Map.insert (chunkEnd newChunk) newChunk spaceMapDel
      block' = block{ spaceMap = spaceMapUpd }
      blocksUpd = Map.insert memory block' blocks
      blocksDel = Map.delete memory blocks
      sizeMapDel = foldl' del sizeMap chunksToReplace
        where del smap chunk@Chunk{ chunkSize } = Map.update (del' chunk) chunkSize smap
              del' chunk cs =
                let cs' = Set.delete chunk cs
                 in if null cs' then Nothing else Just cs'
      sizeMapUpd = Map.alter (Just . ins) newChunkSize sizeMapDel
          where ins Nothing = Set.singleton newChunk
                ins (Just cs) = Set.insert newChunk cs
   in
      if blockBecomesEmpty && length blocks > 1
      then do
        rawFree dev mem
        pure pool{ blocks = blocksDel, sizeMap = sizeMapDel }
      else pure $ pool{ blocks = blocksUpd, sizeMap = sizeMapUpd }


-- | Try to fit allocation into chunk, as far right as possible.
--
-- Returns (if allocation fits in the chunk, needed upper padding in the chunk)
fit :: VkDeviceSize -> VkDeviceSize -> VkDeviceSize -> VkDeviceSize -> (Bool, VkDeviceSize)
fit chunkOffset chunkSize alignment requestSize =
  let precond = chunkSize >= requestSize
      maxOffset = chunkOffset + chunkSize - requestSize
      padding = maxOffset `mod` alignment
   in assert precond (maxOffset >= padding, padding)

alloc :: MemPool
      -> VkDeviceSize -- ^ requested size in bytes
      -> VkDeviceSize -- ^ requested alignment
      -> Prog r (MemPool, MemoryLoc)
alloc pool@MemPool{..} requestSize alignment =
  let -- Returns Maybe for success if the chunk actually was big enough.
      -- Result contains (given chunk, Chunk with leftover mem from the chunk).
      -- Leftover mem can be zero, another Maybe for that.
      tryChunk :: Block -> Chunk -> Maybe ((Chunk, Maybe Chunk), MemoryLoc)
      tryChunk Block{ mem } chunk@Chunk{..} =
        let (doesFit, padding) = fit chunkOffset chunkSize alignment requestSize
            allocSize = requestSize + padding
            newChunkSize = chunkSize - allocSize
            chunk' =
              if newChunkSize > 0
              then Just chunk{ chunkSize = newChunkSize }
              else Nothing
            memoryOffset = chunkOffset + chunkSize - allocSize
            loc = MemoryLoc{ memory = mem, memoryOffset, memorySize = requestSize, allocSize }
         in if doesFit then Just ((chunk, chunk'), loc) else Nothing

      -- All chunks from existing blocks that are big enough for the request size.
      -- Not necessarily big enough when respecting alignment.
      candidateChunks = concatMap toList . map snd .
        iterateMaybe (flip Map.lookupGT sizeMap . fst) $
          Map.lookupGE requestSize sizeMap

      -- Try to do allocation with the chunks from existing blocks
      existingTry = headMay $
        mapMaybe
          (\chunk@Chunk{ blockId } -> tryChunk (blocks Map.! blockId) chunk)
          candidateChunks

   in case existingTry of
        Just (chunkUpdate, memoryLoc) -> pure (updatePool pool chunkUpdate, memoryLoc)
        Nothing -> do
          let blockSize = max memoryChunkSize requestSize
          mem <- rawAlloc dev memTypeIndex blockSize
          let block = Block{ mem, blockSize, spaceMap = Map.empty }
              ((_, mayNewChunk), memoryLoc) = fromJust $
                tryChunk block Chunk{ blockId = mem, chunkOffset = 0, chunkSize = blockSize }
          pure (updatePoolFreshBlock pool block mayNewChunk, memoryLoc)

makeMemPool :: VkDevice -> MemTypeIndex -> MemPool
makeMemPool dev memTypeIndex = MemPool{ dev, memTypeIndex, blocks = Map.empty, sizeMap = Map.empty }

destroyMemPool :: MemPool -> Prog r ()
destroyMemPool MemPool{ dev, blocks } = forM_ blocks $ \Block{ mem } -> rawFree dev mem