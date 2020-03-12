{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Shader
  ( metaFileContent
  , createShaderStage
  , metaShaderModule
  , shaderModuleFile
  , specializationInfo
  ) where

import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable               (pokeElemOff)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           System.IO

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource


-- | Copies file to new Word32 buffer with padding zeroes if necessary.
--   Size is in bytes.
metaFileContent :: FilePath -> MetaResource r (CSize, Ptr Word32)
metaFileContent fpath = metaResource
  (\(_, ptr) -> do
      liftIO $ Foreign.Marshal.Alloc.free ptr
  )
  (do
      fsize <- liftIO $ withBinaryFile fpath ReadMode hFileSize
      let wordCount =
            let (count, remBytes) = fsize `divMod` 4
            in fromIntegral $ if remBytes == 0 then count else count + 1
      ptr :: Ptr Word32 <- liftIO $ mallocArray wordCount
      -- write zero to the last Word32 to make sure the padding bytes are zero
      liftIO $ pokeElemOff ptr (wordCount-1) 0
      _ <- liftIO $ withBinaryFile fpath ReadMode $ \h -> hGetBuf h ptr $ fromIntegral fsize
      return (fromIntegral wordCount * 4, ptr)
  )

createShaderStage :: VkShaderModule
                  -> VkShaderStageFlagBits
                  -> Maybe VkSpecializationInfo
                  -> Program r VkPipelineShaderStageCreateInfo
createShaderStage shaderModule stageBit maySpecInfo = do
    let specInfo = maybe (specializationInfo [] 0 VK_NULL) id maySpecInfo
    return $ createVk @VkPipelineShaderStageCreateInfo
          $  set @"sType"  VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
          &* set @"pNext"  VK_NULL
          &* set @"stage"  stageBit
          &* set @"module" shaderModule
          &* setStrRef @"pName" "main"
          &* setVkRef @"pSpecializationInfo" specInfo


metaShaderModule :: VkDevice
                 -> (CSize, Ptr Word32)
                 -> MetaResource r VkShaderModule
metaShaderModule dev (codeSize, codePtr) =
    metaResource
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


shaderModuleFile :: VkDevice -> FilePath -> Resource r VkShaderModule
shaderModuleFile dev fpath = do
  content <- resource $ metaFileContent fpath
  resource $ metaShaderModule dev content


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
