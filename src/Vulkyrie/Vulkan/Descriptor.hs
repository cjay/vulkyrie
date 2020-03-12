{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Descriptor
  ( createDescriptorPool
  , allocateDescriptorSet
  , allocateDescriptorSets
  , allocateDescriptorSetsForLayout
  , updateDescriptorSet
  , uniformBinding
  , samplerBinding
  , createDescriptorSetLayout
  ) where

import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource


{- some old brainstorming, probably too over-engineered for now:


import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map

import           Vulkyrie.MonadIO.IORef

-- inspect layouts, estimated count per layout -> num of descr types
-- stats: actual allocated counts per layout, used for next pool

data Profile = Profile
  { typeFreqs          :: [(VkDescriptorType, Word32)]
  , setCountMultiplier :: Word32
  , nextSize           :: IORef Word32
  , currentPool        :: IORef VkDescriptorPool
  , allPools           :: IORef [VkDescriptorPool]
  }

data DynamicDescriptorPool = DynamicDescriptorPool
  { -- DescriptorSetLayout -> profile: [layouts], n*type requested, n*type counted, current pool, occupied pools. growth rate 2*n+1
    -- if asked for, have one profile for a linear combination of layouts.

    -- request pooling: [(each:Int, Layout)], multiplier:Int, growth/shrink rule or curve, maybe next size hint.
    -- allocate: unseen layout -> single profile, exp growth.

    -- kill layouts, reset layouts

    -- request layout id: lookup in Map, make Map entry if needed. id comes from counter
    -- id is array index into profiles
    profiles :: IORef (Map VkDescriptorSet Profile)
  }
-}





-- TODO make pool size dynamic
createDescriptorPool :: VkDevice -> Int -> Resource r VkDescriptorPool
createDescriptorPool dev n =
  resource $ metaResource
    (liftIO . flip (vkDestroyDescriptorPool dev) VK_NULL) $
    allocaPeek $ \pPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* setListCountAndRef @"poolSizeCount" @"pPoolSizes"
          [ createVk @VkDescriptorPoolSize
            $  set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
            &* set @"descriptorCount" (fromIntegral n)
          , createVk @VkDescriptorPoolSize
            $  set @"type" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            &* set @"descriptorCount" (fromIntegral n)
          ]
        &* set @"maxSets" (fromIntegral n)
      ) $ \ciPtr -> runVk $ vkCreateDescriptorPool dev ciPtr VK_NULL pPtr


uniformBinding :: Word32 -> VkDescriptorSetLayoutBinding
uniformBinding bindId =
  createVk @VkDescriptorSetLayoutBinding
    $  set @"binding" bindId
    &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
    &* set @"descriptorCount" 1  -- array size in shader
    &* set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT
    &* set @"pImmutableSamplers" VK_NULL


samplerBinding :: Word32 -> VkDescriptorSetLayoutBinding
samplerBinding bindId =
  createVk @VkDescriptorSetLayoutBinding
    $  set @"binding" bindId
    &* set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    &* set @"descriptorCount" 1 -- array size in shader
    &* set @"stageFlags" VK_SHADER_STAGE_FRAGMENT_BIT
    &* set @"pImmutableSamplers" VK_NULL


createDescriptorSetLayout :: VkDevice
                          -> [VkDescriptorSetLayoutBinding]
                          -> Resource r VkDescriptorSetLayout
createDescriptorSetLayout dev bindings =
  let dslCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* setListCountAndRef @"bindingCount" @"pBindings" bindings
  in resource $ metaResource
     (\dsl -> liftIO $ vkDestroyDescriptorSetLayout dev dsl VK_NULL) $
     withVkPtr dslCreateInfo $ \dslciPtr -> allocaPeek $
       runVk . vkCreateDescriptorSetLayout dev dslciPtr VK_NULL


allocateDescriptorSet :: VkDevice
                      -> VkDescriptorPool
                      -> VkDescriptorSetLayout
                      -> Program r VkDescriptorSet
allocateDescriptorSet dev descriptorPool layout =
  head <$> allocateDescriptorSets dev descriptorPool [layout]


allocateDescriptorSetsForLayout :: VkDevice
                                -> VkDescriptorPool
                                -> Int
                                -> VkDescriptorSetLayout
                                -> Program r [VkDescriptorSet]
allocateDescriptorSetsForLayout dev descriptorPool n layout =
  allocateDescriptorSets dev descriptorPool (replicate n layout)


-- TODO should get dataframe instead of list
allocateDescriptorSets :: VkDevice
                       -> VkDescriptorPool
                       -> [VkDescriptorSetLayout]
                       -> Program r [VkDescriptorSet]
allocateDescriptorSets dev descriptorPool layouts = do
  let len = length layouts
  let dsai = createVk @VkDescriptorSetAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"descriptorPool" descriptorPool
        &* setListCountAndRef @"descriptorSetCount" @"pSetLayouts" layouts
  allocaArray len $ \dsPtr -> withVkPtr dsai $ \dsaiPtr -> do
    runVk $ vkAllocateDescriptorSets dev dsaiPtr dsPtr
    peekArray len dsPtr


updateDescriptorSet :: VkDevice
                    -> VkDescriptorSet
                    -> Word32
                    -> [VkDescriptorBufferInfo]
                    -> [VkDescriptorImageInfo]
                    -> Program r ()
updateDescriptorSet dev descriptorSet firstBinding uniformBufferInfos imageInfos =
  let uniformWrite bufferInfo binding =
        createVk @VkWriteDescriptorSet
          $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
          &* set @"pNext" VK_NULL
          &* set @"dstSet" descriptorSet
          &* set @"dstBinding" binding
          &* set @"dstArrayElement" 0
          &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
          &* set @"descriptorCount" 1
          &* setVkRef @"pBufferInfo" bufferInfo
          &* set @"pImageInfo" VK_NULL
          &* set @"pTexelBufferView" VK_NULL
      imageWrite imageInfo binding =
        createVk @VkWriteDescriptorSet
          $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
          &* set @"pNext" VK_NULL
          &* set @"dstSet" descriptorSet
          &* set @"dstBinding" binding
          &* set @"dstArrayElement" 0
          &* set @"descriptorType" VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          &* set @"descriptorCount" 1
          &* set @"pBufferInfo" VK_NULL
          &* setVkRef @"pImageInfo" imageInfo
          &* set @"pTexelBufferView" VK_NULL
      descriptorWrites = zipWith ($)
        (map uniformWrite uniformBufferInfos ++ map imageWrite imageInfos)
        [firstBinding..]
  in withVkArrayLen descriptorWrites $ \dwLen dwPtr ->
      liftIO $ vkUpdateDescriptorSets dev dwLen dwPtr 0 VK_NULL
