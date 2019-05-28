{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib.MetaResource
  ( MetaResource
  , create
  , destroy
  , metaResource
  , auto
  , manual
  , alloc
  , free
  ) where

import           Graphics.Vulkan.Core_1_0
import           Lib.Program

data MetaResource r a = MetaResource { destroy :: a -> Program' (), create :: Program r a }

-- | drop in replacement for Lib.Program.allocResource
metaResource :: (a -> Program' ()) -- ^ destroy resource
             -> Program r a        -- ^ allocate resource
             -> MetaResource r a
metaResource destroy create = MetaResource {..}
{-# INLINE metaResource #-}

auto :: MetaResource r a -> Program r a
auto MetaResource{..} = allocResource destroy create
{-# INLINE auto #-}

manual :: MetaResource r a -> Program r (a, Program' ())
manual MetaResource{..} = do
  x <- create
  return (x, destroy x)
{-# INLINE manual #-}

-- | This is purely for the right wording
class AllocFree a where
  free :: MetaResource r a -> (a -> Program' ())
  free = destroy
  {-# INLINE free #-}

  alloc :: MetaResource r a -> Program r a
  alloc = create
  {-# INLINE alloc #-}

-- These are the only three things that have a vkAlloc.. and vkFree.. function
instance AllocFree VkCommandBuffer
instance AllocFree VkDescriptorSet
instance AllocFree VkDeviceMemory
instance (AllocFree a) => AllocFree [a]
