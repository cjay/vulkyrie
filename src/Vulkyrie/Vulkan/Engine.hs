{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Engine
  ( EngineCapability(..)
  ) where

import           Graphics.Vulkan
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Memory
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync

data EngineCapability = EngineCapability
  { pdev     :: VkPhysicalDevice
  , dev      :: VkDevice
  , cmdCap   :: CommandCapability
  , cmdQueue :: ManagedQueue
  , semPool  :: SemaphorePool
  , memPool  :: MemoryPool
  , descriptorPool :: VkDescriptorPool
  }
