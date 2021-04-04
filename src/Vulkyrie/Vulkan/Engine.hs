{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Engine
  ( EngineCapability(..)
  ) where

import           Graphics.Vulkan
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Device
import           Vulkyrie.Vulkan.Memory
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync
import Vulkyrie.Concurrent

data EngineCapability = EngineCapability
  { pdev     :: VkPhysicalDevice
  , dev      :: VkDevice
  , queues   :: DevQueues
  , cmdCap   :: CommandCapability
  , cmdQueue :: ManagedQueue
  , queueFam :: Word32
  , semPool  :: SemaphorePool
  , memPool  :: MemoryPool
  , descriptorPool :: VkDescriptorPool
  , engineThreadOwner :: ThreadOwner
  }
