module Lib.Vulkan.Engine
  ( EngineCapability(..)
  ) where

import           Graphics.Vulkan
import           Lib.Vulkan.Command
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Sync

data EngineCapability = EngineCapability
  { pdev     :: VkPhysicalDevice
  , dev      :: VkDevice
  , cmdCap   :: CommandCapability
  , cmdQueue :: ManagedQueue
  , semPool  :: SemaphorePool
  }

