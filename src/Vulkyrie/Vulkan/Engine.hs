{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Engine where

import           Graphics.Vulkan
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Device
import           Vulkyrie.Vulkan.Memory
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync
import Vulkyrie.Concurrent
import Control.Monad.Reader (ReaderT(ReaderT, runReaderT))
import Vulkyrie.Program (Prog)

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


type Cmd r = ReaderT VkCommandBuffer (Prog r)
type PlCmd r = ReaderT VkPipelineLayout (Cmd r)

cmd :: (VkCommandBuffer -> Prog r a) -> Cmd r a
cmd = ReaderT

plCmd :: (VkPipelineLayout -> VkCommandBuffer -> Prog r a) -> PlCmd r a
plCmd = ReaderT . (ReaderT .)

runCmd :: VkCommandBuffer -> Cmd r a -> Prog r a
runCmd = flip runReaderT

runPl :: VkPipelineLayout -> PlCmd r a -> Cmd r a
runPl = flip runReaderT

runPlCmd :: VkPipelineLayout -> VkCommandBuffer -> PlCmd r a -> Prog r a
runPlCmd pipelineLayout cmdBuf = runCmd cmdBuf . runPl pipelineLayout