{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Vulkan.Command
  ( metaCommandPool
  , metaCommandBuffers
  , makeCommandBufferBeginInfo

  , forkWithCmdCap
  , postWithAndRetWait
  , postWith
  , postWith_
  , postWithAndRet

  , ManagedCommandBuffer(..)
  , releaseCommandBuffer

  , CommandCapability
  , metaCommandCapability
  , poolSwapOpportunity
  , acquireCommandBuffer

  , CommandPoolPool
  , metaCommandPoolPool
  , acquireCommandPool
  , releaseCommandPool

  -- TODO pools don't grow without insist
  , ManagedCommandPool
  , metaManagedCommandPool
  , waitResetableCommandPool
  , resetCommandPool
  , mcpAcquireCommandBuffer
  , mcpInsistAcquireCommandBuffer
  , mcpReleaseCommandBuffer
  ) where

import           Control.Monad
import           Data.Maybe
import qualified Foreign.Marshal.Array          as Foreign
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           UnliftIO.Concurrent
import           UnliftIO.IORef

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Queue



metaCommandPool :: VkDevice -> Word32 -> VkCommandPoolCreateFlags -> MetaResource VkCommandPool
metaCommandPool dev queueFamIdx flags =
  metaResource (liftIO . flip (vkDestroyCommandPool dev) VK_NULL) $
    allocaPeek $ \pPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" flags
        &* set @"queueFamilyIndex" queueFamIdx
      ) $ \ciPtr -> runVk $ vkCreateCommandPool dev ciPtr VK_NULL pPtr


metaCommandBuffers :: VkDevice
                   -> VkCommandPool
                   -> VkCommandBufferLevel
                   -> Int
                   -> MetaResource [VkCommandBuffer]
metaCommandBuffers dev cmdPool level buffersCount =
  metaResource
    (\cmdBufs -> liftIO $ Foreign.withArray cmdBufs $ \cbsPtr ->
      vkFreeCommandBuffers dev cmdPool (fromIntegral buffersCount) cbsPtr)
    $ do
    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"commandPool" cmdPool
          &* set @"level" level
          &* set @"commandBufferCount" (fromIntegral buffersCount)

    -- allocate a pointer to an array of command buffer handles
    allocaArray buffersCount $ \cbsPtr -> do
      withVkPtr allocInfo $ \aiPtr ->
        runVk $ vkAllocateCommandBuffers dev aiPtr cbsPtr
      peekArray buffersCount cbsPtr




makeCommandBufferBeginInfo :: VkCommandBufferUsageFlags -> Maybe VkCommandBufferInheritanceInfo -> VkCommandBufferBeginInfo
makeCommandBufferBeginInfo flags inheritanceInfo =
  createVk @VkCommandBufferBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
            &* set @"flags" flags
            &* set @"pNext" VK_NULL
            &* maybe (set @"pInheritanceInfo" VK_NULL)
                     (setVkRef @"pInheritanceInfo") inheritanceInfo


postWithAndRetWait :: CommandCapability
                   -> ManagedQueue
                   -> [(VkSemaphore, VkPipelineStageFlags)]
                   -> [VkSemaphore]
                   -> (VkCommandBuffer -> Program a)
                   -> Program a
postWithAndRetWait cmdCap queue waitSemsWithStages signalSems action =
  locally $ do
    managedCmdBuf <- acquireCommandBuffer cmdCap
    let cmdBuf = actualCmdBuf managedCmdBuf
    let cmdbBI = makeCommandBufferBeginInfo VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    result <- action cmdBuf
    runVk $ vkEndCommandBuffer cmdBuf

    queueEvent <- postNotify queue $ makeSubmitInfo waitSemsWithStages signalSems [cmdBuf]
    -- async return because caller doesn't care about internal cleanup
    waitDone queueEvent
    releaseCommandBuffer managedCmdBuf
    return result
    -- continuation ends because of locally. Auto things from action get deallocated.


-- | Posts to ManagedQueue with new command buffer. Local continuation context.
--
--   Asynchronously cleans up allocations from action after the command buffer
--   has been executed.
postWithAndRet :: CommandCapability
               -> ManagedQueue
               -> [(VkSemaphore, VkPipelineStageFlags)]
               -> [VkSemaphore]
               -> (VkCommandBuffer -> Program a)
               -> Program (a, QueueEvent)
postWithAndRet cmdCap queue waitSemsWithStages signalSems action = do
  retBox <- newEmptyMVar
  _ <- forkProg $ run retBox
  takeMVar retBox

  where
  run retBox = do
    managedCmdBuf <- acquireCommandBuffer cmdCap
    let cmdBuf = actualCmdBuf managedCmdBuf
    let cmdbBI = makeCommandBufferBeginInfo VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    result <- action cmdBuf
    runVk $ vkEndCommandBuffer cmdBuf

    queueEvent <- postNotify queue $ makeSubmitInfo waitSemsWithStages signalSems [cmdBuf]
    -- async return because caller doesn't care about internal cleanup
    putMVar retBox (result, queueEvent)
    waitDone queueEvent
    releaseCommandBuffer managedCmdBuf
    -- continuation ends because of forkProg. Auto things from action get deallocated.


-- | Posts to ManagedQueue with new command buffer. Local continuation context.
--
--   Asynchronously cleans up allocations from action after the command buffer
--   has been executed.
postWith :: CommandCapability
         -> ManagedQueue
         -> [(VkSemaphore, VkPipelineStageFlags)]
         -> [VkSemaphore]
         -> (VkCommandBuffer -> Program ())
         -> Program QueueEvent
postWith cmdCap queue waitSemsWithStages signalSems action = do
  ((), queueEvent) <- postWithAndRet cmdCap queue waitSemsWithStages signalSems action
  return queueEvent


-- | Posts to ManagedQueue with new command buffer. Local continuation context.
--
--   Asynchronously cleans up allocations from action after the command buffer
--   has been executed.
postWith_ :: CommandCapability
          -> ManagedQueue
          -> [(VkSemaphore, VkPipelineStageFlags)]
          -> [VkSemaphore]
          -> (VkCommandBuffer -> Program ())
          -> Program ()
postWith_ cmdCap queue waitSemsWithStages signalSems action =
  void $ postWithAndRet cmdCap queue waitSemsWithStages signalSems action


forkWithCmdCap :: CommandPoolPool
               -> (CommandCapability -> Program ())
               -> Program ThreadId
forkWithCmdCap cmdPoolPool action =
  forkProg $ do
    cmdCap <- auto $ metaCommandCapability cmdPoolPool
    action cmdCap


data ManagedCommandBuffer = ManagedCommandBuffer
  { actualCmdBuf    :: VkCommandBuffer
  , cmdPoolOfBuffer :: ManagedCommandPool
  }



-- | Takes care of providing a thread with fresh command buffers.
--
--   touchIORef necessary if reused by another thread!
data CommandCapability = CommandCapability
  { cmdPoolPool :: CommandPoolPool
  , currentPool :: IORef ManagedCommandPool
  -- TODO need a way to prefetch the next command pool to avoid wait for cache sync between cpu cores
  -- , cmdPools     :: IORef [ManagedCommandPool]
  }

metaCommandCapability :: CommandPoolPool -> MetaResource CommandCapability
metaCommandCapability cpp =
  metaResource
  (\CommandCapability{..} -> do
      -- the touch here is only needed if destruction happens in another thread
      touchIORef currentPool
      releaseCommandPool cpp =<< readIORef currentPool
  )
  (do
      pool <- acquireCommandPool cpp
      currentPool <- newIORef pool
      return CommandCapability {cmdPoolPool=cpp, ..}
  )


-- | Give opportunity to obtain a fresh command pool. Not thread-safe.
poolSwapOpportunity :: CommandCapability -> Program ()
poolSwapOpportunity CommandCapability{..} = do
  -- TODO shouldn't swap if pool is mostly unused
  pool <- readIORef currentPool
  releaseCommandPool cmdPoolPool pool
  newPool <- acquireCommandPool cmdPoolPool
  writeIORef currentPool newPool

-- | Resource for automatic acquire and release of command buffers
resCommandBuffer :: CommandCapability -> Resource ManagedCommandBuffer
resCommandBuffer cap = do
  buf <- onCreate $ acquireCommandBuffer cap
  onDestroy $ releaseCommandBuffer buf
  return buf

-- | Acquire a command buffer from the capability. Not thread-safe.
acquireCommandBuffer :: CommandCapability -> Program ManagedCommandBuffer
acquireCommandBuffer CommandCapability{..} = do
  pool <- readIORef currentPool
  mcpAcquireCommandBuffer pool >>= \case
    Just buffer -> return $ ManagedCommandBuffer buffer pool
    Nothing -> do
      releaseCommandPool cmdPoolPool pool
      newPool <- acquireCommandPool cmdPoolPool
      writeIORef currentPool newPool
      buffer <- fromJust <$> mcpAcquireCommandBuffer newPool
      return $ ManagedCommandBuffer buffer newPool


-- | Give a command buffer back to the cmdBuf pool. Thread-safe.
releaseCommandBuffer :: ManagedCommandBuffer -> Program ()
releaseCommandBuffer ManagedCommandBuffer{..} =
  mcpReleaseCommandBuffer cmdPoolOfBuffer actualCmdBuf


data CommandPoolPoolRequest
  = Shutdown (MVar ())
    -- ^ The command pool management thread puts the MVar when it has received the Shutdown request.
    -- Wait on the MVar to make sure the thread has worked through preceding Releases in the channel.
  | Release ManagedCommandPool

-- | Used to reset released command pools in the background and to keep track of fresh ones.
--   Completely thread-safe.
data CommandPoolPool = CommandPoolPool
  { mCommandPool :: MetaResource ManagedCommandPool
  , freshPools   :: MVar [ManagedCommandPool]
    -- ^ ready to use command pools (after creation or cmdPool reset)
  , requestChan :: Chan CommandPoolPoolRequest
    -- ^ return channel for released command pools
  }

-- | Initial number of command pools in a command pool pool
initialCmdPoolNum :: Int
initialCmdPoolNum = 2

metaCommandPoolPool :: VkDevice -> Word32 -> MetaResource CommandPoolPool
metaCommandPoolPool device queueFamIdx =
  metaResource
  (\CommandPoolPool{..} -> do
      -- WARNING destruction doesn't take into account CommandPools that are acquired
      mvar <- newEmptyMVar
      writeChan requestChan (Shutdown mvar)
      takeMVar mvar

      fresh <- takeMVar freshPools
      mapM_ (destroy mCommandPool) fresh
  )
  (do
      let mCommandPool = metaManagedCommandPool device queueFamIdx
      initialCmdPools <- replicateM initialCmdPoolNum (create mCommandPool)
      requestChan <- newChan
      freshPools <- newMVar initialCmdPools

      _ <- forkProg $ loop $
        readChan requestChan >>= \case
          Shutdown mvar -> do
            putMVar mvar ()
            return $ AbortLoop ()
          Release released -> do
            -- TODO check if released pool has enough free buffers to be reused without reset
            -- TODO pools that have outstanding buffers can not reset but can potentially still acquire buffers

            -- TODO not in separate thread for now because we would need to keep track of threads for clean shutdown
            waitResetableCommandPool released
            resetCommandPool released
            touchCommandPool released -- make sure changes arrive in next thread that uses the pool
            fresh <- takeMVar freshPools
            putMVar freshPools (released:fresh)
            return ContinueLoop

      return CommandPoolPool{..}
  )


acquireCommandPool :: CommandPoolPool -> Program ManagedCommandPool
acquireCommandPool CommandPoolPool{ mCommandPool, freshPools } = do
  fresh <- takeMVar freshPools
  case fresh of pool:rest -> putMVar freshPools rest >> return pool
                []        -> putMVar freshPools [] >> create mCommandPool

releaseCommandPool :: CommandPoolPool -> ManagedCommandPool -> Program ()
releaseCommandPool CommandPoolPool{ requestChan } cmdPool = do
  touchCommandPool cmdPool -- make sure changes arrive in next thread that uses the pool
  writeChan requestChan (Release cmdPool)


-- | Initial number of cmdBufs in a ManagedCommandPool
initialCmdBufNum :: Int
initialCmdBufNum = 1

-- | Designed for one thread to get cmdBufs from the pool and any thread to
--   return them to the pool.
data ManagedCommandPool = ManagedCommandPool
  { cmdPool         :: VkCommandPool
  , acquiredCount   :: IORef Int
    -- ^ number of acquired command buffers since the last cmdPool reset
  , releasedCmdBufs :: MVar (Int, [VkCommandBuffer])
    -- ^ released command buffers since the last cmdPool reset
  , enableNotify    :: IORef Bool
    -- ^ toggle for notification when the command pool can be reset
  , notifyResetable :: MVar ()
    -- ^ when enableNotify is true, this MVar will be put when all acquired
    --   command buffers have been released
  , freshCmdBufs    :: IORef [VkCommandBuffer]
    -- ^ command buffers from the pool that are ready to use
  , mCmdBufs        :: Int -> MetaResource [VkCommandBuffer]
  , dev             :: VkDevice
  }

-- | to make sure IORef writes from the other thread have arrived
touchCommandPool :: ManagedCommandPool -> Program ()
touchCommandPool ManagedCommandPool{..} = do
  touchIORef acquiredCount
  -- touchIORef enableNotify <- all writes to this are atomic already
  touchIORef freshCmdBufs



metaManagedCommandPool :: VkDevice -> Word32 -> MetaResource ManagedCommandPool
metaManagedCommandPool device queueFamIdx =
  let mCmdPool = metaCommandPool device queueFamIdx VK_ZERO_FLAGS
  in metaResource
  (\ManagedCommandPool{..} ->
      -- cmdBufs are freed automatically
      destroy mCmdPool cmdPool
  )
  (do
      cmdPool <- create mCmdPool
      acquiredCount <- newIORef 0
      let mCmdBufs = metaCommandBuffers device cmdPool VK_COMMAND_BUFFER_LEVEL_PRIMARY
      releasedCmdBufs <- newMVar (0, [])
      enableNotify <- newIORef False
      notifyResetable <- newEmptyMVar
      initialCmdBufs <- alloc (mCmdBufs initialCmdBufNum)
      freshCmdBufs <- newIORef initialCmdBufs
      return ManagedCommandPool{dev=device, ..}
  )

waitResetableCommandPool :: ManagedCommandPool -> Program ()
waitResetableCommandPool ManagedCommandPool{..} = do
  acquiredCnt <- readIORef acquiredCount
  (releasedCount, released) <- takeMVar releasedCmdBufs
  if acquiredCnt == releasedCount
    then
      putMVar releasedCmdBufs (releasedCount, released)
    else do
      void $ tryTakeMVar notifyResetable
      atomicWriteIORef enableNotify True
      putMVar releasedCmdBufs (releasedCount, released)
      takeMVar notifyResetable
      atomicWriteIORef enableNotify False

-- | Moves released command buffers to the fresh cmdBufs list. Not thread-safe.
--
--   Make sure that mcpAcquireCommandBuffer can't be called at the same time.
resetCommandPool :: ManagedCommandPool -> Program ()
resetCommandPool ManagedCommandPool{..} = do
  (_, cmdBufs) <- takeMVar releasedCmdBufs
  writeIORef acquiredCount 0
  putMVar releasedCmdBufs (0, [])
  runVk $ vkResetCommandPool dev cmdPool VK_ZERO_FLAGS
  -- usually freshCmdBufs should be empty or mostly empty here
  modifyIORef' freshCmdBufs (++ cmdBufs)

-- | Acquire a command buffer from the pool, if available. Not thread-safe.
mcpAcquireCommandBuffer :: ManagedCommandPool -> Program (Maybe VkCommandBuffer)
mcpAcquireCommandBuffer ManagedCommandPool{..} =
  -- first try freshCmdBufs to avoid thread synchronization
  readIORef freshCmdBufs >>= \case
    f:rest -> do
      writeIORef freshCmdBufs rest
      modifyIORef' acquiredCount (+1)
      return (Just f)
    [] -> return Nothing

-- | Acquire a command buffer from the pool, create one if none are available. Not thread-safe.
mcpInsistAcquireCommandBuffer :: ManagedCommandPool -> Program VkCommandBuffer
mcpInsistAcquireCommandBuffer ManagedCommandPool{..} = do
  -- first try freshCmdBufs to avoid thread synchronization
  cmdBuf <- readIORef freshCmdBufs >>= \case
    f:rest -> writeIORef freshCmdBufs rest >> return f
    [] -> head <$> alloc (mCmdBufs 1)
  modifyIORef' acquiredCount (+1)
  return cmdBuf

-- | Give a command buffer back to the managed command pool. Thread-safe.
mcpReleaseCommandBuffer :: ManagedCommandPool -> VkCommandBuffer -> Program ()
mcpReleaseCommandBuffer ManagedCommandPool{..} cmdBuf = do
  (releasedCount, released) <- takeMVar releasedCmdBufs
  let releasedCount' = releasedCount + 1

  enable <- readIORef enableNotify
  when enable $ do
    -- enabled -> acquiredCount can't change
    acquiredCnt <- readIORef acquiredCount
    when (acquiredCnt == releasedCount') $ void $ tryPutMVar notifyResetable ()

  putMVar releasedCmdBufs (releasedCount', cmdBuf:released)
