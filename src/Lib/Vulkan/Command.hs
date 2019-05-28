{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Lib.Vulkan.Command
  ( metaCommandPool
  , ResetCmdBufFlag (..)
  , metaCommandBuffers

  -- deprecated
  , allocateCommandBuffer
  , runCommandsAsync
  , runCommandsOnce

  , forkWithCmdCap
  , withCmdBufWait
  , withCmdBuf

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

import           Control.Concurrent
import           Control.Exception                        (throw)
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import qualified Foreign.Marshal.Array                    as Foreign
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame

import           Lib.MetaResource
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Sync


newtype ResetCmdBufFlag = ResetCmdBuf Bool deriving Eq


metaCommandPool :: VkDevice -> Word32 -> ResetCmdBufFlag -> MetaResource r VkCommandPool
metaCommandPool dev queueFamIdx (ResetCmdBuf resetFlag) =
  metaResource (liftIO . flip (vkDestroyCommandPool dev) VK_NULL) $
    allocaPeek $ \pPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" (if resetFlag then VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT else 0)
        &* set @"queueFamilyIndex" queueFamIdx
      ) $ \ciPtr -> runVk $ vkCreateCommandPool dev ciPtr VK_NULL pPtr


-- TODO return in dataframe?
metaCommandBuffers :: VkDevice
                   -> VkCommandPool
                   -> Int
                   -> MetaResource r [VkCommandBuffer]
metaCommandBuffers dev cmdPool buffersCount = do
  metaResource
    (\cmdBufs -> liftIO $ Foreign.withArray cmdBufs $ \cbsPtr ->
      vkFreeCommandBuffers dev cmdPool (fromIntegral buffersCount) cbsPtr)
    $ do
    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"commandPool" cmdPool
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandBufferCount" (fromIntegral buffersCount)

    -- allocate a pointer to an array of command buffer handles
    allocaArray buffersCount $ \cbsPtr -> do
      withVkPtr allocInfo $ \aiPtr ->
        runVk $ vkAllocateCommandBuffers dev aiPtr cbsPtr
      peekArray buffersCount cbsPtr


allocateCommandBuffer :: VkDevice
                      -> VkCommandPool
                      -> Program r VkCommandBuffer
allocateCommandBuffer dev cmdPool = do
  bufs <- auto $ metaCommandBuffers dev cmdPool 1
  return $ head bufs


-- | Starts in separate thread, but waits until command buffer has been submitted
--
--   Deferres deallocation of resources until execution by the queue is done.
runCommandsAsync :: VkDevice
                 -> VkCommandPool
                 -> VkQueue
                 -> (VkCommandBuffer -> Program () a)
                 -> Program r a
runCommandsAsync dev cmdPool cmdQueue action = do
  fin <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ runProgram (\res -> tryPutMVar fin res >> return ()) $ do
    -- create command buffer
    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandPool" cmdPool
          &* set @"commandBufferCount" 1
          &* set @"pNext" VK_NULL

    cmdBufs <- allocResource
      (liftIO . flip withDFPtr (vkFreeCommandBuffers dev cmdPool 1))
      (withVkPtr allocInfo $ \aiPtr -> allocaPeekDF $ runVk . vkAllocateCommandBuffers dev aiPtr)
    -- record command buffer
    let cmdbBI = createVk @VkCommandBufferBeginInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
          &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          &* set @"pNext" VK_NULL
        cmdBuf = unScalar cmdBufs
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    result <- action cmdBuf
    runVk $ vkEndCommandBuffer cmdBuf

    -- execute command in a give queue
    let submitInfo = createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* set @"waitSemaphoreCount" 0
          &* set @"pWaitSemaphores"   VK_NULL
          &* set @"pWaitDstStageMask" VK_NULL
          &* set @"commandBufferCount" 1
          &* setDFRef @"pCommandBuffers" cmdBufs
          &* set @"signalSemaphoreCount" 0
          &* set @"pSignalSemaphores" VK_NULL

    fence <- auto $ metaFence dev False
    withVkPtr submitInfo $ \siPtr ->
      runVk $ vkQueueSubmit cmdQueue 1 siPtr fence
    _ <- liftIO $ tryPutMVar fin $ Right result
    fencePtr <- newArrayRes [fence]
    runVk $ vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)
    return result

  result <- liftIO $ takeMVar fin
  case result of
    Left except -> throw except
    Right x     -> return x


runCommandsOnce :: VkDevice
                -> VkCommandPool
                -> VkQueue
                -> (VkCommandBuffer -> Program r a)
                -> Program r a
runCommandsOnce dev cmdPool cmdQueue action = do
    -- create command buffer
    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandPool" cmdPool
          &* set @"commandBufferCount" 1
          &* set @"pNext" VK_NULL

    bracket
      (withVkPtr allocInfo $ \aiPtr -> allocaPeekDF $
          runVk . vkAllocateCommandBuffers dev aiPtr)
      (liftIO . flip withDFPtr (vkFreeCommandBuffers dev cmdPool 1))
      $ \cmdBufs -> do
        -- record command buffer
        let cmdbBI = createVk @VkCommandBufferBeginInfo
              $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
              &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
              &* set @"pNext" VK_NULL
            cmdBuf = unScalar cmdBufs
        withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
        result <- action cmdBuf
        runVk $ vkEndCommandBuffer cmdBuf

        -- execute command in a give queue
        let submitInfo = createVk @VkSubmitInfo
              $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
              &* set @"pNext" VK_NULL
              &* set @"waitSemaphoreCount" 0
              &* set @"pWaitSemaphores"   VK_NULL
              &* set @"pWaitDstStageMask" VK_NULL
              &* set @"commandBufferCount" 1
              &* setDFRef @"pCommandBuffers" cmdBufs
              &* set @"signalSemaphoreCount" 0
              &* set @"pSignalSemaphores" VK_NULL
        locally $ do
          -- TODO maybe add a param if it should wait here, or submit a
          -- different fence that is waited for elsewhere, or whatever
          fence <- auto $ metaFence dev False
          withVkPtr submitInfo $ \siPtr ->
            runVk $ vkQueueSubmit cmdQueue 1 siPtr fence
          fencePtr <- newArrayRes [fence]
          runVk $ vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)
        return result


newtype OneTimeSubmitFlag = OneTimeSubmit Bool deriving Eq

makeCommandBufferBeginInfo :: OneTimeSubmitFlag -> VkCommandBufferBeginInfo
makeCommandBufferBeginInfo (OneTimeSubmit oneTimeFlag) =
  createVk @VkCommandBufferBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
            &* set @"flags" (if oneTimeFlag then VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT else 0)
            &* set @"pNext" VK_NULL


withCmdBufWait :: CommandCapability
               -> ManagedQueue
               -> [(VkSemaphore, VkPipelineStageFlags)]
               -> [VkSemaphore]
               -> (VkCommandBuffer -> Program' a)
               -> Program r a
withCmdBufWait cmdCap queue waitSemsWithStages signalSems action = do
  locally $ do
    managedCmdBuf <- acquireCommandBuffer cmdCap
    let cmdBuf = actualCmdBuf managedCmdBuf
    let cmdbBI = makeCommandBufferBeginInfo (OneTimeSubmit True)
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    result <- action cmdBuf
    runVk $ vkEndCommandBuffer cmdBuf

    qdone <- postNotify queue $ makeSubmitInfo waitSemsWithStages signalSems [cmdBuf]
    -- async return because caller doesn't care about internal cleanup
    waitForQueue qdone
    releaseCommandBuffer managedCmdBuf
    return result
    -- continuation ends because of locally. Auto things from action get deallocated.


withCmdBuf :: CommandCapability
           -> ManagedQueue
           -> [(VkSemaphore, VkPipelineStageFlags)]
           -> [VkSemaphore]
           -> (VkCommandBuffer -> Program () a)
           -> Program r a
withCmdBuf cmdCap queue waitSemsWithStages signalSems action = do
  retBox <- liftIO $ newEmptyMVar
  _ <- forkProg $ run retBox
  liftIO $ takeMVar retBox

  where

  run retBox = do
    managedCmdBuf <- acquireCommandBuffer cmdCap
    let cmdBuf = actualCmdBuf managedCmdBuf
    let cmdbBI = makeCommandBufferBeginInfo (OneTimeSubmit True)
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    result <- action cmdBuf
    runVk $ vkEndCommandBuffer cmdBuf

    qdone <- postNotify queue $ makeSubmitInfo waitSemsWithStages signalSems [cmdBuf]
    -- async return because caller doesn't care about internal cleanup
    liftIO $ putMVar retBox result
    waitForQueue qdone
    releaseCommandBuffer managedCmdBuf
    -- continuation ends because of forkProg. Auto things from action get deallocated.


forkWithCmdCap :: CommandPoolPool
               -> (CommandCapability -> Program () ())
               -> Program r ThreadId
forkWithCmdCap cmdPoolPool action =
  forkProg $ do
    cmdCap <- auto $ metaCommandCapability cmdPoolPool
    action cmdCap


data ManagedCommandBuffer = ManagedCommandBuffer
  { actualCmdBuf    :: VkCommandBuffer
  , cmdPoolOfBuffer :: ManagedCommandPool
  }



data CommandCapability = CommandCapability
  { cmdPoolPool :: CommandPoolPool
  , currentPool :: IORef ManagedCommandPool
  -- TODO need a way to prefetch the next command pool to avoid wait for cache sync between cpu cores
  -- , cmdPools     :: IORef [ManagedCommandPool]
  }

-- | create and destroy this per thread
--
--   touchIORef necessary if reused by another thread!
metaCommandCapability :: CommandPoolPool -> MetaResource r CommandCapability
metaCommandCapability cpp =
  metaResource
  (\CommandCapability{..} -> do
      -- the touch here is only needed if destruction happens in another thread
      touchIORef currentPool
      releaseCommandPool cpp =<< (liftIO $ readIORef currentPool)
  )
  (do
      pool <- acquireCommandPool cpp
      currentPool <- liftIO $ newIORef pool
      return CommandCapability {cmdPoolPool=cpp, ..}
  )


-- | Give opportunity to obtain a fresh command pool. Not thread-safe.
poolSwapOpportunity :: CommandCapability -> Program r ()
poolSwapOpportunity CommandCapability{..} = do
  -- TODO shouldn't swap if pool is mostly unused
  pool <- liftIO $ readIORef currentPool
  releaseCommandPool cmdPoolPool pool
  newPool <- acquireCommandPool cmdPoolPool
  liftIO $ writeIORef currentPool newPool


-- | Acquire a command buffer from the pool, if available. Not thread-safe.
acquireCommandBuffer :: CommandCapability -> Program r ManagedCommandBuffer
acquireCommandBuffer CommandCapability{..} = do
  pool <- liftIO $ readIORef currentPool
  mcpAcquireCommandBuffer pool >>= \case
    Just buffer -> return $ ManagedCommandBuffer buffer pool
    Nothing -> do
      releaseCommandPool cmdPoolPool pool
      newPool <- acquireCommandPool cmdPoolPool
      liftIO $ writeIORef currentPool newPool
      buffer <- fromJust <$> mcpAcquireCommandBuffer newPool
      return $ ManagedCommandBuffer buffer newPool


-- | Give a command buffer back to the cmdBuf pool. Thread-safe.
releaseCommandBuffer :: ManagedCommandBuffer -> Program r ()
releaseCommandBuffer ManagedCommandBuffer{..} = do
  mcpReleaseCommandBuffer cmdPoolOfBuffer actualCmdBuf



data CommandPoolPool = CommandPoolPool
  { mCommandPool :: forall r. MetaResource r ManagedCommandPool
  , freshPools   :: MVar [ManagedCommandPool]
  , usedPoolChan :: Chan (Either (MVar ()) ManagedCommandPool)
  }

initialCmdPoolNum :: Int
initialCmdPoolNum = 2

metaCommandPoolPool :: VkDevice -> Word32 -> MetaResource r CommandPoolPool
metaCommandPoolPool device queueFamIdx =
  metaResource
  (\CommandPoolPool{..} -> do
      -- WARNING destruction doesn't take into account CommandPools that are acquired
      mvar <- liftIO $ newEmptyMVar
      liftIO $ writeChan usedPoolChan (Left mvar)
      liftIO $ takeMVar mvar

      fresh <- liftIO $ takeMVar freshPools
      mapM_ (destroy mCommandPool) fresh
  )
  (do
      let mCommandPool = metaManagedCommandPool device queueFamIdx
      initialCmdPools <- sequence $ replicate initialCmdPoolNum (create mCommandPool)
      usedPoolChan <- liftIO $ newChan
      freshPools <- liftIO $ newMVar initialCmdPools

      _ <- forkProg $ loop $ do
        (liftIO $ readChan usedPoolChan) >>= \case
          Left mvar -> do
            liftIO $ putMVar mvar ()
            return $ AbortLoop ()
          Right used -> do
            -- TODO check if used pool has enough free buffers to be reused without reset
            -- TODO pools that have outstanding buffers can not reset but can potentially still acquire buffers

            -- TODO not in separate thread for now because we would need to keep track of threads for clean shutdown
            -- forkProg $ do
            waitResetableCommandPool used
            resetCommandPool used
            touchCommandPool used -- make sure changes arrive in next thread that uses the pool
            fresh <- liftIO $ takeMVar freshPools
            liftIO $ putMVar freshPools (used:fresh)
            return ContinueLoop

      return CommandPoolPool{..}
  )


acquireCommandPool :: CommandPoolPool -> Program r ManagedCommandPool
acquireCommandPool CommandPoolPool{ mCommandPool, freshPools } = do
  fresh <- liftIO $ takeMVar freshPools
  case fresh of pool:rest -> (liftIO $ putMVar freshPools rest) >> return pool
                [] -> (liftIO $ putMVar freshPools []) >> create mCommandPool

releaseCommandPool :: CommandPoolPool -> ManagedCommandPool -> Program r ()
releaseCommandPool CommandPoolPool{ usedPoolChan } cmdPool = do
  touchCommandPool cmdPool -- make sure changes arrive in next thread that uses the pool
  liftIO $ writeChan usedPoolChan (Right cmdPool)


-- arbitrary value
initialCmdBufNum :: Int
initialCmdBufNum = 1

-- | Designed for one thread to get cmdBufs from the pool and many threads to
--   return them to the pool.
data ManagedCommandPool = ManagedCommandPool
  { cmdPool         :: VkCommandPool
  , acquiredCount   :: IORef Int
  , usedCmdBufs     :: MVar (Int, [VkCommandBuffer])
  , enableNotify    :: IORef Bool
  , notifyResetable :: MVar ()
  , freshCmdBufs    :: IORef [VkCommandBuffer]
  , mCmdBufs        :: forall r. Int -> MetaResource r [VkCommandBuffer]
  , dev             :: VkDevice
  }

-- | to make sure IORef writes from the other thread have arrived
touchCommandPool :: ManagedCommandPool -> Program r ()
touchCommandPool ManagedCommandPool{..} = do
  touchIORef acquiredCount
  -- touchIORef enableNotify <- all writes to this are atomic already
  touchIORef freshCmdBufs



metaManagedCommandPool :: VkDevice -> Word32 -> MetaResource r ManagedCommandPool
metaManagedCommandPool device queueFamIdx =
  let mCmdPool = metaCommandPool device queueFamIdx (ResetCmdBuf False)
  in metaResource
  (\ManagedCommandPool{..} -> do
      -- cmdBufs are freed automatically
      destroy mCmdPool cmdPool
  )
  (do
      cmdPool <- create mCmdPool
      acquiredCount <- liftIO $ newIORef 0
      let mCmdBufs = metaCommandBuffers device cmdPool
      usedCmdBufs <- liftIO $ newMVar (0, [])
      enableNotify <- liftIO $ newIORef False
      notifyResetable <- liftIO $ newEmptyMVar
      initialCmdBufs <- alloc (mCmdBufs initialCmdBufNum)
      freshCmdBufs <- liftIO $ newIORef initialCmdBufs
      return ManagedCommandPool{dev=device, ..}
  )

waitResetableCommandPool :: ManagedCommandPool -> Program r ()
waitResetableCommandPool ManagedCommandPool{..} = do
  acquiredCnt <- liftIO $ readIORef acquiredCount
  (usedCount, used) <- liftIO $ takeMVar usedCmdBufs
  if (acquiredCnt == usedCount)
    then do
      liftIO $ putMVar usedCmdBufs (usedCount, used)
    else do
      liftIO $ tryTakeMVar notifyResetable >> return ()
      liftIO $ atomicWriteIORef enableNotify True
      liftIO $ putMVar usedCmdBufs (usedCount, used)
      liftIO $ takeMVar notifyResetable
      liftIO $ atomicWriteIORef enableNotify False

-- | Moves used command buffers to the fresh cmdBufs list. Not thread-safe.
--
--   Make sure that mcpAcquireCommandBuffer can't be called at the same time.
resetCommandPool :: ManagedCommandPool -> Program r ()
resetCommandPool ManagedCommandPool{..} = do
  (_, cmdBufs) <- liftIO $ takeMVar usedCmdBufs
  liftIO $ writeIORef acquiredCount 0
  liftIO $ putMVar usedCmdBufs (0, [])
  runVk $ vkResetCommandPool dev cmdPool 0
  -- usually freshCmdBufs should be empty or mostly empty here
  liftIO $ modifyIORef' freshCmdBufs (++ cmdBufs)

-- | Acquire a command buffer from the pool, if available. Not thread-safe.
mcpAcquireCommandBuffer :: ManagedCommandPool -> Program r (Maybe VkCommandBuffer)
mcpAcquireCommandBuffer ManagedCommandPool{..} = do
  -- first try freshCmdBufs to avoid thread synchronization
  (liftIO $ readIORef freshCmdBufs) >>= \case
    f:rest -> do
      liftIO $ writeIORef freshCmdBufs rest
      liftIO $ modifyIORef' acquiredCount (+1)
      return (Just f)
    [] -> return Nothing

-- | Acquire a command buffer from the pool, create one if none are available. Not thread-safe.
mcpInsistAcquireCommandBuffer :: ManagedCommandPool -> Program r VkCommandBuffer
mcpInsistAcquireCommandBuffer ManagedCommandPool{..} = do
  -- first try freshCmdBufs to avoid thread synchronization
  cmdBuf <- (liftIO $ readIORef freshCmdBufs) >>= \case
    f:rest -> liftIO $ writeIORef freshCmdBufs rest >> return f
    [] -> do
      new <- head <$> alloc (mCmdBufs 1)
      return new
  liftIO $ modifyIORef' acquiredCount (+1)
  return cmdBuf

-- | Give a command buffer back to the cmdBuf pool. Thread-safe.
mcpReleaseCommandBuffer :: ManagedCommandPool -> VkCommandBuffer -> Program r ()
mcpReleaseCommandBuffer ManagedCommandPool{..} cmdBuf = do
  (usedCount, ucbs) <- liftIO $ takeMVar usedCmdBufs
  let usedCount' = usedCount + 1

  enable <- liftIO $ readIORef enableNotify
  when enable $ do
    -- enabled -> acquiredCount can't change
    acquiredCnt <- liftIO $ readIORef acquiredCount
    when (acquiredCnt == usedCount') $ liftIO $ tryPutMVar notifyResetable () >> return ()

  liftIO $ putMVar usedCmdBufs (usedCount', cmdBuf:ucbs)
