{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Lib.Vulkan.Queue
  ( QueueEvent
  , newSetQueueEvent
  , waitForQueue

  -- , WorkUnit(..)

  , metaManagedQueue
  , ManagedQueue

  , post
  , submit
  , postNotify
  , submitNotify
  , postWait
  , submitWait

  , attachQueuePump
  , removeQueuePump

  , makeSubmitInfo

  -- , CommandThread
  -- , joinCommandThreads
  -- , newCommandThread
  ) where

import           Control.Concurrent.Event       (Event)
import qualified Control.Concurrent.Event       as Event
import           Control.Monad
import qualified Data.DList                     as DL
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Lib.MetaResource
import           Lib.MonadIO.Chan
import           Lib.MonadIO.IORef
import           Lib.MonadIO.MVar
import           Lib.MonadIO.Thread
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Sync


newtype QueueEvent = QueueEvent Event

newSetQueueEvent :: Program r QueueEvent
newSetQueueEvent = QueueEvent <$> liftIO Event.newSet

waitForQueue :: QueueEvent -> Program r ()
waitForQueue (QueueEvent event) = liftIO $ Event.wait event


-- data WorkUnit = WorkUnit
--   { wuCmdBufs    :: [VkCommandBuffer]
--   , wuStageFlags :: VkPipelineStageFlags
--   } deriving Eq


data ManagedQueue = ManagedQueue
  { requestChan         :: Chan QueueRequest
  , submitInfos         :: IORef (DL.DList VkSubmitInfo)
  , nextEvent           :: IORef Event
  , fencePool           :: FencePool
  , masterSemaphorePool :: MasterSemaphorePool
  , pumpThread          :: MVar (Maybe ThreadId)
  }

data QueueRequest = Post VkSubmitInfo
                  | Submit
                  | PostNotify VkSubmitInfo (MVar QueueEvent)
                  | SubmitNotify (MVar QueueEvent)
                  | Shutdown
                  deriving Eq

-- | Thread-safe interface for VkQueue, with staging and notification
metaManagedQueue :: VkDevice -> VkQueue -> MasterSemaphorePool -> MetaResource r ManagedQueue
metaManagedQueue dev queue msp =
  let mFencePool = metaFencePool dev
  in metaResource
  (\ManagedQueue{..} -> do
      -- Shutdown is dangerous: Staged VkSubmitInfos won't get submitted, other
      -- threads might wait eternally for Events.
      writeChan requestChan Shutdown
      destroy mFencePool fencePool
      takeMVar pumpThread >>= mapM_ killThread
  )
  (do
      requestChan <- newChan
      submitInfos <- newIORef mempty
      nextEvent <- liftIO $ Event.new >>= newIORef
      fencePool <- create mFencePool
      pumpThread <- newMVar Nothing

      let mq = ManagedQueue { masterSemaphorePool=msp, .. }
          submit_ :: Program r ()
          submit_ = do
            -- prevent empty submission
            sIs <- readIORef submitInfos
            when (not $ null sIs) (submitNotify_ >> return ())

          -- TODO submitNotify blocks too much, compared to postNotify+submit
          submitNotify_ :: Program r Event
          submitNotify_ = do
            fence <- acquireFence fencePool
            fenceResetDone <- asyncProg $ resetFences fencePool
            sIs <- DL.toList <$> readIORef submitInfos
            runVk $ withArrayLen sIs $ \siLen siArr ->
              liftIO $ vkQueueSubmit queue siLen siArr fence
            writeIORef submitInfos mempty
            event <- readIORef nextEvent
            _ <- forkProg $ do
              fencePtr <- newArrayRes [fence]
              runVk $ vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)
              liftIO $ Event.set event
              releaseFence fencePool fence
              sems <- concat <$> mapM submitInfoGetWaitSemaphores sIs
              mspReleaseSemaphores msp sems
            writeIORef nextEvent =<< liftIO Event.new
            waitProg fenceResetDone
            return event

          post_ :: VkSubmitInfo -> Program r ()
          post_ submitInfo = do
            sIs <- readIORef submitInfos
            writeIORef submitInfos (sIs `DL.snoc` submitInfo)

          queueLoop = do
            request <- readChan requestChan
            case request of
              Submit -> submit_
              SubmitNotify eventBox -> do
                event <- submitNotify_
                putMVar eventBox $ QueueEvent event
              Post submitInfo -> post_ submitInfo
              PostNotify submitInfo eventBox -> do
                post_ submitInfo
                event <- readIORef nextEvent
                putMVar eventBox $ QueueEvent event
              Shutdown -> return ()
            when (request /= Shutdown) queueLoop

      -- PERFORMANCE Could write to mutable vector instead of DList to avoid copies,
      -- not sure how pointed-to arrays would be handled.
      -- Also, could keep track of the length instead of using withArrayLen.
      _ <- forkProg queueLoop
      return mq
  )


-- | Stage VkSubmitInfo for submission.
post :: ManagedQueue -> VkSubmitInfo -> Program r ()
post ManagedQueue{ requestChan } submitInfo = do
  writeChan requestChan $ Post submitInfo

-- | Only submits something if there are any staged VkSubmitInfos.
submit :: ManagedQueue -> Program r ()
submit ManagedQueue{ requestChan } =
  writeChan requestChan Submit

-- | Stage VkSubmitInfo for submission and notify when it was done.
postNotify :: ManagedQueue -> VkSubmitInfo -> Program r QueueEvent
postNotify ManagedQueue{ requestChan } submitInfo = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ PostNotify submitInfo resultBox
  takeMVar resultBox

-- | Submit with notification. Always submits, even with empty VkSubmitInfos.
submitNotify :: ManagedQueue -> Program r QueueEvent
submitNotify ManagedQueue{ requestChan } = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ SubmitNotify resultBox
  takeMVar resultBox

-- | Immediately wait for notification after staging.
postWait :: ManagedQueue -> VkSubmitInfo -> Program r ()
postWait mq submitInfo = do
  event <- postNotify mq submitInfo
  waitForQueue event

-- | Immediately wait for notification after submitting.
submitWait :: ManagedQueue -> Program r ()
submitWait mq = do
  event <- submitNotify mq
  waitForQueue event


-- | Creates a thread for automatic submission every n microseconds.
--
--   Kills previous pump thread if it exists.
attachQueuePump :: ManagedQueue -> Int -> Program r ()
attachQueuePump mq@ManagedQueue{ pumpThread } microSecs = do
  takeMVar pumpThread >>= mapM_ killThread
  tId <- forkProg $ forever $ do
    threadDelay microSecs
    submit mq
  putMVar pumpThread (Just tId)

-- | Kills queue pump thread if it exists.
removeQueuePump :: ManagedQueue -> Program r ()
removeQueuePump ManagedQueue{ pumpThread } =
  do
    takeMVar pumpThread >>= mapM_ killThread
    putMVar pumpThread Nothing



makeSubmitInfo :: [(VkSemaphore, VkPipelineStageFlags)] -- ^ WaitSemaphores and WaitDstStageMask
               -> [VkSemaphore]                         -- ^ SignalSemaphores
               -> [VkCommandBuffer]                     -- ^ CommandBuffers
               -> VkSubmitInfo
makeSubmitInfo waitSemsWithStages signalSems cmdBufs =
  -- correct by construction: both arrays need to have the same length
  let (waitSems, waitDstStageMask) = unzip waitSemsWithStages
  in createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" waitSems
          &* setListRef @"pWaitDstStageMask" waitDstStageMask
          &* setListCountAndRef @"commandBufferCount" @"pCommandBuffers" cmdBufs
          &* setListCountAndRef @"signalSemaphoreCount" @"pSignalSemaphores" signalSems


submitInfoGetWaitSemaphores :: VkSubmitInfo -> Program r [VkSemaphore]
submitInfoGetWaitSemaphores sI =
  let ptr = getField @"pWaitSemaphores" sI
      len = getField @"waitSemaphoreCount" sI
  in peekArray (fromIntegral len) ptr


{-
-- TODO maybe it would be better to handle this with a monad instead of the IORef.
data CommandThread = CommandThread
  { waitSems   :: IORef [(VkSemaphore, VkPipelineStageFlags)]
  }

joinCommandThreads :: [CommandThread] -> Program r CommandThread
joinCommandThreads threads = do
  let refs = map waitSems threads
  allSems <- concat <$> mapM (readIORef) refs
  forM_ refs $ \ref -> writeIORef ref (error "tried accessing invalidated CommandThread")
  newRef <- newIORef allSems
  return $ CommandThread newRef

newCommandThread :: Program r CommandThread
newCommandThread = do
  ref <- newIORef []
  return $ CommandThread ref
-}