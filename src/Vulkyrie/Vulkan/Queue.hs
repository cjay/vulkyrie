{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Vulkan.Queue
  ( QueueEvent
  , newDoneQueueEvent
  , waitSubmitted
  , waitDone
  , waitDoneTimeout
  , isSubmitted
  , isDone

  -- , WorkUnit(..)

  , metaManagedQueue
  , ManagedQueue

  , post
  , submit
  , submitIfNeeded
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

import           Vulkyrie.MonadIO.Chan
import           Vulkyrie.MonadIO.IORef
import           Vulkyrie.MonadIO.MVar
import           Vulkyrie.MonadIO.Thread
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Sync


-- | Offers a way to get notified on any thread when the queue submission has
--   been submitted and/or is done executing.
--
--   QueueEvents can be in the submitted, done, or not-yet state. Once set to
--   done, they can't be reset. Only ManagedQueue internals can set an event,
--   though you can produce an event that has already been set to done.
--   QueueEvents don't get reused by ManagedQueue.
--
--   Uses Control.Concurrent.Event from concurrent-extra internally.
data QueueEvent
  = QueueEvent
  { submitEvent :: Event
  , doneEvent :: Event
  }


-- | Produces a QueueEvent that has already been set to the done state.
newDoneQueueEvent :: Program r QueueEvent
newDoneQueueEvent = QueueEvent <$> liftIO Event.newSet <*> liftIO Event.newSet

-- | Block until the submission has been submitted to the queue.
waitSubmitted :: QueueEvent -> Program r ()
waitSubmitted QueueEvent { submitEvent } = liftIO $ Event.wait submitEvent

-- | Block until the submission has been executed by the queue.
waitDone :: QueueEvent -> Program r ()
waitDone QueueEvent { doneEvent } = liftIO $ Event.wait doneEvent

-- | Like wait, but with a timeout. A return value of False indicates a timeout
--   occurred.
--
--   The timeout is specified in microseconds.
waitDoneTimeout :: QueueEvent -> Integer -> Program r Bool
waitDoneTimeout QueueEvent { doneEvent } timeout = liftIO $ Event.waitTimeout doneEvent timeout

-- | Checks if the submission has been submitted.
isSubmitted :: QueueEvent -> Program r Bool
isSubmitted QueueEvent { submitEvent } = liftIO $ Event.isSet submitEvent

-- | Checks if the submission has been executed.
isDone :: QueueEvent -> Program r Bool
isDone QueueEvent { doneEvent } = liftIO $ Event.isSet doneEvent


-- data WorkUnit = WorkUnit
--   { wuCmdBufs    :: [VkCommandBuffer]
--   , wuStageFlags :: VkPipelineStageFlags
--   } deriving Eq


-- | Thread-safe interface for VkQueue, with staging and notification.
--
--   Any of the associated functions can be called from any thread at any time.
data ManagedQueue = ManagedQueue
  { requestChan         :: Chan QueueRequest
  , submitInfos         :: IORef (DL.DList VkSubmitInfo)
  , nextEvent           :: IORef QueueEvent
  , fencePool           :: FencePool
  , masterSemaphorePool :: MasterSemaphorePool
  , pumpThread          :: MVar (Maybe ThreadId)
  }

data QueueRequest = Post VkSubmitInfo
                  | SubmitIfNeeded
                  | Submit
                  | SubmitNotify (MVar QueueEvent)
                  | PostNotify VkSubmitInfo (MVar QueueEvent)
                  | Shutdown
                  deriving Eq

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
      nextEvent <- QueueEvent <$> liftIO Event.new <*> liftIO Event.new >>= newIORef
      fencePool <- create mFencePool
      pumpThread <- newMVar Nothing

      let mq = ManagedQueue { masterSemaphorePool=msp, .. }
          submit_ :: Program r ()
          submit_ = do
            fence <- acquireFence fencePool
            fenceResetDone <- asyncProg $ resetFences fencePool
            sIs <- DL.toList <$> readIORef submitInfos
            writeIORef submitInfos mempty
            runVk $ withArrayLen sIs $ \siLen siArr ->
              liftIO $ vkQueueSubmit queue siLen siArr fence
            QueueEvent{ submitEvent, doneEvent } <- readIORef nextEvent
            liftIO $ Event.set submitEvent
            writeIORef nextEvent =<< QueueEvent <$> liftIO Event.new <*> liftIO Event.new
            _ <- forkProg $ do
              fencePtr <- newArrayRes [fence]
              runVk $ vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)
              liftIO $ Event.set doneEvent
              releaseFence fencePool fence
              sems <- concat <$> mapM submitInfoGetWaitSemaphores sIs
              mspReleaseSemaphores msp sems
            -- blocking because acquireFence is not allowed while resetFences is running
            waitProg fenceResetDone

          post_ :: VkSubmitInfo -> Program r ()
          post_ submitInfo = do
            sIs <- readIORef submitInfos
            writeIORef submitInfos (sIs `DL.snoc` submitInfo)

          queueLoop = do
            request <- readChan requestChan
            case request of
              SubmitIfNeeded -> do
                sIs <- readIORef submitInfos
                unless (null sIs) submit_
              Submit -> submit_
              Post submitInfo -> post_ submitInfo
              SubmitNotify eventBox -> do
                event <- readIORef nextEvent
                putMVar eventBox event
                submit_
              PostNotify submitInfo eventBox -> do
                event <- readIORef nextEvent
                putMVar eventBox event
                post_ submitInfo
              Shutdown -> return ()
            when (request /= Shutdown) queueLoop

      -- PERFORMANCE Could write to mutable vector instead of DList to avoid copies,
      -- not sure how pointed-to arrays would be handled.
      -- Also, could keep track of the length instead of using withArrayLen.
      _ <- forkProg queueLoop
      return mq
  )


-- | Stage VkSubmitInfo for submission. Can stage many.
post :: ManagedQueue -> VkSubmitInfo -> Program r ()
post ManagedQueue{ requestChan } submitInfo =
  writeChan requestChan $ Post submitInfo

-- | Only submits something if there are any staged VkSubmitInfos.
submitIfNeeded :: ManagedQueue -> Program r ()
submitIfNeeded ManagedQueue{ requestChan } =
  writeChan requestChan SubmitIfNeeded

-- | Submit with submission-notification.
submit :: ManagedQueue -> Program r ()
submit ManagedQueue{ requestChan } =
  writeChan requestChan Submit

-- | Stage VkSubmitInfo for submission and notify when it was done.
postNotify :: ManagedQueue -> VkSubmitInfo -> Program r QueueEvent
postNotify ManagedQueue{ requestChan } submitInfo = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ PostNotify submitInfo resultBox
  takeMVar resultBox

-- | Submit with done-notification. Always submits, even with empty VkSubmitInfos.
--
--   Can be used to ensure that any previous submissions have been executed.
submitNotify :: ManagedQueue -> Program r QueueEvent
submitNotify ManagedQueue{ requestChan } = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ SubmitNotify resultBox
  takeMVar resultBox

-- | Immediately wait for notification after staging.
--
--   This will wait forever if nothing causes eventual submission.
postWait :: ManagedQueue -> VkSubmitInfo -> Program r ()
postWait mq submitInfo = do
  event <- postNotify mq submitInfo
  waitDone event

-- | Immediately wait for notification after submitting.
submitWait :: ManagedQueue -> Program r ()
submitWait mq = do
  event <- submitNotify mq
  waitDone event


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
