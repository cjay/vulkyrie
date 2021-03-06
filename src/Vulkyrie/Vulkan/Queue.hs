{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Vulkan.Queue
  ( ManagedPresentQueue (..)
  , present

  , QueueEvent
  , newDoneQueueEvent
  , waitSubmitted
  , waitDone
  , waitDoneTimeout
  , isSubmitted
  , isDone

  , managedQueue
  , ManagedQueue

  , post
  , postSubmitPresent
  , submit
  , submitPresent
  , submitIfNeeded
  , postNotify
  , submitNotify
  , postSubmitNotify
  , postWait
  , submitWait
  , waitCheckpoint

  , attachQueuePump
  , removeQueuePump

  , makeSubmitInfo
  ) where

import           Control.Applicative
import           Control.Concurrent.Event       (Event)
import qualified Control.Concurrent.Event       as Event
import           Control.Monad
import qualified Data.DList                     as DL
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           UnliftIO.Chan
import           UnliftIO.Exception
import qualified UnliftIO.Foreign as Foreign
import           UnliftIO.IORef
import           UnliftIO.MVar
import           UnliftIO.Concurrent

import           Vulkyrie.Concurrent
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Sync

data ManagedPresentQueue =
  ManagedPresentQueue
  { presentQueue :: VkQueue
  , presentQueueMutex :: MVar ()
    -- ^ needed because there is no single management thread for the queue
  }

present :: ManagedPresentQueue -> [(MVar VkSwapchainKHR, Word32)] -> [VkSemaphore] -> Prog r VkResult
present ManagedPresentQueue{..} images waitSems = do
  let (swapchainVars, imageIndices) = unzip images
  bracket
    (mapM takeMVar swapchainVars)
    (\swapchains -> sequence_ $ putMVar <$> ZipList swapchainVars <*> ZipList swapchains)
    (\swapchains -> do
      let presentInfo = createVk @VkPresentInfoKHR
            $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
            &* set @"pNext" VK_NULL
            &* setListRef @"pImageIndices" imageIndices
            &* setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" waitSems
            &* setListCountAndRef @"swapchainCount" @"pSwapchains" swapchains
      -- not doing runVk here to let the postPresentAction handle the swapchain status
      withVkPtr presentInfo $ \ptr -> liftIO
        (vkQueuePresentKHR presentQueue ptr)
    )


-- | Offers a way to get notified on any thread when the queue submission has
--   been submitted to the actual VkQueue and/or is done executing.
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
newDoneQueueEvent :: Prog r QueueEvent
newDoneQueueEvent = QueueEvent <$> liftIO Event.newSet <*> liftIO Event.newSet

-- | Block until the submission has been submitted to the queue.
waitSubmitted :: QueueEvent -> Prog r ()
waitSubmitted QueueEvent { submitEvent } = liftIO $ Event.wait submitEvent

-- | Block until the submission has been executed by the queue.
waitDone :: QueueEvent -> Prog r ()
waitDone QueueEvent { doneEvent } = liftIO $ Event.wait doneEvent

-- | Like wait, but with a timeout. A return value of False indicates a timeout
--   occurred.
--
--   The timeout is specified in microseconds.
waitDoneTimeout :: QueueEvent -> Integer -> Prog r Bool
waitDoneTimeout QueueEvent { doneEvent } timeout = liftIO $ Event.waitTimeout doneEvent timeout

-- | Checks if the submission has been submitted.
isSubmitted :: QueueEvent -> Prog r Bool
isSubmitted QueueEvent { submitEvent } = liftIO $ Event.isSet submitEvent

-- | Checks if the submission has been executed.
isDone :: QueueEvent -> Prog r Bool
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
    -- ^ channel that feeds instructions to the management thread
  , submitInfos         :: IORef (DL.DList VkSubmitInfo)
    -- ^ submit infos staged for submission via post
  , nextEvent           :: IORef QueueEvent
    -- ^ QueueEvent for the next submission
  , fencePool           :: FencePool
    -- ^ used for fences that signal that the VkQueue submission is done
  , masterSemaphorePool :: MasterSemaphorePool
    -- ^ used to release wait-semaphores of the submit infos
  , pumpThread          :: MVar (Maybe ThreadId)
    -- ^ handle for a optional thread that regularly causes submission of staged
    -- submit infos
  , pumpThreadOwner     :: ThreadOwner
  }

data QueueRequest = Shutdown
                  | Transaction [Instruction]

data Instruction = Post VkSubmitInfo
                 | SubmitIfNeeded
                 | Submit
                 | Present ManagedPresentQueue (forall r. Prog r VkResult) (forall r. VkResult -> Prog r ())
                 | Notify (MVar QueueEvent)
                 | Checkpoint Event

managedQueue :: VkDevice -> VkQueue -> MasterSemaphorePool -> Resource ManagedQueue
managedQueue dev queue msp = Resource $ do
  requestChan <- newChan
  submitInfos <- newIORef mempty
  nextEvent <- QueueEvent <$> liftIO Event.new <*> liftIO Event.new >>= newIORef
  fencePool <- auto $ metaFencePool dev
  pumpThreadOwner <- auto threadOwner
  pumpThread <- newMVar Nothing

  presentThreadOwner <- auto threadOwner
  releaseThreadOwner <- auto threadOwner

  let mq = ManagedQueue { masterSemaphorePool=msp, .. }
      submit_ :: Prog r ()
      submit_ = region $ do
        fence <- acquireFence fencePool
        fenceResetDone <- auto $ asyncRes $ resetFences fencePool
        sIs <- DL.toList <$> readIORef submitInfos
        writeIORef submitInfos mempty
        runVk $ withArrayLen sIs $ \siLen siArr ->
          liftIO $ vkQueueSubmit queue siLen siArr fence
        QueueEvent{ submitEvent, doneEvent } <- readIORef nextEvent
        liftIO $ Event.set submitEvent
        writeIORef nextEvent =<< QueueEvent <$> liftIO Event.new <*> liftIO Event.new
        void $ ownedThread releaseThreadOwner $ do
          runVk $ Foreign.withArrayLen [fence] $ \len fencePtr ->
            vkWaitForFences dev (fromIntegral len) fencePtr VK_TRUE (maxBound :: Word64)
          liftIO $ Event.set doneEvent
          releaseFence fencePool fence
          sems <- concat <$> mapM submitInfoGetWaitSemaphores sIs
          mspReleaseSemaphores msp sems
        -- blocking because acquireFence is not allowed while resetFences is running
        wait fenceResetDone

      post_ :: VkSubmitInfo -> Prog r ()
      post_ submitInfo = do
        sIs <- readIORef submitInfos
        writeIORef submitInfos (sIs `DL.snoc` submitInfo)

      queueLoop = do
        -- TODO dependencies on submission in another queue (for semaphores)
        -- TODO reordering when a dependency blocks
        request <- readChan requestChan
        case request of
          Transaction instructions -> do
            forM_ instructions $ \case
              SubmitIfNeeded -> do
                sIs <- readIORef submitInfos
                unless (null sIs) submit_
              Submit -> submit_
              Post submitInfo -> post_ submitInfo
              Notify eventBox -> do
                event <- readIORef nextEvent
                putMVar eventBox event
              Checkpoint event -> liftIO $ Event.set event
              Present (ManagedPresentQueue presentQueue mutex) presentAction postPresentAction ->
                if presentQueue == queue
                  then presentAction >>= postPresentAction
                  else void $ ownedThread presentThreadOwner $ do
                    result <- withMVar mutex $ const presentAction
                    postPresentAction result
            queueLoop
          Shutdown -> return ()

  -- PERFORMANCE Could write to mutable vector instead of DList to avoid copies,
  -- not sure how pointed-to arrays would be handled.
  -- Also, could keep track of the length instead of using withArrayLen.
  void $ auto $ threadRes queueLoop
  -- TODO: Shutdown is dangerous: Staged VkSubmitInfos won't get submitted, other
  -- threads might wait eternally for Events.

  -- Not sure if the following has any benefits instead of just letting the thread be killed
  -- Would need to wait after this, otherwise the thread just gets killed right away.
  -- onDestroy $ writeChan requestChan Shutdown

  return mq


-- | Stage VkSubmitInfo for submission. Can stage many.
post :: ManagedQueue -> VkSubmitInfo -> Prog r ()
post ManagedQueue{ requestChan } submitInfo =
  writeChan requestChan $ Transaction [Post submitInfo]

postSubmitPresent :: ManagedQueue -> VkSubmitInfo -> ManagedPresentQueue -> (forall r. Prog r VkResult) -> (forall r. VkResult -> Prog r ()) -> Prog r' ()
postSubmitPresent ManagedQueue{ requestChan } submitInfo presentQueue presentAction postPresentAction =
  writeChan requestChan $ Transaction
    [Post submitInfo, Submit, Present presentQueue presentAction postPresentAction]

-- | Only submits something if there are any staged VkSubmitInfos.
submitIfNeeded :: ManagedQueue -> Prog r ()
submitIfNeeded ManagedQueue{ requestChan } =
  writeChan requestChan $ Transaction [SubmitIfNeeded]

-- | Submit with submission-notification.
submit :: ManagedQueue -> Prog r ()
submit ManagedQueue{ requestChan } =
  writeChan requestChan $ Transaction [Submit]

submitPresent :: ManagedQueue -> ManagedPresentQueue -> (forall r. Prog r VkResult) -> (forall r. VkResult -> Prog r ()) -> Prog r' ()
submitPresent ManagedQueue{ requestChan } presentQueue presentAction postPresentAction =
  writeChan requestChan $ Transaction
    [Submit, Present presentQueue presentAction postPresentAction]

-- | Stage VkSubmitInfo for submission and notify when it was done.
postNotify :: ManagedQueue -> VkSubmitInfo -> Prog r QueueEvent
postNotify ManagedQueue{ requestChan } submitInfo = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ Transaction [Notify resultBox, Post submitInfo]
  takeMVar resultBox

-- | Submit with done-notification. Always submits, even with empty VkSubmitInfos.
--
--   Can be used to ensure that any previous submissions have been executed.
submitNotify :: ManagedQueue -> Prog r QueueEvent
submitNotify ManagedQueue{ requestChan } = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ Transaction [Notify resultBox, Submit]
  takeMVar resultBox

-- | Stage VkSubmitInfo for submission and notify when it was done.
postSubmitNotify :: ManagedQueue -> VkSubmitInfo -> Prog r QueueEvent
postSubmitNotify ManagedQueue{ requestChan } submitInfo = do
  resultBox <- newEmptyMVar
  writeChan requestChan $ Transaction [Notify resultBox, Post submitInfo, Submit]
  takeMVar resultBox

-- | Immediately wait for notification after staging.
--
--   This will wait forever if nothing causes eventual submission.
postWait :: ManagedQueue -> VkSubmitInfo -> Prog r ()
postWait mq submitInfo = do
  event <- postNotify mq submitInfo
  waitDone event

-- | Immediately wait for notification after submitting.
submitWait :: ManagedQueue -> Prog r ()
submitWait mq = do
  event <- submitNotify mq
  waitDone event

waitCheckpoint :: ManagedQueue -> Prog r ()
waitCheckpoint ManagedQueue{ requestChan } = do
  event <- liftIO Event.new
  writeChan requestChan $ Transaction [Checkpoint event]
  liftIO $ Event.wait event

-- | Creates a thread for automatic submission every n microseconds.
--
--   Kills previous pump thread if it exists.
attachQueuePump :: ManagedQueue -> Int -> Prog r ()
attachQueuePump mq@ManagedQueue{ pumpThread, pumpThreadOwner } microSecs = do
  takeMVar pumpThread >>= mapM_ killThread
  tId <- ownedThread pumpThreadOwner $ forever $ do
    threadDelay microSecs
    submit mq
  putMVar pumpThread (Just tId)

-- | Kills queue pump thread if it exists.
removeQueuePump :: ManagedQueue -> Prog r ()
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


submitInfoGetWaitSemaphores :: VkSubmitInfo -> Prog r [VkSemaphore]
submitInfoGetWaitSemaphores =
  liftIO . getListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores"


{-
-- TODO maybe it would be better to handle this with a monad instead of the IORef.
data CommandThread = CommandThread
  { waitSems   :: IORef [(VkSemaphore, VkPipelineStageFlags)]
  }

joinCommandThreads :: [CommandThread] -> Prog r CommandThread
joinCommandThreads threads = do
  let refs = map waitSems threads
  allSems <- concat <$> mapM (readIORef) refs
  forM_ refs $ \ref -> writeIORef ref (error "tried accessing invalidated CommandThread")
  newRef <- newIORef allSems
  return $ CommandThread newRef

newCommandThread :: Prog r CommandThread
newCommandThread = do
  ref <- newIORef []
  return $ CommandThread ref
-}
