{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Vulkyrie.Engine.Main
  ( App (..)
  , RenderFun
  , EngineFlag (..)
  , runVulkanProgram
  ) where


import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import qualified Graphics.UI.GLFW                     as GLFW
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           UnliftIO.Chan
import           UnliftIO.Exception
import           UnliftIO.IORef
import           UnliftIO.MVar

import           Vulkyrie.Engine.Draw
import           Vulkyrie.GLFW
import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Utils
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Descriptor
import           Vulkyrie.Vulkan.Device as Device
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Memory
import           Vulkyrie.Vulkan.Presentation
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Sync


data EngineFlag = Validation deriving (Eq, Ord, Show)

-- | s is the shared app state handle (usually containing constants/IORefs/MVars)
--   w is the window state handle
data App s w
  = App
  { windowName      :: String
  , windowSize      :: (Int, Int)
  , windowFullscreen :: Bool
  , flags           :: [EngineFlag]
  , syncMode        :: SyncMode
  , maxFramesInFlight :: Int
    -- ^ allowed number of unfinished submitted frames on the graphics queue
  , appNewWindow    :: GLFW.Window -> Resource w
    -- ^ this runs once in the main thread, after GLFW initalization
  , appMainThreadHook :: w -> IO ()
    -- ^ this runs between calls to GLFW.waitEventsTimeout in the main thread
  , appStart        :: w -> EngineCapability -> Resource s
    -- ^ makes the shared app state handle
  , appNewSwapchain :: forall a. s -> SwapchainInfo ->
                       Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
  , appRenderFrame  :: s -> RenderFun
  }

runVulkanProgram :: App s w -> IO ()
runVulkanProgram App{ .. } = runProgram $ runResource $ do
  windowSizeChanged <- newIORef False
  let (windowWidth, windowHeight) = windowSize
  window <- initGLFWWindow windowWidth windowHeight windowName windowFullscreen windowSizeChanged
  let enabledLayers = ["VK_LAYER_KHRONOS_validation" | Validation `elem` flags ]
  vulkanInstance <- auto $ createGLFWVulkanInstance (windowName <> "-instance") enabledLayers
  vulkanSurface <- auto $ createSurface vulkanInstance window
  logInfo $ "Createad surface: " <> showt vulkanSurface

  winState <- appNewWindow window

  liftProg $ glfwWaitEventsMeanwhile (appMainThreadHook winState) $ runResource $ do
    (_, pdev) <- liftProg $ pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " <> showt pdev

    (dev, queues) <- createGraphicsDevice pdev vulkanSurface
    logInfo $ "Createad device: " <> showt dev
    logInfo $ "Createad queues: " <> showt queues

    msp <- auto $ metaMasterSemaphorePool dev
    presentQueue <- ManagedPresentQueue (Device.presentQueue queues) <$> newMVar ()
    gfxQueue <- auto $ metaManagedQueue dev (graphicsQueue queues) msp
    cmdPoolPool <- auto $ metaCommandPoolPool dev (graphicsFamIdx queues)

    semPool <- auto $ metaSemaphorePool msp
    cmdCap <- auto $ metaCommandCapability cmdPoolPool
    memPool <- auto $ metaMemoryPool pdev dev
    descriptorPool <- auto $ createDescriptorPool dev 100 -- TODO make dynamic
    -- TODO create permanently mapped reusable staging buffer
    let cap = EngineCapability
          { pdev, dev, queues, cmdCap, cmdQueue=gfxQueue, queueFam=graphicsFamIdx queues,
          semPool, memPool, descriptorPool }

    logInfo "Starting App.."
    appState <- appStart winState cap

    renderFinishedSems <- newChan
    writeList2Chan renderFinishedSems =<< replicateM maxFramesInFlight (auto $ metaSemaphore dev)
    frameFinishedEvent <- liftIO Event.new

    let beforeSwapchainCreation :: Program ()
        beforeSwapchainCreation =
          -- wait as long as window has width=0 and height=0
          -- commented out because this only works in the main thread:
          -- glfwWaitMinimized window

          -- If a window size change did happen, it will be respected by (re-)creating
          -- the swapchain below, no matter if it was signalled via exception or
          -- the IORef, so reset the IORef now:
          atomicWriteIORef windowSizeChanged False

    -- creating first swapchain before loop
    liftProg beforeSwapchainCreation
    scsd <- liftProg $ querySwapchainSupport pdev vulkanSurface

    nextSwapchainSlot <- createSwapchainSlot dev
    (firstSwapchain, firstSwapInfo) <- liftProg $ createSwapchain dev scsd queues vulkanSurface syncMode Nothing
    putMVar nextSwapchainSlot (Just firstSwapchain)
    swapInfoRef <- newIORef firstSwapInfo
    swapImgTokens <- liftIO . newNatTokenVar $ swapMaxAcquired firstSwapInfo

    -- Those are only needed if commands need to happen before first draw, I think:
    -- attachQueuePump gfxQueue 16666
    -- removeQueuePump gfxQueue

    nextSems <- newMVar []

    -- The code below re-runs when the swapchain was re-created
    liftProg $ asyncRedo $ \redoWithNewSwapchain -> runResource $ do
      logInfo "New thread: Creating things that depend on the swapchain.."
      -- need this for delayed destruction of the old swapchain if it gets replaced
      swapchainSlot <- createSwapchainSlot dev
      putMVar swapchainSlot =<< takeMVar nextSwapchainSlot
      swapInfo <- readIORef swapInfoRef
      -- TODO waiting for vulkan spec clarification: acquiredness of images after swapchain replacement
      liftIO $ changeNumTokens swapImgTokens (swapMaxAcquired swapInfo)

      (framebuffers, nextAppSems) <- appNewSwapchain appState swapInfo
      sems <- takeMVar nextSems
      putMVar nextSems (sems <> nextAppSems)

      -- part of dumb fps counter
      frameCount :: IORef Int <- newIORef 0
      currentSec :: IORef Int <- newIORef 0

      shouldExit <- liftProg $ glfwMainLoop window $ do
        let rdata = RenderData
              { swapchainVar = swapchainSlot
              , presentQueue
              , swapImgTokens
              , renderFinishedSems
              , nextSems
              , frameFinishedEvent
              , renderFun = appRenderFrame appState
              , framebuffers
              }
        needRecreation <- drawFrame cap rdata `catch` ( \err@(VulkanException ecode) ->
          case ecode of
            VK_ERROR_OUT_OF_DATE_KHR -> do
              logInfo "Have got a VK_ERROR_OUT_OF_DATE_KHR error"
              return True
            _ -> throwIO err
          )

        -- part of dumb fps counter
        seconds <- getTime
        liftIO $ do
          cur <- readIORef currentSec
          if floor seconds /= cur
            then do
              count <- readIORef frameCount
              when (cur /= 0) $ print count
              writeIORef currentSec (floor seconds)
              writeIORef frameCount 0
            else
              modifyIORef' frameCount $ \c -> c + 1

        sizeChanged <- readIORef windowSizeChanged
        when sizeChanged $ logInfo "Have got a windowSizeCallback from GLFW"
        if needRecreation || sizeChanged
          then do
            beforeSwapchainCreation
            logInfo "Recreating swapchain.."
            swapchain <- takeMVar swapchainSlot
            -- This query should happen as close to createSwapchain as possible to
            -- get the latest changes to the window size.
            newScsd <- querySwapchainSupport pdev vulkanSurface
            (newSwapchain, newSwapInfo) <- createSwapchain dev newScsd queues vulkanSurface syncMode swapchain
            putMVar swapchainSlot swapchain
            putMVar nextSwapchainSlot (Just newSwapchain)
            atomicWriteIORef swapInfoRef newSwapInfo
            redoWithNewSwapchain
            return $ AbortLoop ()
          else return ContinueLoop
      -- after glfwMainLoop exits, we need to wait for the frame to finish before deallocating things
      liftProg $ waitCheckpoint gfxQueue -- staged stuff in ManagedQueue needs to be submitted
      if shouldExit
        then runVk $ vkDeviceWaitIdle dev
        -- Using Event here properly deals with multiple waiting threads, in
        -- contrast to using plain MVars. The wait here is to make sure it's safe
        -- to destroy the old swapchain (via swapchainSlot). It doesn't affect
        -- rendering, because a new thread has been already started for the new
        -- swapchain via asyncRedo.
        else liftIO $ replicateM_ (length $ swapImgs swapInfo) $ Event.wait frameFinishedEvent
        -- TODO this could wait forever if the swapchain was renewed shortly before exit
      -- logInfo "Finished waiting after main loop termination before deallocating."
  return ()
