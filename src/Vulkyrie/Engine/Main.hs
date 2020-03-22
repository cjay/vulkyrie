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

import           Vulkyrie.Engine.Draw
import           Vulkyrie.GLFW
import           Vulkyrie.MonadIO.Chan
import           Vulkyrie.MonadIO.IORef
import           Vulkyrie.MonadIO.MVar
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
  , flags           :: [EngineFlag]
  , syncMode        :: SyncMode
  , maxFramesInFlight :: Int
    -- ^ allowed number of unfinished submitted frames on the graphics queue
  , appNewWindow    :: forall r. GLFW.Window -> Program r w
    -- ^ this runs once in the main thread, after GLFW initalization
  , appMainThreadHook :: w -> IO ()
    -- ^ this runs between calls to GLFW.waitEventsTimeout in the main thread
  , appStart        :: forall r. w -> EngineCapability -> Program r s
    -- ^ makes the shared app state handle
  , appNewSwapchain :: forall r a. s -> SwapchainInfo ->
                       Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
  , appRenderFrame  :: s -> RenderFun
  }

runVulkanProgram :: App s w -> IO ()
runVulkanProgram App{ .. } = runProgram checkStatus $ do
  windowSizeChanged <- newIORef False
  let (windowWidth, windowHeight) = windowSize
  window <- initGLFWWindow windowWidth windowHeight windowName windowSizeChanged
  let enabledLayers = ["VK_LAYER_LUNARG_standard_validation" | Validation `elem` flags ]
  vulkanInstance <- auto $ createGLFWVulkanInstance (windowName <> "-instance") enabledLayers
  vulkanSurface <- auto $ createSurface vulkanInstance window
  logInfo $ "Createad surface: " ++ show vulkanSurface

  winState <- appNewWindow window

  glfwWaitEventsMeanwhile (appMainThreadHook winState) $ do
    (_, pdev) <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " ++ show pdev

    (dev, queues) <- createGraphicsDevice pdev vulkanSurface
    logInfo $ "Createad device: " ++ show dev
    logInfo $ "Createad queues: " ++ show queues

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
          { pdev, dev, cmdCap, cmdQueue=gfxQueue, semPool, memPool, descriptorPool }

    logInfo "Starting App.."
    appState <- appStart winState cap

    renderFinishedSems <- newChan
    writeList2Chan renderFinishedSems =<< replicateM maxFramesInFlight (auto $ metaSemaphore dev)
    frameFinishedEvent <- liftIO Event.new

    let beforeSwapchainCreation :: Program r ()
        beforeSwapchainCreation =
          -- wait as long as window has width=0 and height=0
          -- commented out because this only works in the main thread:
          -- glfwWaitMinimized window

          -- If a window size change did happen, it will be respected by (re-)creating
          -- the swapchain below, no matter if it was signalled via exception or
          -- the IORef, so reset the IORef now:
          atomicWriteIORef windowSizeChanged False

    -- creating first swapchain before loop
    beforeSwapchainCreation
    scsd <- querySwapchainSupport pdev vulkanSurface

    nextSwapchainSlot <- createSwapchainSlot dev
    firstSwapInfo <- createSwapchain dev scsd queues vulkanSurface syncMode Nothing
    putMVar nextSwapchainSlot (swapchain firstSwapInfo)
    swapInfoRef <- newIORef firstSwapInfo
    -- TODO The -1 is a workaround. Without it, validation layer complains.
    -- Not sure if bug in validaiton/MoltenVK. Complaint:
    -- "Application has already previously acquired 2 images from swapchain. Only 2
    -- are available to be acquired using a timeout of UINT64_MAX (given the
    -- swapchain has 3, and VkSurfaceCapabilitiesKHR::minImageCount is 2)."
    let numSwapImgTokens swapInfo = swapMaxAcquired swapInfo - 1
        ntok = numSwapImgTokens firstSwapInfo
    swapImgTokens <- liftIO $ newNatTokenVar ntok
    logInfo $ "number of swap image tokens is " ++ show ntok

    -- Those are only needed if commands need to happen before first draw, I think:
    -- attachQueuePump gfxQueue 16666
    -- removeQueuePump gfxQueue

    nextSems <- newMVar []

    -- The code below re-runs when the swapchain was re-created
    asyncRedo $ \redoWithNewSwapchain -> do
      logInfo "New thread: Creating things that depend on the swapchain.."
      -- need this for delayed destruction of the old swapchain if it gets replaced
      swapchainSlot <- createSwapchainSlot dev
      putMVar swapchainSlot =<< takeMVar nextSwapchainSlot
      swapInfo <- readIORef swapInfoRef
      liftIO $ changeNumTokens swapImgTokens (numSwapImgTokens swapInfo)

      (framebuffers, nextAppSems) <- appNewSwapchain appState swapInfo
      sems <- takeMVar nextSems
      putMVar nextSems (sems <> nextAppSems)

      -- part of dumb fps counter
      frameCount :: IORef Int <- newIORef 0
      currentSec :: IORef Int <- newIORef 0

      shouldExit <- glfwMainLoop window $ do
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
        needRecreation <- drawFrame cap rdata `catchError` ( \err@(VulkanException ecode _) ->
          case ecode of
            Just VK_ERROR_OUT_OF_DATE_KHR -> do
              logInfo "Have got a VK_ERROR_OUT_OF_DATE_KHR error"
              return True
            _ -> throwError err
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
            newSwapInfo <- createSwapchain dev newScsd queues vulkanSurface syncMode (Just swapchain)
            putMVar swapchainSlot swapchain
            putMVar nextSwapchainSlot (Vulkyrie.Vulkan.Presentation.swapchain newSwapInfo)
            atomicWriteIORef swapInfoRef newSwapInfo
            redoWithNewSwapchain
            return $ AbortLoop ()
          else return ContinueLoop
      -- after glfwMainLoop exits, we need to wait for the frame to finish before deallocating things
      waitCheckpoint gfxQueue -- staged stuff in ManagedQueue needs to be submitted
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
