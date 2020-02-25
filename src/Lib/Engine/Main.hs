{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
module Lib.Engine.Main
  ( App (..)
  , runVulkanProgram
  ) where


import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import qualified Graphics.UI.GLFW                     as GLFW
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain

import           Lib.GLFW
import           Lib.MonadIO.IORef
import           Lib.MonadIO.MVar
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Resource
import           Lib.Vulkan.Command
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Memory
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Sync

-- | s is the shared app state handle (usually containing constants/IORefs/MVars)
--   w is the window state handle
data App s w
  = App
  { windowName      :: String
  , windowSize      :: (Int, Int)
  , appNewWindow    :: forall r. GLFW.Window -> Program r w
    -- ^ this runs once in the main thread, after GLFW initalization
  , appMainThreadHook :: w -> IO ()
    -- ^ this runs between calls to GLFW.waitEventsTimeout in the main thread
  , appStart        :: forall r. w -> EngineCapability -> Program r s
    -- ^ makes the shared app state handle
  , appNewSwapchain :: forall r a. s -> SwapchainInfo ->
                       Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
  , appRecordFrame  :: forall r. s -> VkCommandBuffer -> VkFramebuffer ->
                       Program r ()
  }

runVulkanProgram :: App s w -> IO ()
runVulkanProgram App{ .. } = runProgram checkStatus $ do
  windowSizeChanged <- newIORef False
  let (windowWidth, windowHeight) = windowSize
  window <- initGLFWWindow windowWidth windowHeight windowName windowSizeChanged
  vulkanInstance <- auto $ createGLFWVulkanInstance (windowName <> "-instance")
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
    gfxQueue <- auto $ metaManagedQueue dev (graphicsQueue queues) msp
    cpp <- auto $ metaCommandPoolPool dev (graphicsFamIdx queues)

    semPool <- auto $ metaSemaphorePool msp
    cmdCap <- auto $ metaCommandCapability cpp
    memPool <- auto $ metaMemoryPool pdev dev
    descriptorPool <- auto $ createDescriptorPool dev 100 -- TODO make dynamic
    -- TODO create permanently mapped reusable staging buffer
    let cap = EngineCapability{ pdev, dev, cmdCap, cmdQueue=gfxQueue, semPool, memPool, descriptorPool }

    logInfo $ "Starting App.."
    appState <- appStart winState cap

    frameIndexRef <- newIORef 0
    renderFinishedSems <- createFrameSemaphores dev
    queueEvents <- sequence $ replicate maxFramesInFlight $ newDoneQueueEvent >>= newIORef
    frameFinishedEvent <- liftIO $ Event.new
    frameOnQueueVars <- sequence $ replicate maxFramesInFlight $ newEmptyMVar

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIndexPtr <- mallocRes

    let beforeSwapchainCreation :: Program r ()
        beforeSwapchainCreation = do
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

    swapchainSlot <- createSwapchainSlot dev
    swapInfoRef <- createSwapchain dev scsd queues vulkanSurface swapchainSlot Nothing >>= newIORef

    -- Those are only needed if commands need to happen before first draw, I think:
    -- attachQueuePump gfxQueue 16666
    -- removeQueuePump gfxQueue

    nextSems <- newMVar []

    -- The code below re-runs when the swapchain was re-created
    asyncRedo $ \redoWithNewSwapchain -> do
      logInfo "New thread: Creating things that depend on the swapchain.."
      -- need this for delayed destruction of the old swapchain if it gets replaced
      oldSwapchainSlot <- createSwapchainSlot dev
      swapInfo <- readIORef swapInfoRef

      (framebuffers, nextAppSems) <- appNewSwapchain appState swapInfo
      sems <- takeMVar nextSems
      putMVar nextSems (sems <> nextAppSems)

      -- part of dumb fps counter
      frameCount :: IORef Int <- newIORef 0
      currentSec :: IORef Int <- newIORef 0

      shouldExit <- glfwMainLoop window $ do
        let rdata = RenderData
              { swapInfo
              , queues
              , imgIndexPtr
              , frameIndexRef
              , renderFinishedSems
              , nextSems
              , frameFinishedEvent
              , queueEvents
              , frameOnQueueVars
              -- , memories           = transObjMems
              -- , memoryMutator      = \mem -> do
              --     t <- updateTransObj (swapExtent swapInfo)
              --     uboUpdate dev transObjSize mem t
              -- , descrSetMutator    = updateDescrSet dev descrTextureInfos
              , recCmdBuffer       = appRecordFrame appState
              -- , frameDescrSets
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
          if floor seconds /= cur then do
            count <- readIORef frameCount
            when (cur /= 0) $ print count
            writeIORef currentSec (floor seconds)
            writeIORef frameCount 0
          else do
            modifyIORef' frameCount $ \c -> c + 1

        sizeChanged <- readIORef windowSizeChanged
        when sizeChanged $ logInfo "Have got a windowSizeCallback from GLFW"
        if needRecreation || sizeChanged then do
          beforeSwapchainCreation
          logInfo "Recreating swapchain.."
          newScsd <- querySwapchainSupport pdev vulkanSurface
          newSwapInfo <- createSwapchain dev newScsd queues vulkanSurface swapchainSlot (Just oldSwapchainSlot)
          atomicWriteIORef swapInfoRef newSwapInfo
          redoWithNewSwapchain
          return $ AbortLoop ()
        else return ContinueLoop
      -- after glfwMainLoop exits, we need to wait for the frame to finish before deallocating things
      if shouldExit
      then runVk $ vkDeviceWaitIdle dev
      -- Using Event here properly deals with multiple waiting threads, in
      -- contrast to using plain MVars. The wait here is to make sure it's safe
      -- to destroy the old swapchain (via oldSwapchainSlot). It doesn't affect
      -- rendering, because a new thread has been already started for the new
      -- swapchain via asyncRedo.
      else liftIO $ sequence_ $ replicate maxFramesInFlight $ Event.wait frameFinishedEvent
      -- logInfo "Finished waiting after main loop termination before deallocating."
  return ()
