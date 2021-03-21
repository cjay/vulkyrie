{-# LANGUAGE Strict #-}
module Vulkyrie.GLFW
    ( createGLFWVulkanInstance
    , initGLFWWindow
    , glfwMainLoop
    , glfwWaitMinimized
    , glfwWaitEventsMeanwhile
    , getTime
    ) where

import           Control.Applicative
import           Control.Monad       (forever, unless, when)
import           Control.Monad.Trans.Maybe
import           Graphics.UI.GLFW    (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW    as GLFW
import           Graphics.Vulkan
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Instance


initGLFWWindow :: Int -- ^ Window width. Ignored if fullscreen.
               -> Int -- ^ Window height. Ignored if fullscreen.
               -> String -- ^ Window name
               -> Bool -- ^ fullscreen
               -> IORef Bool -- ^ Window size change signalling
               -> Resource GLFW.Window
initGLFWWindow winWidth winHeight name fullscreen windowSizeChanged = do
    -- even if something bad happens, we need to terminate GLFW
    allocResource
      (const $ liftIO GLFW.terminate >> logInfo "Terminated GLFW.")
      (liftIO GLFW.init >>= flip unless (throwString "Failed to initialize GLFW."))

    liftIO GLFW.getVersionString >>= mapM_ (logInfo . ("GLFW version: " <>))

    liftIO GLFW.vulkanSupported >>= flip unless
      (throwString "GLFW reports that vulkan is not supported!")

    liftIO . GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    liftIO . GLFW.windowHint $ WindowHint'Resizable True

    allocResource
      ( \window -> do
          liftIO (GLFW.destroyWindow window)
          logDebug "Closed GLFW window."
      )
      ( do
          mw <- liftIO . runMaybeT $
            ( if fullscreen then do
                mon <- MaybeT GLFW.getPrimaryMonitor
                mode <- MaybeT $ GLFW.getVideoMode mon
                let (w, h) = (GLFW.videoModeWidth mode, GLFW.videoModeHeight mode)
                MaybeT $ GLFW.createWindow w h name (Just mon) Nothing
              else MaybeT $ return Nothing
            ) <|> MaybeT (GLFW.createWindow winWidth winHeight name Nothing Nothing)

          case mw of
            Nothing -> throwString "Failed to initialize GLFW window."
            Just window  -> do
              logDebug "Initialized GLFW window."
              liftIO $ GLFW.setWindowSizeCallback window $
                Just (\_ _ _ -> atomicWriteIORef windowSizeChanged True)
              return window
      )

-- | Repeats until WindowShouldClose flag is set. Returns true if program should exit.
--   Local resource scope.
glfwMainLoop :: GLFW.Window -> Program (LoopControl ()) -> Program Bool
glfwMainLoop w action = go
  where
    go = do
      should <- liftIO $ GLFW.windowShouldClose w
      if not should
        then do
          status <- action
          if status == ContinueLoop then go else return False
        else return True


-- | Runs GLFW event handling in the main thread continuously.
--
--   Waits repeatedly with 1 second timeout to allow exceptions to be handled
--   without events happening. If waiting without timeout, the waitEvents
--   function would need to be marked interruptible in the GLFW binding.
glfwWaitEventsMeanwhile :: IO () -> Program () -> Program ()
glfwWaitEventsMeanwhile mainThreadHook =
  occupyThreadAndFork
    (liftIO $ forever $ GLFW.waitEventsTimeout 1 >> mainThreadHook)


glfwWaitMinimized :: GLFW.Window -> Program ()
glfwWaitMinimized win = liftIO go where
  go = do
    (x,y) <- GLFW.getFramebufferSize win
    GLFW.waitEvents
    when (x == 0 && y == 0) go

createGLFWVulkanInstance :: String -> [String] -> Resource VkInstance
createGLFWVulkanInstance progName layers = do
    -- get required extension names from GLFW
    glfwReqExts <- liftIO GLFW.getRequiredInstanceExtensions
    createVulkanInstance
      progName
      "My perfect Haskell engine"
      glfwReqExts
      layers

getTime :: MonadIO m => m Double
getTime = liftIO $ GLFW.getTime >>= \case
      Just time -> return time
      Nothing -> error "GLFW.getTime failed"