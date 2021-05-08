{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Vulkyrie.Examples.Flat
  ( runMyVulkanProgram
  ) where

import           Control.Monad
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import Numeric.DataFrame ( DataFrame(Vec2), Vector2(vec2), vec4, scalar )
import           UnliftIO.Chan
import           UnliftIO.MVar

import           Vulkyrie.Concurrent
import           Vulkyrie.Examples.Flat.Game
import           Vulkyrie.Engine.Main
import           Vulkyrie.Engine.Simple2D
import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Descriptor
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Image
import           Vulkyrie.Vulkan.Presentation
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Engine.Pipeline
import qualified Vulkyrie.Engine.Pipeline.Sprite as Sprite
import qualified Vulkyrie.Engine.Pipeline.ColorRect as ColorRect
import Numeric.Matrix (Mat44f)
import Data.Tagged (Tagged(unTagged))

-- | All pipelines that are in use.
type PipelineOrder = '[Sprite.Pipeline, ColorRect.Pipeline]

-- | This verifies that the all needed ProtoPipelines were created, and in the right order
verifyPipelines :: Tagged PipelineOrder [ProtoPipeline] -> [ProtoPipeline]
verifyPipelines = unTagged

data Pipelines = Pipelines
  { pipelineObjs :: Vector VkPipeline
  , pipelineLayouts :: Vector VkPipelineLayout
  }

instance (PipelineIndex PipelineOrder pipeline pipelineIndex) => PipelineProvider Pipelines pipeline where
  getPipeline_ Pipelines{ pipelineObjs, pipelineLayouts } =
    ( pipelineLayouts Vector.! indexVal @pipelineIndex
    , pipelineObjs Vector.! indexVal @pipelineIndex
    )

loadAssets :: EngineCapability -> VkDescriptorSetLayout -> Resource Assets
loadAssets cap@EngineCapability{ dev, descriptorPool } materialDSL = Resource $ do
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg", "sprite.png"]
  (textureReadyEvents, descrTextureInfos) <- unzip <$> mapM
    (auto . createTextureFromFile cap True) texturePaths

  loadEvents <- newMVar $ textureReadyEvents

  materialDescrSets <-
    allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

  forM_ (zip descrTextureInfos materialDescrSets) $
    \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

  return Assets{..}

data Assets
  = Assets
  { loadEvents        :: MVar [QueueEvent]
  , materialDescrSets :: [VkDescriptorSet]
  }


renderWorld ::
  (
    PipelineProvider pipelines Sprite.Pipeline,
    PipelineProvider pipelines ColorRect.Pipeline
  ) =>
  pipelines -> Mat44f -> GameState -> Assets -> VkCommandBuffer -> Prog r ()
renderWorld pipelines transform GameState{..} Assets{..} cmdBuf = do
  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  when allDone $ do
    do
      let (pipelineLayout, pipelineObj) = getPipeline @Sprite.Pipeline pipelines

      liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineObj
      Sprite.pushTransform cmdBuf pipelineLayout transform
      bindMat cmdBuf pipelineLayout $ DescrBindInfo (materialDescrSets !! 0) []
      Sprite.pushSize cmdBuf pipelineLayout $ vec2 1 1
      Sprite.pushUVPos cmdBuf pipelineLayout $ vec2 0 0
      Sprite.pushUVSize cmdBuf pipelineLayout $ vec2 1 1
      forM_ walls $ \(Vec2 x y) -> do
        Sprite.pushPos cmdBuf pipelineLayout (vec2 (realToFrac x) (realToFrac y))
        Sprite.draw cmdBuf
    do
      let (pipelineLayout, pipelineObj) = getPipeline @ColorRect.Pipeline pipelines

      liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineObj
      -- TODO ensure in types that only the pushTransform of ColorRect can be used
      ColorRect.pushTransform cmdBuf pipelineLayout transform
      let Vec2 x y = playerPos
      ColorRect.pushPos cmdBuf pipelineLayout (vec2 (realToFrac x + 0.5) (realToFrac y + 0.5))
      ColorRect.pushSize cmdBuf pipelineLayout $ vec2 1 1.2
      ColorRect.pushCenter cmdBuf pipelineLayout $ vec2 0.5 0.6
      ColorRect.pushTurns cmdBuf pipelineLayout $ scalar (1/16)
      ColorRect.pushColor cmdBuf pipelineLayout $ vec4 0.8 0.5 0.7 0.8
      ColorRect.draw cmdBuf

myAppRenderFrame :: MyAppState -> RenderFun
myAppRenderFrame MyAppState{..} framebuffer waitSemsWithStages signalSems = do
  -- let WindowState{..} = winState
  gs@GameState{ camPos } <- readMVar gameState
  renderContext@RenderContext{ pipelineObjs } <- readMVar renderContextVar
  let pipelineLayouts = Vector.map (\ProtoPipeline{ pipelineLayout } -> pipelineLayout) protoPipelines
      pipelines = Pipelines{ pipelineLayouts, pipelineObjs }
  viewProjTransform <- viewProjMatrix (extent renderContext) camPos
  postWith (cmdCap cap) (cmdQueue cap) waitSemsWithStages signalSems renderThreadOwner $ \cmdBuf -> fakeResource $ do
    withRenderPass renderContext cmdBuf framebuffer $ do
      renderWorld pipelines viewProjTransform gs assets cmdBuf

myAppNewWindow :: GLFW.Window -> Resource WindowState
myAppNewWindow window = Resource $ do
  keyEventChan <- newChan
  let keyCallback _ key _ keyState _ = do
        writeChan keyEventChan $ KeyEvent key keyState
  liftIO $ GLFW.setKeyCallback window (Just keyCallback)
  return WindowState{..}

myAppMainThreadHook :: WindowState -> IO ()
myAppMainThreadHook _ = do
  return ()

myAppStart :: WindowState -> EngineCapability -> Resource MyAppState
myAppStart winState@WindowState{ keyEventChan } cap = Resource $ do
  (spritePipeline, materialDSL) <- auto $ Sprite.loadPipeline cap
  colorRectPipeline <- auto $ ColorRect.loadPipeline cap
  let protoPipelines = Vector.fromList $ verifyPipelines
        (spritePipeline +: colorRectPipeline +: nilTaggedList)
  assets <- auto $ loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  gameState <- newMVar initialGameState
  void $ auto $ threadRes $ runGame gameState keyEventChan
  renderThreadOwner <- auto threadOwner
  return MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = Resource $ do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- auto $ prepareRender cap swapInfo protoPipelines
  putMVar renderContextVar renderContext
  return (framebuffers, nextSems)

data WindowState
  = WindowState
  { window       :: GLFW.Window
  , keyEventChan :: Chan Event
  }

data MyAppState
  = MyAppState
  { protoPipelines   :: Vector ProtoPipeline
  , cap              :: EngineCapability
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  , winState         :: WindowState
  , gameState        :: MVar GameState
  , renderThreadOwner :: ThreadOwner
  }


runMyVulkanProgram :: IO ()
runMyVulkanProgram = do
  let app = App
        { windowName = "vulkan-experiment"
        , windowSize = (800, 600)
        , windowFullscreen = False
        , flags = [Validation]
        , syncMode = VSync
        , maxFramesInFlight = 2
        , appNewWindow = myAppNewWindow
        , appMainThreadHook = myAppMainThreadHook
        , appStart = myAppStart
        , appNewSwapchain = myAppNewSwapchain
        , appRenderFrame = myAppRenderFrame
        }
  runVulkanProgram app
