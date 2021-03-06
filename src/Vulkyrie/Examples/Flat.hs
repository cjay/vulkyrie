{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Vulkyrie.Examples.Flat
  ( runMyVulkanProgram
  ) where

import           Control.Monad
import           Control.Monad.Reader
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import Numeric.DataFrame ( DataFrame(Vec2), Vector2(vec2), vec4, scalar, Vector3 (vec3) )
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
type PipelineOrder = '[Sprite.AlphaBlendPipeline, Sprite.AlphaDiscardPipeline, Sprite.OpaquePipeline, ColorRect.Pipeline]

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
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg", "sprite.png", "sun.png"]
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
    PipelineProvider pipelines Sprite.AlphaBlendPipeline,
    PipelineProvider pipelines Sprite.AlphaDiscardPipeline,
    PipelineProvider pipelines Sprite.OpaquePipeline,
    PipelineProvider pipelines ColorRect.Pipeline
  ) =>
  pipelines -> Mat44f -> GameState -> Assets -> VkCommandBuffer -> Prog r ()
renderWorld pipelines transform GameState{..} Assets{..} cmdBuf = do
  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  when allDone $ runCmd cmdBuf $ do
    -- sprite.png has only alpha 0 or 1. visible in front of walls thanks to depth write.
    -- z=1
    withPipeline @Sprite.AlphaDiscardPipeline pipelines $ do
      Sprite.pushTransform transform
      bindMat $ DescrBindInfo (materialDescrSets !! 2) []
      Sprite.pushSize $ vec2 0.25 0.25
      Sprite.pushCenter $ vec2 0 0
      Sprite.pushTurns $ scalar 0
      Sprite.pushUVPos $ vec2 0 0
      Sprite.pushUVSize $ vec2 1 1
      let Vec2 x y = playerPos
      Sprite.pushPos (vec3 (realToFrac x + 0.25) (realToFrac y + 0.25) 1.0)
      Sprite.draw

    -- walls at z=2
    withPipeline @Sprite.OpaquePipeline pipelines $ do
      Sprite.pushTransform transform
      bindMat $ DescrBindInfo (materialDescrSets !! 0) []
      Sprite.pushSize $ vec2 1 1
      Sprite.pushCenter $ vec2 0 0
      Sprite.pushTurns $ scalar 0
      Sprite.pushUVPos $ vec2 0 0
      Sprite.pushUVSize $ vec2 1 1
      forM_ walls $ \(Vec2 x y) -> do
        Sprite.pushPos (vec3 (realToFrac x) (realToFrac y) 2.0)
        Sprite.draw

    withPipeline @ColorRect.Pipeline pipelines $ do
      ColorRect.pushTransform transform
      let Vec2 x y = playerPos - vec2 1 1
      ColorRect.pushPos (vec3 (realToFrac x + 0.5) (realToFrac y + 0.5) 1.0)
      ColorRect.pushSize $ vec2 1 1.2
      ColorRect.pushCenter $ vec2 0.5 0.6
      ColorRect.pushTurns $ scalar (1/16)
      ColorRect.pushColor $ vec4 0.8 0.5 0.7 0.8
      ColorRect.draw

    -- sun with transparent aura
    -- z=1.5, overlapped by sprite.png thanks to depth test
    withPipeline @Sprite.AlphaBlendPipeline pipelines $ do
      Sprite.pushTransform transform
      bindMat $ DescrBindInfo (materialDescrSets !! 3) []
      Sprite.pushSize $ vec2 1 1
      Sprite.pushCenter $ vec2 0 0
      Sprite.pushTurns $ scalar 0
      Sprite.pushUVPos $ vec2 0 0
      Sprite.pushUVSize $ vec2 1 1
      let Vec2 x y = playerPos
      Sprite.pushPos (vec3 (realToFrac x) (realToFrac y) 1.5)
      Sprite.draw


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
  (spriteAlphaBlendPipeline, materialDSL) <- auto $ Sprite.loadPipeline Sprite.AlphaBlend cap
  (spriteAlphaDiscardPipeline, _) <- auto $ Sprite.loadPipeline Sprite.AlphaDiscard cap
  (spriteOpaquePipeline, _) <- auto $ Sprite.loadPipeline Sprite.Opaque cap
  colorRectPipeline <- auto $ ColorRect.loadPipeline cap
  let protoPipelines = Vector.fromList $ verifyPipelines $
        spriteAlphaBlendPipeline
        +: spriteAlphaDiscardPipeline
        +: spriteOpaquePipeline
        +: colorRectPipeline
        +: nilTaggedList
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
