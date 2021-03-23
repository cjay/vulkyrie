{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
module Vulkyrie.Examples.Flat
  ( runMyVulkanProgram
  ) where

import           Control.Concurrent       (forkIO)
import           Control.Monad
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame
import           UnliftIO.Chan
import           UnliftIO.MVar

import           Vulkyrie.Examples.Flat.Game
import           Vulkyrie.Engine.Main
import           Vulkyrie.Engine.Simple2D
import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Utils                (orthogonalVk, scale)
import           Vulkyrie.Vulkan.Command
import           Vulkyrie.Vulkan.Default.Pipeline
import           Vulkyrie.Vulkan.Default.RenderPass
import           Vulkyrie.Vulkan.Descriptor
import           Vulkyrie.Vulkan.Device
import           Vulkyrie.Vulkan.Engine
import           Vulkyrie.Vulkan.Framebuffer
import           Vulkyrie.Vulkan.Image
import           Vulkyrie.Vulkan.PipelineLayout
import           Vulkyrie.Vulkan.Presentation
import           Vulkyrie.Vulkan.Queue
import           Vulkyrie.Vulkan.Shader
-- import           Vulkyrie.Vulkan.UniformBufferObject




-- | cam pos using (x, y), ortho projection from z 0.1 to 10 excluding boundaries.
viewProjMatrix :: VkExtent2D -> Vec2f -> Program Mat44f
viewProjMatrix extent (Vec2 x y) = do
  let width :: Float = fromIntegral $ getField @"width" extent
      height :: Float = fromIntegral $ getField @"height" extent
      camPos = Vec3 x y 0
      view = translate3 (- camPos)
      camHeight = 5
      proj = orthogonalVk 0.1 10 (width/height * camHeight) camHeight
  return $ view %* proj


loadShaders :: EngineCapability -> Resource [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = do
    vertSM <- auto $ shaderModuleFile dev "shaders/sprites.vert.spv"
    fragSM <- auto $ shaderModuleFile dev "shaders/triangle.frag.spv"

    liftProg $ do
      shaderVert
        <- createShaderStage vertSM
              VK_SHADER_STAGE_VERTEX_BIT
              Nothing

      shaderFrag
        <- createShaderStage fragSM
              VK_SHADER_STAGE_FRAGMENT_BIT
              Nothing

      logInfo $ "Createad vertex shader module: " <> showt shaderVert
      logInfo $ "Createad fragment shader module: " <> showt shaderFrag

      return [shaderVert, shaderFrag]


makePipelineLayouts :: VkDevice -> Resource (VkDescriptorSetLayout, VkPipelineLayout)
makePipelineLayouts dev = do
  frameDSL <- auto $ createDescriptorSetLayout dev [] --[uniformBinding 0]
  -- TODO automate bind ids
  materialDSL <- auto $ createDescriptorSetLayout dev [samplerBinding 0]
  pipelineLayout <- auto $ createPipelineLayout dev
    -- descriptor set numbers 0,1,..
    [frameDSL, materialDSL]
    -- push constant ranges
    [ pushConstantRange VK_SHADER_STAGE_VERTEX_BIT 0 64
    ]

  -- (transObjMems, transObjBufs) <- unzip <$> uboCreateBuffers pdev dev transObjSize maxFramesInFlight
  -- descriptorBufferInfos <- mapM (uboBufferInfo transObjSize) transObjBufs

  -- frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL

  -- forM_ (zip descriptorBufferInfos frameDescrSets) $
    -- \(bufInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [bufInfo] []

  return (materialDSL, pipelineLayout)



loadAssets :: EngineCapability -> VkDescriptorSetLayout -> Resource Assets
loadAssets cap@EngineCapability { dev, descriptorPool } materialDSL = do
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg", "sprite.png"]
  (textureReadyEvents, descrTextureInfos) <- auto $ unzip <$> mapM
    (createTextureInfo cap True) texturePaths

  liftProg $ do
    loadEvents <- newMVar $ textureReadyEvents

    materialDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

    forM_ (zip descrTextureInfos materialDescrSets) $
      \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

    return $ Assets {..}

data Assets
  = Assets
  { loadEvents        :: MVar [QueueEvent]
  , materialDescrSets :: [VkDescriptorSet]
  }


prepareRender :: EngineCapability
              -> SwapchainInfo
              -> [VkPipelineShaderStageCreateInfo]
              -> VkPipelineLayout
              -> Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{ dev, pdev } swapInfo shaderStages pipelineLayout = do
  let SwapchainInfo { swapImgs, swapExtent, swapImgFormat } = swapInfo
  msaaSamples <- liftProg $ getMaxUsableSampleCount pdev
  -- to turn off msaa:
  -- let msaaSamples = VK_SAMPLE_COUNT_1_BIT
  depthFormat <- liftProg $ findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1) swapImgs
  renderPass <- auto $ createRenderPass dev swapImgFormat depthFormat msaaSamples VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  graphicsPipeline
    <- auto $ createGraphicsPipeline dev swapExtent
                              [] []
                              shaderStages
                              renderPass
                              pipelineLayout
                              msaaSamples
                              True

  (nextSems, privAttachments) <- auto $ createPrivateAttachments cap swapExtent swapImgFormat msaaSamples
  framebuffers <- mapM
    (auto . createFramebuffer dev renderPass swapExtent . (privAttachments <>) . (:[]))
    swapImgViews

  return (framebuffers, nextSems, RenderContext graphicsPipeline renderPass pipelineLayout swapExtent)



makeWorld :: GameState -> Assets -> Program (Vec2f, [Object])
makeWorld GameState {..} Assets {..} = do

  let objs = flip map walls $
        \(Vec2 x y) ->
          Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , modelMatrix = (scale 1 1 1) %* (translate3 $ vec3 (realToFrac x) (realToFrac y) (1))
          }

  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  return (camPos, if allDone then objs else [])

myAppNewWindow :: GLFW.Window -> Resource WindowState
myAppNewWindow window = do
  keyEvents <- newChan
  let keyCallback _ key _ keyState _ = do
        writeChan keyEvents $ KeyEvent key keyState
  liftIO $ GLFW.setKeyCallback window (Just keyCallback)
  return WindowState {..}

myAppMainThreadHook :: WindowState -> IO ()
myAppMainThreadHook WindowState {..} = do
  return ()

myAppStart :: WindowState -> EngineCapability -> Resource MyAppState
myAppStart winState@WindowState{ keyEvents } cap@EngineCapability{ dev } = do
  shaderStages <- loadShaders cap
  (materialDSL, pipelineLayout) <- makePipelineLayouts dev
  -- TODO beware of automatic resource lifetimes when making assets dynamic
  assets <- loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  gameState <- newMVar initialGameState
  _ <- liftIO $ forkIO $ runGame gameState keyEvents
  return $ MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- prepareRender cap swapInfo shaderStages pipelineLayout
  putMVar renderContextVar renderContext
  return (framebuffers, nextSems)

myAppRenderFrame :: MyAppState -> RenderFun
myAppRenderFrame MyAppState{..} framebuffer waitSemsWithStages signalSems = do
  let WindowState{..} = winState

  gs <- readMVar gameState
  (camPos, objs) <- makeWorld gs assets

  renderContext <- readMVar renderContextVar
  viewProjTransform <- viewProjMatrix (extent renderContext) camPos
  postWith (cmdCap cap) (cmdQueue cap) waitSemsWithStages signalSems $ \cmdBuf ->
    liftProg $ recordAll renderContext viewProjTransform objs cmdBuf framebuffer

data WindowState
  = WindowState
  { window    :: GLFW.Window
  , keyEvents :: Chan Event
  }

data MyAppState
  = MyAppState
  { shaderStages     :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout   :: VkPipelineLayout
  , cap              :: EngineCapability
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  , winState         :: WindowState
  , gameState        :: MVar GameState
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
