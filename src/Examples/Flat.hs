{-# LANGUAGE Strict #-}
module Examples.Flat
  ( runMyVulkanProgram
  ) where

import           Control.Monad
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan.Core_1_0
import           Numeric.DataFrame

import           Lib.Engine.Main
import           Lib.Engine.Simple2D
import           Lib.MonadIO.MVar
import           Lib.Program
import           Lib.Resource
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Image
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Shader
-- import           Lib.Vulkan.UniformBufferObject


-- | cam pos using (x, y), ortho projection from z -10 to +10 excluding boundaries.
viewProjMatrix :: VkExtent2D -> (Double, Double) -> Program r Mat44f
viewProjMatrix extent (x, y) = do
  let width = fromIntegral $ getField @"width" extent
      height = fromIntegral $ getField @"height" extent
      camPos = vec3 (realToFrac x) (realToFrac y) 10
      view = translate3 (- camPos)
      camHeight = 5
      proj = orthogonal 0 20 (width/height * camHeight) camHeight
  return $ view %* proj


scale :: Float -> Float -> Float -> Mat44f
scale sx sy sz = DF4 (DF4 (S sx) (S 0.0) (S 0.0) (S 0.0)) (DF4 (S 0.0) (S sy) (S 0.0) (S 0.0)) (DF4 (S 0.0) (S 0.0) (S sz) (S 0.0)) (DF4 (S 0.0) (S 0.0) (S 0.0) (S 1.0))


loadShaders :: EngineCapability -> Program r [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = do
    vertSM <- auto $ shaderModuleFile dev "shaders/sprites.vert.spv"
    fragSM <- auto $ shaderModuleFile dev "shaders/triangle.frag.spv"

    shaderVert
      <- createShaderStage vertSM
            VK_SHADER_STAGE_VERTEX_BIT
            Nothing

    shaderFrag
      <- createShaderStage fragSM
            VK_SHADER_STAGE_FRAGMENT_BIT
            Nothing

    logInfo $ "Createad vertex shader module: " ++ show shaderVert
    logInfo $ "Createad fragment shader module: " ++ show shaderFrag

    return [shaderVert, shaderFrag]


makePipelineLayouts :: VkDevice -> Program r (VkDescriptorSetLayout, VkPipelineLayout)
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



loadAssets :: EngineCapability -> VkDescriptorSetLayout -> Program r Assets
loadAssets cap@EngineCapability { dev, descriptorPool } materialDSL = do
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg"]
  (textureReadyEvents, descrTextureInfos) <- auto $ unzip <$> mapM
    (createTextureInfo cap) texturePaths

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
              -> Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{..} swapInfo shaderStages pipelineLayout = do
  msaaSamples <- getMaxUsableSampleCount pdev
  depthFormat <- findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1)
         (swapImgs swapInfo)
  renderPass <- auto $ createRenderPass dev swapInfo depthFormat msaaSamples
  graphicsPipeline
    <- auto $ createGraphicsPipeline dev swapInfo
                              [] []
                              shaderStages
                              renderPass
                              pipelineLayout
                              msaaSamples

  (colorAttSem, colorAttImgView) <- auto $ createColorAttImgView cap
                      (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
  (depthAttSem, depthAttImgView) <- auto $ createDepthAttImgView cap
                      (swapExtent swapInfo) msaaSamples
  let nextSems = [(colorAttSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                 , (depthAttSem, VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                 ]
  framebuffers
    <- auto $ createFramebuffers dev renderPass swapInfo swapImgViews depthAttImgView colorAttImgView

  return (framebuffers, nextSems, RenderContext graphicsPipeline renderPass pipelineLayout (swapExtent swapInfo))



makeWorld :: MyAppState -> Program r [Object]
makeWorld MyAppState{ assets } = do
  let Assets{..} = assets

  let objs =
        [ Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 0) []
          , modelMatrix = (scale 2 2 1) %* (translate3 $ vec3 0 0 0)
          }
        , Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , modelMatrix = translate3 $ vec3 2 0 0
          }
        ]

  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  if allDone then return objs else return []

myAppNewWindow :: GLFW.Window -> Program r WindowState
myAppNewWindow window = do
  return WindowState {..}

myAppMainThreadHook :: WindowState -> IO ()
myAppMainThreadHook WindowState {..} = do
  return ()

myAppStart :: WindowState -> EngineCapability -> Program r MyAppState
myAppStart winState cap@EngineCapability{ dev } = do
  shaderStages <- loadShaders cap
  (materialDSL, pipelineLayout) <- makePipelineLayouts dev
  -- TODO beware of automatic resource lifetimes when making assets dynamic
  assets <- loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  return $ MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- prepareRender cap swapInfo shaderStages pipelineLayout
  putMVar renderContextVar renderContext
  return (framebuffers, nextSems)

myAppRecordFrame :: MyAppState -> VkCommandBuffer -> VkFramebuffer -> Program r ()
myAppRecordFrame appState@MyAppState{..} cmdBuf framebuffer = do
  let WindowState{..} = winState
  objs <- makeWorld appState
  renderContext <- readMVar renderContextVar
  let camPos = (1, 1)

  viewProjTransform <- viewProjMatrix (extent renderContext) camPos
  recordAll renderContext viewProjTransform objs cmdBuf framebuffer


data WindowState
  = WindowState
  { window         :: GLFW.Window
  }

data MyAppState
  = MyAppState
  { shaderStages     :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout   :: VkPipelineLayout
  , cap              :: EngineCapability
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  , winState         :: WindowState
  }


runMyVulkanProgram :: IO ()
runMyVulkanProgram = do
  let app = App
        { windowName = "vulkan-experiment"
        , windowSize = (800, 600)
        , appNewWindow = myAppNewWindow
        , appMainThreadHook = myAppMainThreadHook
        , appStart = myAppStart
        , appNewSwapchain = myAppNewSwapchain
        , appRecordFrame = myAppRecordFrame
        }
  runVulkanProgram app
