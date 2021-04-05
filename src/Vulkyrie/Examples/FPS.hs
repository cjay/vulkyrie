{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
module Vulkyrie.Examples.FPS
  ( runMyVulkanProgram
  ) where

import           Control.Monad
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame
import           UnliftIO.MVar

import           Vulkyrie.Concurrent
import           Vulkyrie.Engine.Main
import           Vulkyrie.Engine.Simple3D
import           Vulkyrie.GLFW
import           Vulkyrie.Program
import           Vulkyrie.Resource
import           Vulkyrie.Utils                (perspectiveVk, scale)
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
import           Vulkyrie.Vulkan.VertexBuffer

import           Vulkyrie.Examples.Vertex

rectVertices :: DataFrame Vertex '[XN 3]
rectVertices = atLeastThree $ fromList
  [ -- rectangle
    --              coordinate                  texture coordinate
    scalar $ Vertex (vec3 (-0.5) (-0.5)   0.0 ) (vec2 0 0)
  , scalar $ Vertex (vec3   0.5  (-0.5)   0.0 ) (vec2 1 0)
  , scalar $ Vertex (vec3   0.5    0.5    0.0 ) (vec2 1 1)
  , scalar $ Vertex (vec3 (-0.5)   0.5    0.0 ) (vec2 0 1)
  ]

rectIndices :: DataFrame Word32 '[XN 3]
rectIndices = atLeastThree $ fromList
  [ -- rectangle
    0, 1, 2, 2, 3, 0
  ]


rotation :: Double -> Mat44f
rotation seconds =
  let rate = 1/16 -- rotations per second
      (_::Int, phaseTau) = properFraction $ seconds * rate
  in rotate (vec3 0 0 1) (realToFrac phaseTau * 2 * pi)


objMatrixOverTime :: Prog r Mat44f
objMatrixOverTime = rotation <$> getTime


-- | cam rotation using (yaw, pitch)
viewProjMatrix :: VkExtent2D -> (Double, Double) -> Prog r Mat44f
viewProjMatrix extent (yaw, pitch) = do
  let width = getField @"width" extent
      height = getField @"height" extent
      aspectRatio = fromIntegral width / fromIntegral height
      camPos = vec3 0 0 (-3)
      -- view = lookAt (vec3 0 0 (-1)) camPos (vec3 0 0 0)
      view = translate3 (- camPos) %* (rotateEuler (realToFrac pitch) (-realToFrac yaw) 0)
      proj = perspectiveVk 0.1 200 (90/360*2*pi) aspectRatio
  return $ view %* proj


loadShaders :: EngineCapability -> Resource [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = Resource $ do
    vertSM <- auto $ shaderModuleFile dev "shaders/triangle.vert.spv"
    fragSM <- auto $ shaderModuleFile dev "shaders/triangle.frag.spv"

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
makePipelineLayouts dev = Resource $ do
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
loadAssets cap@EngineCapability { dev, descriptorPool } materialDSL = Resource $ do
  let vertices = rectVertices
      indices = rectIndices
      indexCount = dfLen indices

  (vertexBufReady, vertexBuffer) <-
    auto $ createVertexBuffer cap vertices

  (indexBufReady, indexBuffer) <- auto $ createIndexBuffer cap indices
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg"]
  (textureReadyEvents, descrTextureInfos) <- unzip <$> mapM
    (auto . createTextureInfo cap False) texturePaths

  loadEvents <- newMVar $ textureReadyEvents <> [vertexBufReady, indexBufReady]

  materialDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

  forM_ (zip descrTextureInfos materialDescrSets) $
    \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

  return $ Assets {..}

data Assets
  = Assets
  { loadEvents        :: MVar [QueueEvent]
  , materialDescrSets :: [VkDescriptorSet]
  , vertexBuffer      :: VkBuffer
  , indexBuffer       :: VkBuffer
  , indexCount        :: Word32
  }


prepareRender :: EngineCapability
              -> SwapchainInfo
              -> [VkPipelineShaderStageCreateInfo]
              -> VkPipelineLayout
              -> Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{ dev, pdev } swapInfo shaderStages pipelineLayout = Resource $ do
  let SwapchainInfo { swapImgs, swapExtent, swapImgFormat } = swapInfo
  msaaSamples <- getMaxUsableSampleCount pdev
  depthFormat <- findDepthFormat pdev

  swapImgViews <-
    mapM (\image -> auto $ createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1) swapImgs
  renderPass <- auto $ createRenderPass dev swapImgFormat depthFormat msaaSamples VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  graphicsPipeline
    <- auto $ createGraphicsPipeline dev swapExtent
                              [vertIBD] vertIADs
                              shaderStages
                              renderPass
                              pipelineLayout
                              msaaSamples
                              False

  (nextSems, privAttachments) <- auto $ createPrivateAttachments cap swapExtent swapImgFormat msaaSamples
  framebuffers <- mapM
    (auto . createFramebuffer dev renderPass swapExtent . (privAttachments <>) . (:[]))
    swapImgViews
  return (framebuffers, nextSems, RenderContext graphicsPipeline renderPass pipelineLayout swapExtent)



makeWorld :: MyAppState -> Prog r [Object]
makeWorld MyAppState{ assets } = do
  let Assets{..} = assets
  -- objTransformsRef <- newIORef [translate3 $ vec3 0 1 1, translate3 $ vec3 0 0 0]
  objMatrix <- objMatrixOverTime

  let objs =
        [ Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 0) []
          , vertexBufferLoc = BufferLoc vertexBuffer 0
          , indexBufferLoc = BufferLoc indexBuffer 0
          , firstIndex = 0
          , indexCount
          , modelMatrix = objMatrix %* (translate3 $ vec3 0 1 1)
          }
        , Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , vertexBufferLoc = BufferLoc vertexBuffer 0
          , indexBufferLoc = BufferLoc indexBuffer 0
          , firstIndex = 0
          , indexCount
          , modelMatrix = objMatrix
          }
        -- the room:
        , Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , vertexBufferLoc = BufferLoc vertexBuffer 0
          , indexBufferLoc = BufferLoc indexBuffer 0
          , firstIndex = 0
          , indexCount
          , modelMatrix = rotateX (-pi/2) %* scale 100 100 100 %* translate3 (vec3 0 25 0)
          }
        , Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , vertexBufferLoc = BufferLoc vertexBuffer 0
          , indexBufferLoc = BufferLoc indexBuffer 0
          , firstIndex = 0
          , indexCount
          , modelMatrix = rotateX (pi/2) %* scale 100 100 100 %* translate3 (vec3 0 (-25) 0)
          }
        , Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , vertexBufferLoc = BufferLoc vertexBuffer 0
          , indexBufferLoc = BufferLoc indexBuffer 0
          , firstIndex = 0
          , indexCount
          , modelMatrix = rotateY (pi/2) %* scale 100 100 100 %* translate3 (vec3 25 0 0)
          }
        , Object
          { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
          , vertexBufferLoc = BufferLoc vertexBuffer 0
          , indexBufferLoc = BufferLoc indexBuffer 0
          , firstIndex = 0
          , indexCount
          , modelMatrix = rotateY (-pi/2) %* scale 100 100 100 %* translate3 (vec3 (-25) 0 0)
          }
        ]

  -- a bit simplistic. when hot loading assets, better filter the objects that depend on them
  events <- takeMVar loadEvents
  notDone <- filterM (fmap not . isDone) events
  let allDone = null notDone
  putMVar loadEvents notDone

  if allDone then return objs else return []

myAppNewWindow :: GLFW.Window -> Resource WindowState
myAppNewWindow window = Resource $ do
  liftIO $ GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  rawSupported <- liftIO $ GLFW.rawMouseMotionSupported
  if rawSupported
    then do
      liftIO $ GLFW.setRawMouseMotion window True
      logInfo "Raw mouse mode activated"
    else logInfo $ "Now raw mouse support"

  startPos <- liftIO $ GLFW.getCursorPos window
  anchorMousePos <- newMVar startPos
  mousePos <- newMVar startPos
  -- liftIO $ GLFW.setCursorPosCallback window $ Just $ \_ x y -> do
  --   -- putStrLn $ "mouse(" <> show x <> "," <> show y <> ")"
  --   _ <- swapMVar mousePos (x, y)
  --   return ()

  return WindowState {..}

myAppMainThreadHook :: WindowState -> IO ()
myAppMainThreadHook WindowState {..} = do
  pos <- GLFW.getCursorPos window
  _ <- tryTakeMVar mousePos
  putMVar mousePos pos
  -- putStrLn "."
  return ()

myAppStart :: WindowState -> EngineCapability -> Resource MyAppState
myAppStart winState cap@EngineCapability{ dev } = Resource $ do
  shaderStages <- auto $ loadShaders cap
  (materialDSL, pipelineLayout) <- auto $ makePipelineLayouts dev
  -- TODO beware of automatic resource lifetimes when making assets dynamic
  assets <- auto $ loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  inputMutex <- newMVar ()
  renderThreadOwner <- auto threadOwner
  return $ MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = Resource $ do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- auto $ prepareRender cap swapInfo shaderStages pipelineLayout
  putMVar renderContextVar renderContext
  return (framebuffers, nextSems)

-- | for FPS controls: can only look further down if you look straight up
clampPitch :: Double -> Double
clampPitch a = min (max a (-0.5)) 0.5

myAppRenderFrame :: MyAppState -> RenderFun
myAppRenderFrame appState@MyAppState{..} framebuffer waitSemsWithStages signalSems = do
  let WindowState{..} = winState
  objs <- makeWorld appState
  renderContext <- readMVar renderContextVar

  takeMVar inputMutex

  -- _ <- takeMVar mousePos
  liftIO $ GLFW.postEmptyEvent
  (x, y) <- readMVar mousePos
  (ax, ay) <- takeMVar anchorMousePos
  let (dx, dy) = (x-ax, y-ay)

  let sens = 0.0015 -- half revolutions per mouse count
      rawPitch = dy * sens
      normPitch = clampPitch rawPitch
      lookDir = (dx * sens * pi, normPitch * pi)
      ay' = ay + (rawPitch - normPitch) / sens
  putMVar anchorMousePos (ax, ay')

  putMVar inputMutex ()

  viewProjTransform <- viewProjMatrix (extent renderContext) lookDir
  postWith (cmdCap cap) (cmdQueue cap) waitSemsWithStages signalSems renderThreadOwner $ \cmdBuf -> Resource $
    recordAll renderContext viewProjTransform objs cmdBuf framebuffer


data WindowState
  = WindowState
  { window         :: GLFW.Window
  , mousePos       :: MVar (Double, Double)
  , anchorMousePos :: MVar (Double, Double)
  }

data MyAppState
  = MyAppState
  { shaderStages     :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout   :: VkPipelineLayout
  , cap              :: EngineCapability
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  , winState         :: WindowState
  , inputMutex       :: MVar ()
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


{-
-- not needed right now
updateDescrSet :: VkDevice
               -> [VkDescriptorImageInfo]
               -> VkDescriptorSet
               -> Program ()
updateDescrSet dev texInfos descrSet = do
  -- seconds <- getTime
  -- let texIx = floor seconds `mod` 2
  -- updateDescriptorSet dev descrSet 1 [] [texInfos !! texIx]
  updateDescriptorSet dev descrSet 1 [] [texInfos !! 0]
-}
