{-# LANGUAGE Strict #-}
module Examples.FPS
  ( runMyVulkanProgram
  ) where

import           Control.Monad
import qualified Graphics.UI.GLFW         as GLFW
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame

import           Lib.Engine.Main
import           Lib.Engine.Simple3D
import           Lib.MonadIO.MVar
import           Lib.Program
import           Lib.Resource
import           Lib.Utils                (perspectiveVk, scale)
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Framebuffer
import           Lib.Vulkan.Image
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.RenderPass
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Shader
-- import           Lib.Vulkan.UniformBufferObject
import           Lib.Vulkan.Vertex
import           Lib.Vulkan.VertexBuffer


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


objMatrixOverTime :: Program r Mat44f
objMatrixOverTime = do
  seconds <- getTime
  return $ rotation seconds


-- | cam rotation using (yaw, pitch)
viewProjMatrix :: VkExtent2D -> (Double, Double) -> Program r Mat44f
viewProjMatrix extent (yaw, pitch) = do
  let width = getField @"width" extent
      height = getField @"height" extent
      aspectRatio = fromIntegral width / fromIntegral height
      camPos = vec3 0 0 (-3)
      -- view = lookAt (vec3 0 0 (-1)) camPos (vec3 0 0 0)
      view = translate3 (- camPos) %* (rotateEuler (realToFrac pitch) (-realToFrac yaw) 0)
      proj = perspectiveVk 0.1 200 (90/360*2*pi) aspectRatio
  return $ view %* proj


loadShaders :: EngineCapability -> Program r [VkPipelineShaderStageCreateInfo]
loadShaders EngineCapability{ dev } = do
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
  let vertices = rectVertices
      indices = rectIndices
      indexCount = dfLen indices

  (vertexBufReady, vertexBuffer) <-
    auto $ createVertexBuffer cap vertices

  (indexBufReady, indexBuffer) <- auto $ createIndexBuffer cap indices
  let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg"]
  (textureReadyEvents, descrTextureInfos) <- auto $ unzip <$> mapM
    (createTextureInfo cap False) texturePaths

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
              -> Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{..} swapInfo shaderStages pipelineLayout = do
  let SwapchainInfo { swapImgs, swapExtent, swapImgFormat } = swapInfo
  msaaSamples <- getMaxUsableSampleCount pdev
  depthFormat <- findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1) swapImgs
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



makeWorld :: MyAppState -> Program r [Object]
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

myAppNewWindow :: GLFW.Window -> Program r WindowState
myAppNewWindow window = do
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

myAppStart :: WindowState -> EngineCapability -> Program r MyAppState
myAppStart winState cap@EngineCapability{ dev } = do
  shaderStages <- loadShaders cap
  (materialDSL, pipelineLayout) <- makePipelineLayouts dev
  -- TODO beware of automatic resource lifetimes when making assets dynamic
  assets <- loadAssets cap materialDSL
  renderContextVar <- newEmptyMVar
  inputMutex <- newMVar ()
  return $ MyAppState{..}

myAppNewSwapchain :: MyAppState -> SwapchainInfo -> Program r ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)])
myAppNewSwapchain MyAppState{..} swapInfo = do
  _ <- tryTakeMVar renderContextVar
  (framebuffers, nextSems, renderContext) <- prepareRender cap swapInfo shaderStages pipelineLayout
  putMVar renderContextVar renderContext
  return (framebuffers, nextSems)

-- | for FPS controls: can only look further down if you look straight up
clampPitch :: Double -> Double
clampPitch a = min (max a (-0.5)) 0.5

myAppRecordFrame :: MyAppState -> VkCommandBuffer -> VkFramebuffer -> Program r ()
myAppRecordFrame appState@MyAppState{..} cmdBuf framebuffer = do
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
  }


runMyVulkanProgram :: IO ()
runMyVulkanProgram = do
  let app = App
        { windowName = "vulkan-experiment"
        , windowSize = (800, 600)
        , flags = [Validation]
        , syncMode = VSync
        , maxFramesInFlight = 2
        , appNewWindow = myAppNewWindow
        , appMainThreadHook = myAppMainThreadHook
        , appStart = myAppStart
        , appNewSwapchain = myAppNewSwapchain
        , appRecordFrame = myAppRecordFrame
        }
  runVulkanProgram app


{-
-- not needed right now
updateDescrSet :: VkDevice
               -> [VkDescriptorImageInfo]
               -> VkDescriptorSet
               -> Program r ()
updateDescrSet dev texInfos descrSet = do
  -- seconds <- getTime
  -- let texIx = floor seconds `mod` 2
  -- updateDescriptorSet dev descrSet 1 [] [texInfos !! texIx]
  updateDescriptorSet dev descrSet 1 [] [texInfos !! 0]
-}
