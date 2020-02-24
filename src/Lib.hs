{-# LANGUAGE Strict     #-}
module Lib
  ( runMyVulkanProgram
  ) where

import           Control.Monad
import           Graphics.Vulkan.Core_1_0
import           Numeric.DataFrame

import           Lib.Engine.Main
import           Lib.Engine.Simple3D
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
    (createTextureInfo cap) texturePaths

  let loadEvents = textureReadyEvents <> [vertexBufReady, indexBufReady]

  materialDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

  forM_ (zip descrTextureInfos materialDescrSets) $
    \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

  return $ Assets {..}

data Assets
  = Assets
  { loadEvents        :: [QueueEvent]
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
  msaaSamples <- getMaxUsableSampleCount pdev
  depthFormat <- findDepthFormat pdev

  swapImgViews <- auto $
    mapM (\image -> createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1)
         (swapImgs swapInfo)
  renderPass <- auto $ createRenderPass dev swapInfo depthFormat msaaSamples
  graphicsPipeline
    <- auto $ createGraphicsPipeline dev swapInfo
                              vertIBD vertIADs
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
        ]

  -- TODO should stop checking once all are loaded
  loaded <- and <$> mapM isDone loadEvents

  if loaded then return objs else return []


myAppStart :: EngineCapability -> Program r MyAppState
myAppStart cap@EngineCapability{ dev } = do
  shaderStages <- loadShaders cap
  (materialDSL, pipelineLayout) <- makePipelineLayouts dev
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
  objs <- makeWorld appState
  renderContext <- readMVar renderContextVar
  recordAll renderContext objs cmdBuf framebuffer

data MyAppState
  = MyAppState
  { shaderStages     :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout   :: VkPipelineLayout
  , cap              :: EngineCapability
  , assets           :: Assets
  , renderContextVar :: MVar RenderContext
  }


runMyVulkanProgram :: IO ()
runMyVulkanProgram = do
  let app = App
        { windowName = "vulkan-experiment"
        , windowSize = (800, 600)
        , appStart = myAppStart
        , appNewSwapchain = myAppNewSwapchain
        , appRecordFrame = myAppRecordFrame
        }
  runVulkanProgram app


{-
-- TODO not needed right now
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
