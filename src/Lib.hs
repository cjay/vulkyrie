{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
  ( runVulkanProgram
  ) where

import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import           Data.Maybe                           (fromJust)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame
import           Numeric.Dimensions

import           Lib.GLFW
import           Lib.MetaResource
import           Lib.MonadIO.IORef
import           Lib.MonadIO.MVar
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Command
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Engine
import           Lib.Vulkan.Engine.Simple3D
import           Lib.Vulkan.Image
import           Lib.Vulkan.Memory
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Shader
import           Lib.Vulkan.Shader.TH
import           Lib.Vulkan.Sync
-- import           Lib.Vulkan.UniformBufferObject
import           Lib.Vulkan.Vertex
import           Lib.Vulkan.VertexBuffer


rectVertices :: DataFrame Vertex '[XN 3]
rectVertices = fromJust $ fromList (D @3)
  [ -- rectangle
    --              coordinate                  texture coordinate
    scalar $ Vertex (vec3 (-0.5) (-0.5)   0.0 ) (vec2 0 0)
  , scalar $ Vertex (vec3   0.5  (-0.5)   0.0 ) (vec2 1 0)
  , scalar $ Vertex (vec3   0.5    0.5    0.0 ) (vec2 1 1)
  , scalar $ Vertex (vec3 (-0.5)   0.5    0.0 ) (vec2 0 1)
  ]

rectIndices :: DataFrame Word32 '[XN 3]
rectIndices = fromJust $ fromList (D @3)
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


viewProjMatrix :: VkExtent2D -> Program r Mat44f
viewProjMatrix extent = do
  let width = getField @"width" extent
  let height = getField @"height" extent
  let aspectRatio = fromIntegral width / fromIntegral height
  let view = lookAt (vec3 0 0 (-1)) (vec3 2 2 2) (vec3 0 0 0)
  let proj = perspective 0.1 20 (45/360*2*pi) aspectRatio
  return $ proj %* view


runVulkanProgram :: IO ()
runVulkanProgram = runProgram checkStatus $ do
  windowSizeChanged <- newIORef False
  window <- initGLFWWindow 800 600 "vulkan-experiment" windowSizeChanged

  vulkanInstance <- createGLFWVulkanInstance "vulkan-experiment-instance"

  vulkanSurface <- createSurface vulkanInstance window
  logInfo $ "Createad surface: " ++ show vulkanSurface

  glfwWaitEventsMeanwhile $ do
    (_, pdev) <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " ++ show pdev
    msaaSamples <- getMaxUsableSampleCount pdev

    (dev, queues) <- createGraphicsDevice pdev vulkanSurface
    let gfxQ = graphicsQueue queues

    msp <- auto $ metaMasterSemaphorePool dev
    gq <- auto $ metaManagedQueue dev gfxQ msp
    attachQueuePump gq 16666
    cpp <- auto $ metaCommandPoolPool dev (graphicsFamIdx queues)

    sp <- auto $ metaSemaphorePool msp
    cmdCap <- auto $ metaCommandCapability cpp
    memPool <- auto $ metaMemoryPool pdev dev
    -- TODO create permanently mapped reusable staging buffer
    let cap = EngineCapability pdev dev cmdCap gq sp memPool

    logInfo $ "Createad device: " ++ show dev
    logInfo $ "Createad queues: " ++ show queues

    shaderVert
      <- createVkShaderStageCI dev
            $(compileGLSL "shaders/triangle.vert")
            VK_SHADER_STAGE_VERTEX_BIT

    shaderFrag
      <- createVkShaderStageCI dev
            $(compileGLSL "shaders/triangle.frag")
            VK_SHADER_STAGE_FRAGMENT_BIT

    logInfo $ "Createad vertex shader module: " ++ show shaderVert
    logInfo $ "Createad fragment shader module: " ++ show shaderFrag

    frameIndexRef <- newIORef 0
    renderFinishedSems <- createFrameSemaphores dev
    queueEvents <- sequence $ replicate maxFramesInFlight $ newSetQueueEvent >>= newIORef
    frameFinishedEvent <- liftIO $ Event.new
    frameOnQueueVars <- sequence $ replicate maxFramesInFlight $ newEmptyMVar

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIndexPtr <- mallocRes

    let vertices = rectVertices
        indices = rectIndices

    (vertexSem, vertexBuffer) <-
      createVertexBuffer cap vertices

    (indexSem, indexBuffer) <- createIndexBuffer cap indices

    frameDSL <- createDescriptorSetLayout dev [] --[uniformBinding 0]
    materialDSL <- createDescriptorSetLayout dev [samplerBinding 0]
    pipelineLayout <- createPipelineLayout dev
      [frameDSL, materialDSL] -- descriptor set bindings 0,1,..
      [pushConstantRange VK_SHADER_STAGE_VERTEX_BIT 0 64] -- push constant ranges

    let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg"]
    (textureSems, descrTextureInfos) <- unzip <$> mapM
      (createTextureInfo cap) texturePaths

    depthFormat <- findDepthFormat pdev

    -- (transObjMems, transObjBufs) <- unzip <$> uboCreateBuffers pdev dev transObjSize maxFramesInFlight
    -- descriptorBufferInfos <- mapM (uboBufferInfo transObjSize) transObjBufs

    descriptorPool <- createDescriptorPool dev $ maxFramesInFlight * (1 + length descrTextureInfos)
    -- frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL
    materialDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

    -- forM_ (zip descriptorBufferInfos frameDescrSets) $
      -- \(bufInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [bufInfo] []

    forM_ (zip descrTextureInfos materialDescrSets) $
      \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

    let objs =
          [ Object
            { materialBindInfo = DescrBindInfo (materialDescrSets !! 0) Nothing
            , vertexBufferLoc = BufferLoc vertexBuffer 0
            , indexBufferLoc = BufferLoc indexBuffer 0
            , firstIndex = 0
            , indexCount = fromIntegral $ dimSize1 indices
            }
          , Object
            { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) Nothing
            , vertexBufferLoc = BufferLoc vertexBuffer 0
            , indexBufferLoc = BufferLoc indexBuffer 0
            , firstIndex = 0
            , indexCount = fromIntegral $ dimSize1 indices
            }
          ]

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

    removeQueuePump gq

    let indexSems = [indexSem]
    let loadSems = [(vertexSem, VK_PIPELINE_STAGE_VERTEX_INPUT_BIT)] <>
          map (\sem -> (sem, VK_PIPELINE_STAGE_VERTEX_INPUT_BIT)) indexSems <>
          map (\sem -> (sem, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)) textureSems
    nextSems <- newMVar loadSems

    -- The code below re-runs when the swapchain was re-created
    asyncRedo $ \redoWithNewSwapchain -> do
      logInfo "New thread: Creating things that depend on the swapchain.."
      -- need this for delayed destruction of the old swapchain if it gets replaced
      oldSwapchainSlot <- createSwapchainSlot dev
      swapInfo <- readIORef swapInfoRef
      swapImgViews <- mapM (\image -> createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
      renderPass <- createRenderPass dev swapInfo depthFormat msaaSamples
      graphicsPipeline
        <- createGraphicsPipeline dev swapInfo
                                  vertIBD vertIADs
                                  [shaderVert, shaderFrag]
                                  renderPass
                                  pipelineLayout
                                  msaaSamples

      (colorAttSem, colorAttImgView) <- createColorAttImgView cap
                          (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
      (depthAttSem, depthAttImgView) <- createDepthAttImgView cap
                          (swapExtent swapInfo) msaaSamples
      sems <- takeMVar nextSems
      putMVar nextSems ((colorAttSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                                 : (depthAttSem, VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                                 : sems)
      framebuffers
        <- createFramebuffers dev renderPass swapInfo swapImgViews depthAttImgView colorAttImgView

      objTransformsRef <- newIORef [translate3 $ vec3 0 1 1, translate3 $ vec3 0 0 0]

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
            , recCmdBuffer       = recordAll graphicsPipeline
                                        renderPass pipelineLayout (swapExtent swapInfo)
                                        objs objTransformsRef
            , getViewProjMatrix = viewProjMatrix (swapExtent swapInfo)
            -- , frameDescrSets
            , framebuffers
            }

      logInfo $ "Createad swapchain image views: " ++ show swapImgViews
      logInfo $ "Createad renderpass: " ++ show renderPass
      logInfo $ "Createad pipeline: " ++ show graphicsPipeline
      logInfo $ "Createad framebuffers: " ++ show framebuffers

      -- part of dumb fps counter
      frameCount :: IORef Int <- newIORef 0
      currentSec :: IORef Int <- newIORef 0

      shouldExit <- glfwMainLoop window $ do
        objMatrix <- objMatrixOverTime
        writeIORef objTransformsRef [(translate3 $ vec3 0 1 1) %* objMatrix, objMatrix]

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
      -- using Event here properly deals with multiple waiting threads, in contrast to using plain MVars
      else liftIO $ sequence_ $ replicate 2 $ Event.wait frameFinishedEvent
      -- logInfo "Finished waiting after main loop termination before deallocating."
  return ()

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
