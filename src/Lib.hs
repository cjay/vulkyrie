{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
  ( runVulkanProgram
  ) where

import qualified Control.Concurrent.Event             as Event
import           Control.Monad
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame

import           Lib.Engine.Simple3D
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
import           Lib.Vulkan.Image
import           Lib.Vulkan.Memory
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Queue
import           Lib.Vulkan.Shader
import           Lib.Vulkan.Sync
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


viewProjMatrix :: VkExtent2D -> Program r Mat44f
viewProjMatrix extent = do
  let width = getField @"width" extent
  let height = getField @"height" extent
  let aspectRatio = fromIntegral width / fromIntegral height
  let view = lookAt (vec3 0 0 (-1)) (vec3 2 2 2) (vec3 0 0 0)
  let proj = perspective 0.1 20 (45/360*2*pi) aspectRatio
  return $ view %* proj


runVulkanProgram :: IO ()
runVulkanProgram = runProgram checkStatus $ do
  windowSizeChanged <- newIORef False
  window <- initGLFWWindow 800 600 "vulkan-experiment" windowSizeChanged

  vulkanInstance <- auto $ createGLFWVulkanInstance "vulkan-experiment-instance"

  vulkanSurface <- auto $ createSurface vulkanInstance window
  logInfo $ "Createad surface: " ++ show vulkanSurface

  glfwWaitEventsMeanwhile $ do
    (_, pdev) <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " ++ show pdev
    msaaSamples <- getMaxUsableSampleCount pdev

    (dev, queues) <- createGraphicsDevice pdev vulkanSurface

    msp <- auto $ metaMasterSemaphorePool dev
    gfxQueue <- auto $ metaManagedQueue dev (graphicsQueue queues) msp
    attachQueuePump gfxQueue 16666
    cpp <- auto $ metaCommandPoolPool dev (graphicsFamIdx queues)

    semPool <- auto $ metaSemaphorePool msp
    cmdCap <- auto $ metaCommandCapability cpp
    memPool <- auto $ metaMemoryPool pdev dev
    -- TODO create permanently mapped reusable staging buffer
    let cap = EngineCapability{ pdev, dev, cmdCap, cmdQueue=gfxQueue, semPool, memPool }

    logInfo $ "Createad device: " ++ show dev
    logInfo $ "Createad queues: " ++ show queues

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

    frameIndexRef <- newIORef 0
    renderFinishedSems <- createFrameSemaphores dev
    queueEvents <- sequence $ replicate maxFramesInFlight $ newDoneQueueEvent >>= newIORef
    frameFinishedEvent <- liftIO $ Event.new
    frameOnQueueVars <- sequence $ replicate maxFramesInFlight $ newEmptyMVar

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIndexPtr <- mallocRes

    let vertices = rectVertices
        indices = rectIndices

    (vertexBufReady, vertexBuffer) <-
      auto $ createVertexBuffer cap vertices

    (indexBufReady, indexBuffer) <- auto $ createIndexBuffer cap indices

    frameDSL <- auto $ createDescriptorSetLayout dev [] --[uniformBinding 0]
    -- TODO automate bind ids
    materialDSL <- auto $ createDescriptorSetLayout dev [samplerBinding 0]
    pipelineLayout <- auto $ createPipelineLayout dev
      -- descriptor set numbers 0,1,..
      [frameDSL, materialDSL]
      -- push constant ranges
      [ pushConstantRange VK_SHADER_STAGE_VERTEX_BIT 0 64
      ]

    let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg"]
    (textureReadyEvents, descrTextureInfos) <- auto $ unzip <$> mapM
      (createTextureInfo cap) texturePaths

    let loadEvents = textureReadyEvents <> [vertexBufReady, indexBufReady]

    depthFormat <- findDepthFormat pdev

    -- (transObjMems, transObjBufs) <- unzip <$> uboCreateBuffers pdev dev transObjSize maxFramesInFlight
    -- descriptorBufferInfos <- mapM (uboBufferInfo transObjSize) transObjBufs

    descriptorPool <- auto $ createDescriptorPool dev 100 -- TODO make dynamic
    -- frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL
    materialDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

    -- forM_ (zip descriptorBufferInfos frameDescrSets) $
      -- \(bufInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [bufInfo] []

    forM_ (zip descrTextureInfos materialDescrSets) $
      \(texInfo, descrSet) -> updateDescriptorSet dev descrSet 0 [] [texInfo]

    let objs =
          [ Object
            { materialBindInfo = DescrBindInfo (materialDescrSets !! 0) []
            , vertexBufferLoc = BufferLoc vertexBuffer 0
            , indexBufferLoc = BufferLoc indexBuffer 0
            , firstIndex = 0
            , indexCount = dfLen indices
            }
          , Object
            { materialBindInfo = DescrBindInfo (materialDescrSets !! 1) []
            , vertexBufferLoc = BufferLoc vertexBuffer 0
            , indexBufferLoc = BufferLoc indexBuffer 0
            , firstIndex = 0
            , indexCount = dfLen indices
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

    removeQueuePump gfxQueue

    nextSems <- newMVar []

    -- The code below re-runs when the swapchain was re-created
    asyncRedo $ \redoWithNewSwapchain -> do
      logInfo "New thread: Creating things that depend on the swapchain.."
      -- need this for delayed destruction of the old swapchain if it gets replaced
      oldSwapchainSlot <- createSwapchainSlot dev
      swapInfo <- readIORef swapInfoRef
      swapImgViews <- auto $ mapM (\image -> createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
      renderPass <- auto $ createRenderPass dev swapInfo depthFormat msaaSamples
      graphicsPipeline
        <- auto $ createGraphicsPipeline dev swapInfo
                                  vertIBD vertIADs
                                  [shaderVert, shaderFrag]
                                  renderPass
                                  pipelineLayout
                                  msaaSamples

      (colorAttSem, colorAttImgView) <- auto $ createColorAttImgView cap
                          (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
      (depthAttSem, depthAttImgView) <- auto $ createDepthAttImgView cap
                          (swapExtent swapInfo) msaaSamples
      sems <- takeMVar nextSems
      putMVar nextSems ((colorAttSem, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                                 : (depthAttSem, VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                                 : sems)
      framebuffers
        <- auto $ createFramebuffers dev renderPass swapInfo swapImgViews depthAttImgView colorAttImgView

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
                                        loadEvents objs objTransformsRef
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
        writeIORef objTransformsRef [objMatrix %* (translate3 $ vec3 0 1 1) , objMatrix]

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
            -- exitFailure
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
