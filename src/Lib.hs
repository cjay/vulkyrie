{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Lib
  ( runVulkanProgram
  ) where

import qualified Control.Concurrent.Event             as Event
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.IORef
import           Data.Maybe                           (fromJust)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame
import           Numeric.Dimensions

import           Lib.GLFW
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Command
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Image
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Shader
import           Lib.Vulkan.Shader.TH
import           Lib.Vulkan.TransformationObject
import           Lib.Vulkan.Vertex
import           Lib.Vulkan.VertexBuffer


-- | Interleaved array of vertices containing at least 3 entries.
--
--   Obviously, in real world vertices come from a separate file and not known at compile time.
--   The shader pipeline requires at least 3 unique vertices (for a triangle)
--   to render something on a screen. Setting `XN 3` here is just a handy way
--   to statically ensure the program satisfies this requirement.
--   This way, not-enough-vertices error occures at the moment of DataFrame initialization
--   instead of silently failing to render something onto a screen.
--
--   Note: in this program, `n >= 3` requirement is also forced in `Lib/Vulkan/VertexBuffer.hs`,
--         where it is not strictly necessary but allows to avoid specifying DataFrame constraints
--         in function signatures (such as, e.g. `KnownDim n`).
rectVertices :: DataFrame Vertex '[XN 3]
rectVertices = fromJust $ fromList (D @3)
  [ -- rectangle
    --              coordinate                  color        texture coordinate
    scalar $ Vertex (vec3 (-0.5) (-0.5)   0.0 ) (vec3 1 0 0) (vec2 0 0)
  , scalar $ Vertex (vec3   0.4  (-0.5)   0.0 ) (vec3 0 1 0) (vec2 1 0)
  , scalar $ Vertex (vec3   0.4    0.4    0.0 ) (vec3 0 0 1) (vec2 1 1)
  , scalar $ Vertex (vec3 (-0.5)   0.4    0.0 ) (vec3 1 1 1) (vec2 0 1)

    -- rectangle
    --              coordinate                  color        texture coordinate
  , scalar $ Vertex (vec3 (-0.5) (-0.5) (-0.5)) (vec3 1 0 0) (vec2 0 0)
  , scalar $ Vertex (vec3   0.4  (-0.5) (-0.5)) (vec3 0 1 0) (vec2 1 0)
  , scalar $ Vertex (vec3   0.4    0.4  (-0.5)) (vec3 0 0 1) (vec2 1 1)
  , scalar $ Vertex (vec3 (-0.5)   0.4  (-0.5)) (vec3 1 1 1) (vec2 0 1)
  ]

rectIndices :: DataFrame Word32 '[XN 3]
rectIndices = fromJust $ fromList (D @3)
  [ -- rectangle
    0, 1, 2, 2, 3, 0
  ]

rect2Indices :: DataFrame Word32 '[XN 3]
rect2Indices = fromJust $ fromList (D @3)
  [ -- rectangle
    4, 5, 6, 6, 7, 4
  ]

runVulkanProgram :: IO ()
runVulkanProgram = runProgram checkStatus $ do
  windowSizeChanged <- liftIO $ newIORef False
  window <- initGLFWWindow 800 600 "vulkan-triangles-GLFW" windowSizeChanged

  vulkanInstance <- createGLFWVulkanInstance "vulkan-triangles-instance"

  vulkanSurface <- createSurface vulkanInstance window
  logInfo $ "Createad surface: " ++ show vulkanSurface

  -- TODO terminate when normal exception happens?
  glfwWaitEventsMeanwhile $ do
    (_, pdev) <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " ++ show pdev
    msaaSamples <- getMaxUsableSampleCount pdev

    (dev, queues) <- createGraphicsDevice pdev vulkanSurface
    let gfxQ = graphicsQueue queues
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

    frameIndexRef <- liftIO $ newIORef 0
    renderFinishedSems <- createFrameSemaphores dev
    imageAvailableSems <- createFrameSemaphores dev
    inFlightFences <- createFrameFences dev
    frameFinishedEvent <- liftIO $ Event.new
    frameOnQueueVars <- liftIO $ sequence $ replicate maxFramesInFlight $ newEmptyMVar

    cmdPool <- createCommandPool dev queues
    logInfo $ "Createad command pool: " ++ show cmdPool

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIndexPtr <- mallocRes

    let vertices = rectVertices
        indicesObjs = [rectIndices, rect2Indices]
        objects :: [(Word32, DataFrame Word32 '[XN 3])]
        objects = map (\ixs -> (fromIntegral $ dimSize1 ixs, ixs)) indicesObjs

    vertexBuffer <-
      createVertexBuffer pdev dev cmdPool gfxQ vertices

    indexBuffers <- forM objects $ mapM $ \indices ->
      createIndexBuffer pdev dev cmdPool gfxQ indices

    frameDSL <- createDescriptorSetLayout dev [uniformBinding 0]
    materialDSL <- createDescriptorSetLayout dev [samplerBinding 0]
    pipelineLayout <- createPipelineLayout dev [frameDSL, materialDSL]

    let texturePaths = map ("textures/" ++) ["texture.jpg", "texture2.jpg"]
    descrTextureInfos <- mapM
      (createTextureInfo pdev dev cmdPool gfxQ) texturePaths

    depthFormat <- findDepthFormat pdev

    (transObjMems, transObjBufs) <- unzip <$> createTransObjBuffers pdev dev maxFramesInFlight
    descriptorBufferInfos <- mapM transObjBufferInfo transObjBufs

    descriptorPool <- createDescriptorPool dev $ maxFramesInFlight * (1 + length descrTextureInfos)
    frameDescrSets <- allocateDescriptorSetsForLayout dev descriptorPool maxFramesInFlight frameDSL
    materialDescrSetsPerFrame <- sequence $ replicate maxFramesInFlight $
      allocateDescriptorSetsForLayout dev descriptorPool (length descrTextureInfos) materialDSL

    forM_ (zip descriptorBufferInfos frameDescrSets) $
      \(bufInfo, descrSet) -> prepareDescriptorSet dev descrSet 0 [bufInfo] []

    forM_ materialDescrSetsPerFrame $ \materialDescrSets ->
      forM_ (zip descrTextureInfos materialDescrSets) $
        \(texInfo, descrSet) -> prepareDescriptorSet dev descrSet 0 [] [texInfo]

    transObjMemories <- newArrayRes $ transObjMems

    let beforeSwapchainCreation :: Program r ()
        beforeSwapchainCreation = do
          -- wait as long as window has width=0 and height=0
          -- commented out because this only works in the main thread:
          -- glfwWaitMinimized window

          -- If a window size change did happen, it will be respected by (re-)creating
          -- the swapchain below, no matter if it was signalled via exception or
          -- the IORef, so reset the IORef now:
          liftIO $ atomicWriteIORef windowSizeChanged False

    -- creating first swapchain before loop
    beforeSwapchainCreation
    scsd <- querySwapchainSupport pdev vulkanSurface

    swapchainSlot <- createSwapchainSlot dev
    swapInfoRef <- createSwapchain dev scsd queues vulkanSurface swapchainSlot Nothing >>= liftIO . newIORef

    -- The code below re-runs when the swapchain was re-created
    asyncRedo $ \redoWithNewSwapchain -> do
      logInfo "New thread: Creating things that depend on the swapchain, not only its length.."
      -- need this for delayed destruction of the old swapchain if it gets replaced
      oldSwapchainSlot <- createSwapchainSlot dev
      swapInfo <- liftIO $ readIORef swapInfoRef
      swapImgViews <- mapM (\image -> createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
      renderPass <- createRenderPass dev swapInfo depthFormat msaaSamples
      graphicsPipeline
        <- createGraphicsPipeline dev swapInfo
                                  vertIBD vertIADs
                                  [shaderVert, shaderFrag]
                                  renderPass
                                  pipelineLayout
                                  msaaSamples

      colorAttImgView <- createColorAttImgView pdev dev cmdPool gfxQ
                          (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
      depthAttImgView <- createDepthAttImgView pdev dev cmdPool gfxQ
                          (swapExtent swapInfo) msaaSamples
      framebuffers
        <- createFramebuffers dev renderPass swapInfo swapImgViews depthAttImgView colorAttImgView

      dynCmdBuffers <- sequence $ replicate maxFramesInFlight $ allocateCommandBuffer dev cmdPool

      let rdata = RenderData
            { dev
            , swapInfo
            , queues
            , imgIndexPtr
            , frameIndexRef
            , renderFinishedSems
            , imageAvailableSems
            , inFlightFences
            , frameFinishedEvent
            , frameOnQueueVars
            , memories           = transObjMemories
            , memoryMutator      = updateTransObj dev (swapExtent swapInfo)
            -- , descrSetMutator    = updateDescrSet dev descrTextureInfos
            , dynCmdBuffers
            , recCmdBuffer       = recordCommandBuffer graphicsPipeline
                                        renderPass pipelineLayout swapInfo
                                        vertexBuffer indexBuffers
            , frameDescrSets
            , materialDescrSetsPerFrame
            , framebuffers
            }

      logInfo $ "Createad swapchain image views: " ++ show swapImgViews
      logInfo $ "Createad renderpass: " ++ show renderPass
      logInfo $ "Createad pipeline: " ++ show graphicsPipeline
      logInfo $ "Createad framebuffers: " ++ show framebuffers

      -- part of dumb fps counter
      frameCount <- liftIO $ newIORef @Int 0
      currentSec <- liftIO $ newIORef @Int 0

      shouldExit <- glfwMainLoop window $ do
        return () -- do some app logic

        needRecreation <- drawFrame rdata `catchError` ( \err@(VulkanException ecode _) ->
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
            modifyIORef frameCount $ \c -> c + 1

        sizeChanged <- liftIO $ readIORef windowSizeChanged
        when sizeChanged $ logInfo "Have got a windowSizeCallback from GLFW"
        if needRecreation || sizeChanged then do
          beforeSwapchainCreation
          logInfo "Recreating swapchain.."
          newScsd <- querySwapchainSupport pdev vulkanSurface
          newSwapInfo <- createSwapchain dev newScsd queues vulkanSurface swapchainSlot (Just oldSwapchainSlot)
          liftIO $ atomicWriteIORef swapInfoRef newSwapInfo
          redoWithNewSwapchain
          return AbortLoop
        else return ContinueLoop
      -- after glfwMainLoop exits, we need to wait for the frame to finish before deallocating things
      if shouldExit
      then runVk $ vkDeviceWaitIdle dev
      -- using Event here properly deals with multiple waiting threads, in contrast to using plain MVars
      else liftIO $ sequence_ $ replicate 2 $ Event.wait frameFinishedEvent
      -- logInfo "Finished waiting after main loop termination before deallocating."
  return ()


-- TODO not needed right now
updateDescrSet :: VkDevice
               -> [VkDescriptorImageInfo]
               -> VkDescriptorSet
               -> Program r ()
updateDescrSet dev texInfos descrSet = do
  -- seconds <- getTime
  -- let texIx = floor seconds `mod` 2
  -- prepareDescriptorSet dev descrSet 1 [] [texInfos !! texIx]
  prepareDescriptorSet dev descrSet 1 [] [texInfos !! 0]
