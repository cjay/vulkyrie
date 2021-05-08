{-# LANGUAGE Strict #-}
module Vulkyrie.Engine.Simple2D
  ( DescrBindInfo(..)
  , frameSetId
  , materialSetId
  , objectSetId
  , bindMat
  , RenderContext(..)
  , withRenderPass
  , viewProjMatrix
  , prepareRender
  ) where

import Numeric.DataFrame
    ( Mat44f,
      (%*),
      DataFrame(Vec3, Vec2),
      HomTransform4(translate3),
      Vec2f )
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource
import           Vulkyrie.Vulkan.Default.RenderPass
import           Vulkyrie.Utils (orthogonalVk)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Vulkyrie.Vulkan.Engine
import Vulkyrie.Vulkan.Presentation
import Vulkyrie.Vulkan.Device
import Vulkyrie.Vulkan.Image
import Graphics.Vulkan.Ext
import Vulkyrie.Vulkan.Framebuffer
import Vulkyrie.Engine.Pipeline


-- | cam pos using (x, y), ortho projection from z 0.1 to 10 excluding boundaries.
viewProjMatrix :: VkExtent2D -> Vec2f -> Prog r Mat44f
viewProjMatrix extent (Vec2 x y) = do
  let width :: Float = fromIntegral $ getField @"width" extent
      height :: Float = fromIntegral $ getField @"height" extent
      camPos = Vec3 x y 0
      view = translate3 (- camPos)
      camHeight = 5
      proj = orthogonalVk 0.1 10 (width/height * camHeight) camHeight
  return $ view %* proj

data DescrBindInfo = DescrBindInfo
  { descrSet       :: VkDescriptorSet
  , dynamicOffsets :: [Word32]
  }


-- Id of per-frame descriptor set
frameSetId :: Word32
frameSetId = 0

-- Id of per-material descriptor set
materialSetId :: Word32
materialSetId = 1

-- Id of per-object descriptor set
objectSetId :: Word32
objectSetId = 2

bindDescrSet :: Word32 -> VkCommandBuffer -> VkPipelineLayout -> DescrBindInfo -> Prog r ()
bindDescrSet descrSetId cmdBuf pipelineLayout DescrBindInfo{..} = region $ do
  descrSetPtr <- auto $ newArrayRes [descrSet]
  let descrSetCnt = 1
  let dynOffCnt = fromIntegral $ length dynamicOffsets
  dynOffPtr <- auto $ newArrayRes dynamicOffsets
  liftIO $ vkCmdBindDescriptorSets cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout
    descrSetId descrSetCnt descrSetPtr dynOffCnt dynOffPtr

bindMat :: VkCommandBuffer -> VkPipelineLayout -> DescrBindInfo -> Prog r ()
bindMat = bindDescrSet materialSetId


-- | Update push constants: transformation matrix
--
-- Assumes that the transformation matrix is in the push constant range.
-- pushTransformUnsafe :: VkCommandBuffer -> VkPipelineLayout -> Mat44f -> Prog r ()
-- pushTransformUnsafe = pushDF 0 64 VK_SHADER_STAGE_VERTEX_BIT

{-      not in use
-- | Update push constants: texture index
pushTexIndex :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> Program ()
pushTexIndex cmdBuf pipelineLayout texIndex = alloca $ \ptr -> do
    poke ptr texIndex
    liftIO $ vkCmdPushConstants cmdBuf pipelineLayout VK_SHADER_STAGE_FRAGMENT_BIT 64 4 (castPtr ptr)
-}


data RenderContext
  = RenderContext
  { pipelineObjs   :: Vector VkPipeline
  , renderPass     :: VkRenderPass
  , extent         :: VkExtent2D
  }

withRenderPass :: RenderContext -> VkCommandBuffer -> VkFramebuffer -> Prog r () -> Prog r ()
withRenderPass RenderContext{..} cmdBuf framebuffer prog = do
  let renderPassBeginInfo = createRenderPassBeginInfo renderPass framebuffer extent
  withVkPtr renderPassBeginInfo $ \rpbiPtr ->
    liftIO $ vkCmdBeginRenderPass cmdBuf rpbiPtr VK_SUBPASS_CONTENTS_INLINE

  prog

  liftIO $ vkCmdEndRenderPass cmdBuf

prepareRender :: EngineCapability
              -> SwapchainInfo
              -> Vector ProtoPipeline
              -> Resource ([VkFramebuffer], [(VkSemaphore, VkPipelineStageBitmask a)], RenderContext)
prepareRender cap@EngineCapability{ dev, pdev } swapInfo pipelines = Resource $ do
  let SwapchainInfo{ swapImgs, swapExtent, swapImgFormat } = swapInfo
  msaaSamples <- getMaxUsableSampleCount pdev
  -- to turn off msaa:
  -- let msaaSamples = VK_SAMPLE_COUNT_1_BIT
  depthFormat <- findDepthFormat pdev

  swapImgViews <-
    mapM (\image -> auto $ createImageView dev image swapImgFormat VK_IMAGE_ASPECT_COLOR_BIT 1) swapImgs
  renderPass <- auto $ createRenderPass dev swapImgFormat depthFormat msaaSamples VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

  pipelineObjs <- Vector.forM pipelines $ \ProtoPipeline{ createPipeline } ->
    auto $ createPipeline renderPass swapExtent msaaSamples

  (nextSems, privAttachments) <- auto $ createPrivateAttachments cap swapExtent swapImgFormat msaaSamples
  framebuffers <- mapM
    (auto . createFramebuffer dev renderPass swapExtent . (privAttachments <>) . (:[]))
    swapImgViews

  return (framebuffers, nextSems, RenderContext{ pipelineObjs, renderPass, extent = swapExtent })
