{-# LANGUAGE Strict           #-}
module Lib.Vulkan.RenderPass
  ( createRenderPass
  , createFramebuffers
  , createRenderPassBeginInfo
  ) where

import           Data.Bits
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.Vector

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Resource
import           Lib.Vulkan.Presentation



createRenderPass :: VkDevice
                 -> SwapchainInfo
                 -> VkFormat
                 -> VkSampleCountFlagBits
                 -> Resource r VkRenderPass
createRenderPass dev SwapchainInfo{ swapImgFormat } depthFormat samples =
  let -- attachment description
      colorAttachment = createVk @VkAttachmentDescription
        $  set @"flags" VK_ZERO_FLAGS
        &* set @"format" swapImgFormat
        &* set @"samples" samples
        &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
        &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
        &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set @"finalLayout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

      depthAttachment = createVk @VkAttachmentDescription
        $  set @"flags" VK_ZERO_FLAGS
        &* set @"format" depthFormat
        &* set @"samples" samples
        &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
        &* set @"storeOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set @"finalLayout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL

      colorAttachmentResolve = createVk @VkAttachmentDescription
        $  set @"flags" VK_ZERO_FLAGS
        &* set @"format" swapImgFormat
        &* set @"samples" VK_SAMPLE_COUNT_1_BIT
        &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
        &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

      -- subpasses and attachment references
      colorAttachmentRef = createVk @VkAttachmentReference
        $  set @"attachment" 0
        &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

      depthAttachmentRef = createVk @VkAttachmentReference
        $  set @"attachment" 1
        &* set @"layout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL

      colorAttachmentResolveRef = createVk @VkAttachmentReference
        $  set @"attachment" 2
        &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

      subpass = createVk @VkSubpassDescription
        $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
        &* set @"colorAttachmentCount" 1
        &* setVkRef @"pColorAttachments" colorAttachmentRef
        &* setVkRef @"pDepthStencilAttachment" depthAttachmentRef
        &* setVkRef @"pResolveAttachments" colorAttachmentResolveRef
        &* set @"pPreserveAttachments" VK_NULL
        &* set @"pInputAttachments" VK_NULL

      -- subpass dependencies
      dependency = createVk @VkSubpassDependency
        $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
        &* set @"dstSubpass" 0
        &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set @"srcAccessMask" VK_ZERO_FLAGS
        &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set @"dstAccessMask"
            (   VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
            .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )

      -- render pass
      rpCreateInfo = createVk @VkRenderPassCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* setListCountAndRef @"attachmentCount" @"pAttachments"
            [colorAttachment, depthAttachment, colorAttachmentResolve]
        &* set @"subpassCount" 1
        &* setVkRef @"pSubpasses" subpass
        &* set @"dependencyCount" 1
        &* setVkRef @"pDependencies" dependency

  in resource $ metaResource
       (\rp -> liftIO $ vkDestroyRenderPass dev rp VK_NULL) $
       withVkPtr rpCreateInfo $ \rpciPtr -> allocaPeek $
         runVk . vkCreateRenderPass dev rpciPtr VK_NULL


createFramebuffers :: VkDevice
                   -> VkRenderPass
                   -> SwapchainInfo
                   -> [VkImageView]
                   -> VkImageView
                   -> VkImageView
                   -> Resource r [VkFramebuffer]
createFramebuffers dev renderPass SwapchainInfo{ swapExtent } swapImgViews depthImgView colorImgView =
  resource $ metaResource
    (liftIO . mapM_  (\fb -> vkDestroyFramebuffer dev fb VK_NULL) )
    (mapM createFB swapImgViews)
  where
    createFB swapImgView =
      let fbci = createVk @VkFramebufferCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"renderPass" renderPass
            &* setListCountAndRef @"attachmentCount" @"pAttachments" [colorImgView, depthImgView, swapImgView]
            &* set @"width" (getField @"width" swapExtent)
            &* set @"height" (getField @"height" swapExtent)
            &* set @"layers" 1
      in allocaPeek $ \fbPtr -> withVkPtr fbci $ \fbciPtr ->
          runVk $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr


createRenderPassBeginInfo :: VkRenderPass -> VkFramebuffer -> VkExtent2D -> VkRenderPassBeginInfo
createRenderPassBeginInfo renderPass framebuffer extent = createVk @VkRenderPassBeginInfo
      $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
      &* set @"pNext" VK_NULL
      &* set @"renderPass" renderPass
      &* set @"framebuffer" framebuffer
      &* setVk @"renderArea"
          (  setVk @"offset"
              ( set @"x" 0 &* set @"y" 0 )
          &* set @"extent" extent
          )
      &* setListCountAndRef @"clearValueCount" @"pClearValues"
          [  createVk @VkClearValue
             $ setVk @"color"
             $ setVec @"float32" (vec4 0 0 0.2 1)
          ,  createVk @VkClearValue
             $ setVk @"depthStencil"
             $  set @"depth" 1.0
             &* set @"stencil" 0
          ]
