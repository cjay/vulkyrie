{-# LANGUAGE Strict           #-}
module Vulkyrie.Vulkan.Framebuffer
  ( createFramebuffer
  ) where

import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource


createFramebuffer :: VkDevice
                  -> VkRenderPass
                  -> VkExtent2D
                  -> [VkImageView]
                  -> Resource VkFramebuffer
createFramebuffer dev renderPass extent attachments =
  resource $ metaResource
    (\fb -> liftIO $ vkDestroyFramebuffer dev fb VK_NULL)
    (let fbci = createVk @VkFramebufferCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"renderPass" renderPass
            -- this needs to fit the renderpass attachments
            &* setListCountAndRef @"attachmentCount" @"pAttachments" attachments
            &* set @"width" (getField @"width" extent)
            &* set @"height" (getField @"height" extent)
            &* set @"layers" 1
      in allocaPeek $ \fbPtr -> withVkPtr fbci $ \fbciPtr ->
          runVk $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr
    )
