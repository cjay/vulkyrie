{-# LANGUAGE Strict #-}
module Vulkyrie.Vulkan.Instance
    ( createVulkanInstance
    ) where

import           Data.Text                            (pack)
import qualified Data.Text as Text
import           Foreign.C.String               (peekCString)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign
import           Vulkyrie.Resource

-- | Run an action with vulkan instance
createVulkanInstance :: String -- ^ application name
                     -> String -- ^ engine name
                     -> [CString]
                        -- ^ required extensions
                        --   passed as a list of CStrings, because they are
                        --   available either via vulkan-api pattern synonyms,
                        --   or from GLFW
                     -> [String]
                        -- ^ required layer names
                     -> Resource VkInstance
createVulkanInstance progName engineName extensions layers =
  resource $ metaResource destroyVulkanInstance $ do

    extStrings <- liftIO $ mapM (fmap pack . peekCString) extensions
    logDebug $ Text.unlines
      $ "Enabling instance extensions: " : map ("  " <>) extStrings

    logDebug $ Text.unlines
      $ "Enabling instance layers: " : map (("  " <>) . pack) layers

    withVkPtr iCreateInfo $ \iciPtr ->
      allocaPeek $ runVk . vkCreateInstance iciPtr VK_NULL
  where
    appInfo = createVk @VkApplicationInfo
      $  set       @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      &* set       @"pNext" VK_NULL
      &* setStrRef @"pApplicationName" progName
      &* set       @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
      &* setStrRef @"pEngineName" engineName
      &* set       @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
      &* set       @"apiVersion" (_VK_MAKE_VERSION 1 0 68)

    iCreateInfo = createVk @VkInstanceCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* setVkRef      @"pApplicationInfo" appInfo
      &* set           @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set           @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef    @"ppEnabledExtensionNames" extensions

destroyVulkanInstance :: VkInstance -> Prog r ()
destroyVulkanInstance vkInstance
  = liftIO (vkDestroyInstance vkInstance VK_NULL) >> (logDebug "Destroyed vkInstance.")
