{-# LANGUAGE Strict           #-}
module Vulkyrie.Vulkan.Device
    ( pickPhysicalDevice
    , isDeviceSuitable
    , getMaxUsableSampleCount
    , DevQueues (..)
    , SwapchainSupportDetails (..)
    , querySwapchainSupport
    , checkDeviceExtensionSupport
    , createGraphicsDevice
    ) where

import           Control.Monad
import           Data.Bits
import           Data.List                            ((\\), sortOn, intersect)
import           Data.Ord
import qualified Data.Map                             as Map
import           Foreign.C.String                     (peekCString)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Vulkyrie.Program
import           Vulkyrie.Program.Foreign

data SwapchainSupportDetails
  = SwapchainSupportDetails
  { capabilities :: VkSurfaceCapabilitiesKHR
  , formats      :: [VkSurfaceFormatKHR]
  , presentModes :: [VkPresentModeKHR]
  } deriving (Eq, Show)


pickPhysicalDevice :: VkInstance
                   -> Maybe VkSurfaceKHR
                   -> Program r (Maybe SwapchainSupportDetails, VkPhysicalDevice)
pickPhysicalDevice vkInstance mVkSurf = do
    devs <- asListVk
      $ \x -> runVk . vkEnumeratePhysicalDevices vkInstance x

    when (null devs) $ throwVkMsg "Zero device count!"
    logInfo $ "Found " ++ show (length devs) ++ " devices."

    suitableDevs <- filterM (fmap snd . isDeviceSuitable mVkSurf) devs
    when (null suitableDevs) $ throwVkMsg "No suitable devices!"
    scoredDevs <- mapM (\dev -> deviceScore dev >>= \score -> return (score, dev)) suitableDevs
    let dev = snd . head $ sortOn (Down . fst) scoredDevs
    (mscsd, _) <- isDeviceSuitable mVkSurf dev
    return (mscsd, dev)


deviceScore :: VkPhysicalDevice -> Program r Int
deviceScore pdev = do
  devProps <- allocaPeek $ \propsPtr ->
    liftIO $ vkGetPhysicalDeviceProperties pdev propsPtr
  let deviceType = getField @"deviceType" devProps
  if deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU then return 100 else return 0


querySwapchainSupport :: VkPhysicalDevice
                      -> VkSurfaceKHR
                      -> Program r SwapchainSupportDetails
querySwapchainSupport pdev surf = do

  capabilities <- allocaPeekVk
    $ runVk . vkGetPhysicalDeviceSurfaceCapabilitiesKHR pdev surf

  formats <- asListVk
    $ \x -> runVk . vkGetPhysicalDeviceSurfaceFormatsKHR pdev surf x

  presentModes <- asListVk
    $ \x -> runVk . vkGetPhysicalDeviceSurfacePresentModesKHR pdev surf x

  return SwapchainSupportDetails {..}


checkDeviceExtensionSupport :: VkPhysicalDevice
                            -> [CString]
                            -> Program r Bool
checkDeviceExtensionSupport pdev extensions = do
  reqExts <- liftIO $ mapM peekCString extensions
  availExtsC <- asListVk
    $ \x -> runVk . vkEnumerateDeviceExtensionProperties pdev VK_NULL_HANDLE x
  availExts <- forM availExtsC . flip withVkPtr $
    liftIO . peekCString
           . ( `plusPtr` fieldOffset @"extensionName" @VkExtensionProperties)
  logInfo $ "available extensions: " ++ unlines availExts
  return . null $ reqExts \\ availExts



isDeviceSuitable :: Maybe VkSurfaceKHR
                 -> VkPhysicalDevice
                 -> Program r (Maybe SwapchainSupportDetails, Bool)
isDeviceSuitable mVkSurf pdev = do
  extsGood <- checkDeviceExtensionSupport pdev
    [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

  (mscsd, surfGood) <- case mVkSurf of
    Nothing -> pure (Nothing, True)
    Just vkSurf
      | not extsGood -> pure (Nothing, False)
      | otherwise -> do
      scsd@SwapchainSupportDetails {..} <- querySwapchainSupport pdev vkSurf
      return  ( Just scsd
              ,    not (null formats)
                 && not (null presentModes)
              )

  supportedFeatures <- allocaPeek $
    liftIO . vkGetPhysicalDeviceFeatures pdev

  let supportsAnisotropy = getField @"samplerAnisotropy" supportedFeatures == VK_TRUE
  -- let supportsImgArray = getField @"shaderSampledImageArrayDynamicIndexing" supportedFeatures == VK_TRUE
  -- logInfo $ "device support for img array: " ++ show supportsImgArray

  -- pure (mscsd, extsGood && surfGood && supportsAnisotropy && supportsImgArray)
  pure (mscsd, extsGood && surfGood && supportsAnisotropy)



getMaxUsableSampleCount :: VkPhysicalDevice
                        -> Program r VkSampleCountFlagBits
getMaxUsableSampleCount pdev = do
  devProps <- allocaPeek $ \propsPtr ->
    liftIO $ vkGetPhysicalDeviceProperties pdev propsPtr
  let limits = getField @"limits" devProps
      colorSampleCounts = getField @"framebufferColorSampleCounts" limits
      depthSampleCounts = getField @"framebufferDepthSampleCounts" limits
      counts = min colorSampleCounts depthSampleCounts
      splitCounts = filter ((/= VK_ZERO_FLAGS) . (counts .&.))
        [ VK_SAMPLE_COUNT_64_BIT
        , VK_SAMPLE_COUNT_32_BIT
        , VK_SAMPLE_COUNT_16_BIT
        , VK_SAMPLE_COUNT_8_BIT
        , VK_SAMPLE_COUNT_4_BIT
        , VK_SAMPLE_COUNT_2_BIT
        , VK_SAMPLE_COUNT_1_BIT
        ]
      highestCount = head splitCounts
      -- need to convert from "VkSampleCountBitmask FlagMask" to "VkSampleCountBitmask FlagBit"
      VkSampleCountBitmask rawFlags = highestCount
      result = VkSampleCountBitmask rawFlags
  -- logInfo $ show limits
  return result



getQueueFamilies :: VkPhysicalDevice -> Program r [(Word32, VkQueueFamilyProperties)]
getQueueFamilies pdev = do
  fams <- asListVk
    $ \c -> liftIO . vkGetPhysicalDeviceQueueFamilyProperties pdev c
  when (null fams) $ throwVkMsg "Zero queue family count!"
  return $ zip [0..] fams


isGraphicsFam :: (Word32, VkQueueFamilyProperties) -> Bool
isGraphicsFam (_, qfp)
  = getField @"queueFlags" qfp .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits

isPresentationFam :: VkPhysicalDevice
                  -> VkSurfaceKHR
                  -> (Word32, VkQueueFamilyProperties)
                  -> Program r Bool
isPresentationFam dev surf (i, _)
  = do
    supported <- allocaPeek $
      runVk . vkGetPhysicalDeviceSurfaceSupportKHR dev i surf
    return (supported == VK_TRUE)


data DevQueues
  = DevQueues
  { graphicsQueue  :: VkQueue
  , presentQueue   :: VkQueue
  , qFamIndices    :: Ptr Word32
  , graphicsFamIdx :: Word32
  , presentFamIdx  :: Word32
  } deriving (Eq, Show)


createGraphicsDevice :: VkPhysicalDevice
                     -> VkSurfaceKHR
                     -> Program r (VkDevice, DevQueues)
createGraphicsDevice pdev surf
  | extensions <- [VK_KHR_SWAPCHAIN_EXTENSION_NAME] = do
  -- check physical device extensions

  -- find an appropriate queue family
  qfams <- getQueueFamilies pdev
  let gfxFams = map fst $ filter isGraphicsFam qfams
  when (null gfxFams) $ throwVkMsg "No graphics queue family found."
  presFams <- map fst <$> filterM (isPresentationFam pdev surf) qfams
  when (null presFams) $ throwVkMsg "No presentation queue family found for the surface."
  -- It's best for performance if presentation can happen in the graphics queue
  let bestFams = gfxFams `intersect` presFams
  let (gfxFamIdx, presFamIdx) =
        case bestFams of
          fam:_ -> (fam, fam)
          [] -> (head gfxFams, head presFams)
      qFamIndices = if gfxFamIdx == presFamIdx then [gfxFamIdx] else [gfxFamIdx, presFamIdx]
  famIndsPtr <- newArrayRes qFamIndices

  let qcInfoMap = Map.fromList $ flip fmap qFamIndices $ \qFamIdx -> (qFamIdx, qCreateInfo qFamIdx)
        where qCreateInfo qFamIdx = createVk @VkDeviceQueueCreateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"flags" VK_ZERO_FLAGS
                &* set @"queueFamilyIndex" qFamIdx
                &* set @"queueCount" 1
                &* setListRef @"pQueuePriorities" [1.0]

      pdevFeatures = createVk @VkPhysicalDeviceFeatures
        $  set @"samplerAnisotropy" VK_TRUE
        -- &* set @"shaderSampledImageArrayDynamicIndexing" VK_TRUE

      devCreateInfo = createVk @VkDeviceCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* setListRef @"pQueueCreateInfos" (Map.elems qcInfoMap)
        &* set @"queueCreateInfoCount" (fromIntegral $ Map.size qcInfoMap)
        &* set @"enabledLayerCount" 0 -- deprecated
        &* setStrListRef @"ppEnabledLayerNames" [] -- deprecated
        &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
        &* setListRef @"ppEnabledExtensionNames" extensions
        &* setVkRef @"pEnabledFeatures" pdevFeatures

  -- try to create a device
  dev <- allocResource (\dev -> liftIO $ vkDestroyDevice dev VK_NULL) $
    withVkPtr devCreateInfo $ \dciPtr ->
      allocaPeek $ runVk . vkCreateDevice pdev dciPtr VK_NULL

  -- get the queues
  gQueues <- flip Map.traverseWithKey qcInfoMap $ \qFamIdx _ ->
             allocaPeek $ liftIO . vkGetDeviceQueue dev qFamIdx 0

  mdevQueues <- maybe (throwVkMsg "Some queues lost!") pure
                $ DevQueues
               <$> Map.lookup gfxFamIdx gQueues
               <*> Map.lookup presFamIdx gQueues
               <*> Just famIndsPtr
               <*> Just gfxFamIdx
               <*> Just presFamIdx

  return (dev, mdevQueues)
