{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE Strict              #-}
module Lib.Vulkan.TransformationObject
  ( updateTransObj
  , transObjSize
  ) where

import           GHC.Generics                   (Generic)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Numeric.DataFrame

import           Lib.Program
import           Lib.Program.Foreign


data TransformationObject = TransformationObject
  { model :: Mat44f
  , view  :: Mat44f
  , proj  :: Mat44f
  } deriving (Show, Generic)

instance PrimBytes TransformationObject

rotation :: Double -> Mat44f
rotation seconds =
  let rate = 1/16 -- rotations per second
      (_::Int, phaseTau) = properFraction $ seconds * rate
  in rotate (vec3 0 0 1) (realToFrac phaseTau * 2 * pi)

updateTransObj :: VkExtent2D -> Program r TransformationObject
updateTransObj extent = do
  seconds <- getTime
  let width = getField @"width" extent
  let height = getField @"height" extent
  let aspectRatio = fromIntegral width / fromIntegral height
  return TransformationObject
        { model = rotation seconds
        , view = lookAt (vec3 0 0 (-1)) (vec3 2 2 2) (vec3 0 0 0)
        , proj = perspective 0.1 20 (45/360*2*pi) aspectRatio
        }

transObjSize :: VkDeviceSize
transObjSize = fromIntegral $ sizeOf @(Scalar TransformationObject) undefined
