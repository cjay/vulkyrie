{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Vulkyrie.Engine.Pipeline where

import Data.Kind (Type)
import Data.Type.Lits (type (+), natVal, KnownNat)
import Data.Proxy ( Proxy(Proxy) )
import GHC.TypeLits (Nat, ErrorMessage (ShowType))
import Type.Errors ( TypeError, ErrorMessage(Text, (:<>:)) )
import Graphics.Vulkan.Core_1_0
    ( VkPipelineLayout, VkPipelineShaderStageCreateInfo )

type NumberElems elems = NumberElems_ elems '[] 0
type family NumberElems_ (elems :: [Type]) (accum :: [(Nat, Type)]) (counter :: Nat) :: [(Nat, Type)] where
  NumberElems_ '[] accum _ = accum
  NumberElems_ (x ': xs) accum counter = NumberElems_ xs ('(counter, x) ': accum) (counter + 1)

type family LookupNumber (elem :: Type) (elems :: [(Nat, Type)]) :: Nat where
  LookupNumber elem ('(num, elem) ': _) = num
  LookupNumber elem (_ ': elems) = LookupNumber elem elems
  LookupNumber elem '[] = TypeError ('Text "LookupNumber: not found: " ':<>: ('ShowType elem))

type PipelineIndex (pipelines :: [(Nat, Type)]) (pipeline :: Type) (index :: Nat) =
  (
    index ~ LookupNumber pipeline pipelines,
    KnownNat index
  )

{- | Returns index of the given pipeline.

Each pipeline configuration (usually each in its own module under
Vulkyrie.Engine.Pipeline) needs a phantom type to represent it. A program should
have a type level list of the pipelines that it uses.  The type family
NumberElems pairs the pipeline configuration types with unique array indices.
Those indices can in turn be used to get the corresponding VkPipeline objects
out of an array.

@
data Pipeline1
data Pipeline2
type Pipelines = NumberElems '[Pipeline1, Pipeline2]
@

>>> pipelineIndex @Pipelines @Pipeline2
1

A module that only uses Pipeline2 can resolve the index through type
parameters, despite not knowing at compile time what other pipelines are in
use:
@
-- pass first type argument
doSomethingWithPipeline2 ::
  forall pipelines {index}.
  (PipelineIndex pipelines Pipeline2 index) =>
  Vector VkPipeline -> _
doSomethingWithPipeline2 pipelineObjs =
  let pipelineObj = pipelineObjs Vector.! (indexVal @index)
   in _
@
-}
pipelineIndex :: forall pipelines pipeline index. (PipelineIndex pipelines pipeline index) => Int
pipelineIndex = indexVal @index

-- | See pipelineIndex
indexVal :: forall a. KnownNat a => Int
indexVal = fromIntegral $ natVal $ Proxy @a




data ProtoPipeline =
  ProtoPipeline
  { shaderStages :: [VkPipelineShaderStageCreateInfo]
  , pipelineLayout :: VkPipelineLayout
  } deriving (Eq, Ord, Show)
