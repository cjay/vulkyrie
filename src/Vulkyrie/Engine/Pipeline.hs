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

module Vulkyrie.Engine.Pipeline where

import Data.Kind (Type, Constraint)
import Data.Tagged (Tagged(..))
import Data.Type.Lits (type (+), natVal, KnownNat)
import Data.Proxy ( Proxy(Proxy) )
import GHC.TypeLits (Nat, ErrorMessage (ShowType))
import Type.Errors ( TypeError, ErrorMessage(Text, (:<>:)) )
import Graphics.Vulkan.Core_1_0
import Vulkyrie.Resource (Resource)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.IO.Class (MonadIO (liftIO))

-- Pairs each element with a number starting at 0, usable as array indices
type NumberElems elems = NumberElems_ elems '[] 0
type family NumberElems_ (elems :: [Type]) (accum :: [(Nat, Type)]) (counter :: Nat) :: [(Nat, Type)] where
  NumberElems_ '[] accum _ = accum
  NumberElems_ (x ': xs) accum counter = NumberElems_ xs ('(counter, x) ': accum) (counter + 1)

type family LookupNumber (elem :: Type) (elems :: [(Nat, Type)]) :: Nat where
  LookupNumber elem ('(num, elem) ': _) = num
  LookupNumber elem (_ ': elems) = LookupNumber elem elems
  LookupNumber elem '[] = TypeError ('Text "LookupNumber: not found: " ':<>: ('ShowType elem))

type PipelineIndex (pipelines :: [Type]) (pipeline :: Type) (index :: Nat) =
  (
    index ~ LookupNumber pipeline (NumberElems pipelines),
    KnownNat index
  )

{- | Returns index of the given pipeline.

Each pipeline configuration (usually each in its own module under
Vulkyrie.Engine.Pipeline) needs a dummy type to represent it. A program should
have a type level list of the pipelines that it uses. Unique array indices are
derived from the order in that list, and can be used to get the corresponding
VkPipeline objects out of an array.

@
data Pipeline1
data Pipeline2
type Pipelines = '[Pipeline1, Pipeline2]
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

See `PipelineProvider` to hide the array indexing as an implementation detail.
-}
pipelineIndex :: forall pipelines pipeline index. (PipelineIndex pipelines pipeline index) => Int
pipelineIndex = indexVal @index

-- | See pipelineIndex
indexVal :: forall a. KnownNat a => Int
indexVal = fromIntegral $ natVal $ Proxy @a




data ProtoPipeline =
  ProtoPipeline
  { pipelineLayout :: VkPipelineLayout
  , createPipeline :: VkRenderPass -> VkExtent2D -> VkSampleCountFlagBits  -> Resource VkPipeline
  }

{- | Abstractly providing pipelines to pipeline using modules

@
type PipelineOrder = '[Pipeline1, Pipeline2]

data Pipelines = Pipelines
  { pipelineObjs :: Vector VkPipeline
  , pipelineLayouts :: Vector VkPipelineLayout
  }

instance (PipelineIndex PipelineOrder a pipelineIndex) => PipelineProvider Pipelines a where
  getPipeline Pipelines{ pipelineObjs, pipelineLayouts } (_ :: a) =
    ( pipelineLayouts Vector.! indexVal @pipelineIndex
    , pipelineObjs Vector.! indexVal @pipelineIndex
    )
@
-}
class PipelineProvider provider pipeline where
  getPipeline_ :: provider -> (VkPipelineLayout, VkPipeline)

-- | Allows passing the pipeline type argument first
getPipeline ::
  forall pipeline provider.
  PipelineProvider provider pipeline =>
  provider -> (VkPipelineLayout, VkPipeline)
getPipeline = getPipeline_ @provider @pipeline

type family ProvidesPipelines provider (handles :: [Type]) :: Constraint where
  ProvidesPipelines provider '[] = ()
  ProvidesPipelines provider (x ': xs) = (PipelineProvider provider x, ProvidesPipelines provider xs)


-- | Has to be used as a terminator after a `+:` chain.
--
-- If you use `Tagged []` instead of this on the value level, the type level
-- list in the tag will be inferred.  Since the type level list is supposed to
-- be checked for equality, this is dangerous. The inference will just make up
-- any missing tags at the end of the list.
nilTaggedList :: Tagged '[] [a]
nilTaggedList = Tagged []

-- | Collect all the type level tags while assembling a list
(+:) :: Tagged t a -> Tagged ts [a] -> Tagged (t ': ts) [a]
(+:) (Tagged a) (Tagged as) = Tagged (a:as)

infixr 5 +:


{-# INLINE withPipeline #-}
withPipeline ::
  forall pipeline provider m a.
  ( PipelineProvider provider pipeline
  , MonadReader VkCommandBuffer m
  , MonadIO m
  ) =>
  provider -> ReaderT VkPipelineLayout m a -> m a
withPipeline provider plAction = do
  let (pipelineLayout, pipelineObj) = getPipeline @pipeline provider
  cmdBuf <- ask
  liftIO $ vkCmdBindPipeline cmdBuf VK_PIPELINE_BIND_POINT_GRAPHICS pipelineObj
  runReaderT plAction pipelineLayout