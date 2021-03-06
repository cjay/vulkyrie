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
-- NamedWildCards together with PartialTypeSignatures effectively allows type
-- variables _foo that don't need to be listed in the forall section.
-- Also, annoyingly, _ isn't allowed as a constraint parameter, but _foo is.
{-# LANGUAGE NamedWildCards #-}
-- PartialTypeSignatures among other things also allows _ in the constraint list
-- to infer missing constraints.
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Vulkyrie.Vulkan.PushConstant where

import Data.Int
import Data.Kind (Type, Constraint)
import Data.Type.Lits (Symbol, type (*), type (+), type (-), natVal, KnownNat)
import Data.Proxy ( Proxy(Proxy) )
import GHC.TypeLits (Nat, Mod, ErrorMessage (ShowType))
import GHC.Word ( Word8, Word16, Word32, Word64 )
import Numeric.DataFrame ( DataFrame, PrimArray, Scalar, Vector, Matrix )
import Type.Errors ( TypeError, ErrorMessage(Text, (:<>:)) )
import Vulkyrie.Program
import Graphics.Vulkan.Core_1_0 (VkShaderStageFlags, vkCmdPushConstants)
import Graphics.Vulkan (VkCommandBuffer)
import Graphics.Vulkan (VkPipelineLayout)
import Numeric.Dimensions (Dimensions, Head, Last)
import Numeric.DataFrame.IO (withDataFramePtr, thawPinDataFrame)
import Foreign.Ptr (castPtr)
import Vulkyrie.Vulkan.Engine

-- wrapper to differentiate vectors from arrays, for different base alignment
newtype GpuArray t n = GpuArray (ArrayType t n)

type family ArrayType t n where
  ArrayType (DataFrame t' dims) n = DataFrame t' (n ': dims)
  ArrayType t n = DataFrame t '[n]

-- rows on GPU become columns in DataFrames and vice versa
-- glsl reads column-major by default and is column-major internally
type GlslMatrix t cols rows = Matrix t rows cols

type family Product (a :: [Nat]) :: Nat where
  Product (a:as) = a * Product as
  Product '[] = 1

type family SizeOf a :: Nat where
  SizeOf Word8 = 1
  SizeOf Word16 = 2
  SizeOf Word32 = 4
  SizeOf Word64 = 8
  SizeOf Int8 = 1
  SizeOf Int16 = 2
  SizeOf Int32 = 4
  SizeOf Int64 = 8
  SizeOf Float = 4
  SizeOf Double = 8
  SizeOf (DataFrame t dims) = Product dims * SizeOf t
  SizeOf (GpuArray t n) = SizeOf (ArrayType t n)

-- size of matrix rows (= columns in glsl) or array elements
type ElemSize t = ElemSize_ (SizeOf t) (AlignmentOf t) (Mod (SizeOf t) (AlignmentOf t))
type family ElemSize_ size alignment rem where
  ElemSize_ size _ 0 = size
  ElemSize_ size alignment rem = size + alignment - rem

type family GpuSizeOf a :: Nat where
  GpuSizeOf (Matrix t rows cols) = rows * ElemSize (Vector t cols)
  GpuSizeOf (GpuArray t n) = n * ElemSize t
  GpuSizeOf x = SizeOf x

-- GPU alignment for std430 layout (note: not allowed for uniform buffers)
type family AlignmentOf a :: Nat where
  AlignmentOf (Scalar a) = SizeOf a
  AlignmentOf (Vector t 2) = SizeOf t * 2
  AlignmentOf (Vector t 3) = SizeOf t * 4
  AlignmentOf (Vector t 4) = SizeOf t * 4
  AlignmentOf (Matrix t rows cols) = AlignmentOf (Vector t cols)
  AlignmentOf (GpuArray t n) = AlignmentOf t

type VerifyOffset (field :: Symbol) (t :: Type) (offset :: Nat) =
  VerifyOffset_ (Mod offset (AlignmentOf t)) (AlignmentOf t) field offset

type family VerifyOffset_ rem alignment field offset where
  VerifyOffset_ 0 _ _ offset = offset
  VerifyOffset_ rem alignment field _ = TypeError
    ('Text "Field " ':<>: 'Text field ':<>: 'Text " has wrong alignment. Reorder fields or insert padding of "
      ':<>: 'ShowType (alignment - rem) ':<>: 'Text " bytes before the field.")

type VerifyType t = VerifyType_ t (SizeOf t) (GpuSizeOf t)

type family VerifyType_ t size gpuSize where
  VerifyType_ t s s = t
  VerifyType_ t s gs = TypeError
    ('Text "The type " ':<>: 'ShowType t
      ':<>: 'Text " can't be used in a field because the GPU equivalent needs internal padding.")

-- TODO could encode the shader stage flags in the field list, like:
-- data Field = Field Symbol Type | StageRangeBegin ShaderStage | StageRangeEnd ShaderStage
-- The ranges can overlap. The bitmask in vkCmdPushConstants could be completely
-- derived from this, because the spec demands it to exactly match the
-- definition of the ranges in the pipeline layout.
data Field = Field Symbol Type
type (:::) (s :: Symbol) (t :: Type) = 'Field s t

type family FieldName (field :: Field) :: Symbol where
  FieldName (sym ::: _) = sym

type family FieldType (fields :: [Field]) (field :: Symbol) :: Type where
  FieldType '[] field = TypeError ('Text "Field not found: " ':<>: 'Text field)
  FieldType (field ::: t : _) field = t
  FieldType (_ : xs) field = FieldType xs field

-- | Returns offset and length of the field in bytes
type Place fields f = Place_ fields f 0
type family Place_ (fields :: [Field]) (f :: Symbol) (current :: Nat) :: (Nat, Nat) where
  Place_ '[] f cur = TypeError ('Text "Field not found: " ':<>: 'Text f)
  Place_ (f ::: t : _) f cur = '(VerifyOffset f t cur, SizeOf (VerifyType t))
  Place_ (_ ::: t : xs) f cur = Place_ xs f (cur + SizeOf t)

type HasPlacement (fields :: [Field]) (f :: Symbol) (offset :: Nat) (len :: Nat) =
  ( KnownNat offset,
    KnownNat len,
    '(offset, len) ~ Place fields f
  )

{- | Returns offset and length of the field in bytes

You have to take care of alignment requirements yourself by ordering the fields
appropriately and maybe even inserting dummy fields, otherwise
shaders can read things from the wrong offsets.
See https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/chap15.html#interfaces-resources-layout

@
type Fields =
  [ "transform" ::: Mat44f,
    "color" ::: Vec4f,
    "pos" ::: Vec2f
  ]
@

>>> place @Fields @"pos"
(80,8)
-}
place ::
  forall (fields :: [Field]) (f :: Symbol).
  HasPlacement fields f _offset _len =>
  (Word32, Word32)
place = (fromIntegral $ natVal (Proxy @_offset), fromIntegral $ natVal (Proxy @_len))

-- | Proxy version of place. Not sure if needed.
place2 ::
  HasPlacement fields f offset len =>
  proxy (fields :: [Field]) ->
  proxy2 (f :: Symbol) ->
  (Word32, Word32)
place2 (_ :: proxy fs) (_ :: proxy2 f) = place @fs @f

-- | Returns offset and length of a range of fields
range ::
  forall (fields :: [Field]) (start :: Symbol) (end :: Symbol).
  ( HasPlacement fields start _offset1 _len1,
    HasPlacement fields end _offset2 _len2,
    KnownNat _len,
    _len ~ (_offset2 - _offset1 + _len2)
  ) =>
  (Word32, Word32)
range = (fromIntegral $ natVal (Proxy @_offset1), fromIntegral $ natVal (Proxy @_len))

type Mention f = () :: Constraint

-- | Returns offset and length of all fields.
wholeRange ::
  forall (fields :: [Field]).
  (
    -- TODO try different compiler versions for this
    Mention fields, -- absolutely no idea why this is needed, found out by accident
    _
  ) =>
  (Word32, Word32)
wholeRange = range @fields @(FieldName (Head fields)) @(FieldName (Last fields))

pushDF :: (Dimensions dims, PrimArray t (DataFrame t dims))
       => Word32
       -> Word32
       -> VkShaderStageFlags
       -> DataFrame t dims
       -> VkPipelineLayout
       -> VkCommandBuffer
       -> Prog r ()
pushDF offset len shaderStages df pipelineLayout cmdBuf =
  liftIO $ thawPinDataFrame df >>= flip withDataFramePtr
    (vkCmdPushConstants cmdBuf pipelineLayout shaderStages offset len . castPtr)
{-# INLINE pushDF #-}

-- | Push a DataFrame with offset and length derived from the field.
--
-- Pass the first two type parameters via type application.
pushField ::
  forall (fields :: [Field]) (f :: Symbol).
  ( FieldType fields f ~ DataFrame _t _dims,
    _
  ) =>
       VkShaderStageFlags
    -> DataFrame _t _dims
    -> PlCmd _p _r ()
pushField shaderStages df =
  let (offset, len) = place @fields @f
   in plCmd $ pushDF offset len shaderStages df
{-# INLINE pushField #-}