{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE Strict         #-}
{-# LANGUAGE UnboxedTuples  #-}
-- | Collection of functions adapted from @Foreign@ module hierarchy
module Vulkyrie.Program.Foreign
    ( Ptr, plusPtr, Storable.sizeOf
    , withVkPtr
    , withArrayLen
    , withVkArrayLen
    , alloca, allocaArray
    , peek, peekArray, poke
    , ptrAtIndex
    , asListVk
    , allocaPeek, allocaPeekVk, allocaPeekDF
    , mallocRes, mallocArrayRes, newArrayRes
    , withUnsafeField
    , getListCountAndRef
    ) where

import qualified GHC.Base
import GHC.TypeLits (Symbol)
import Data.Kind (Type)

import           Control.Monad.IO.Class
import qualified Foreign.Marshal.Alloc   as Foreign
import qualified Foreign.Marshal.Array   as Foreign
import           Foreign.Ptr
import           Foreign.Storable        (Storable)
import qualified Foreign.Storable        as Storable
import           Graphics.Vulkan.Marshal
import           Numeric.DataFrame
import           Numeric.DataFrame.IO
import           Numeric.Dimensions

import           Vulkyrie.MonadIO.MVar
import           Vulkyrie.Program


withVkPtr :: VulkanMarshal a
          => a
          -> (Ptr a -> Program' b)
          -> Program r b
withVkPtr x = liftIOWith (withPtr x)
{-# INLINE withVkPtr #-}

-- | This should probably be in Graphics.Vulkan.Marshal
withArrayLen :: (Storable a, VulkanMarshal a) => [a] -> (Word32 -> Ptr a -> IO b) -> IO b
withArrayLen xs pf = do
  ret <- Foreign.withArrayLen xs (pf . fromIntegral)
  touch xs
  return ret
{-# INLINE withArrayLen #-}

withVkArrayLen :: (Storable a, VulkanMarshal a) => [a] -> (Word32 -> Ptr a -> Program' b) -> Program r b
withVkArrayLen xs pf = liftIOWith (withArrayLen xs . curry) (uncurry pf)
{-# INLINE withVkArrayLen #-}

-- | Uses `newVkData`, deallocation happens via GC.
allocaPeekVk :: VulkanMarshal a
             => (Ptr a -> Program () ())
             -> Program r a
allocaPeekVk pf = Program $ \ref c -> do
  locVar <- newEmptyMVar
  a <- newVkData (\ptr -> unProgram (pf ptr) ref (putMVar locVar))
  takeMVar locVar >>= c . (a <$)
{-# INLINE allocaPeekVk #-}




-- | Prevent earlier GC of given value
touch :: a -> IO ()
touch x = GHC.Base.IO $ \s -> case GHC.Base.touch# x s of s' -> (# s', () #)
{-# INLINE touch #-}

alloca :: Storable a
       => (Ptr a -> Program' b)
       -> Program r b
alloca = liftIOWith Foreign.alloca
{-# INLINE alloca #-}

allocaPeekDF :: forall a (ns :: [Nat]) r
              . (PrimBytes a, Dimensions ns)
             => (Ptr a -> Program () ())
             -> Program r (DataFrame a ns)
allocaPeekDF pf
  | Dict <- inferKnownBackend @a @ns
  = Program $ \ref c -> do
    mdf <- newPinnedDataFrame
    locVar <- newEmptyMVar
    withDataFramePtr mdf $ \ptr -> unProgram (pf ptr) ref (putMVar locVar)
    df <- unsafeFreezeDataFrame mdf
    takeMVar locVar >>= c . (df <$)
{-# INLINE allocaPeekDF #-}

allocaArray :: Storable a
            => Int
            -> (Ptr a -> Program' b)
            -> Program r b
allocaArray = liftIOWith . Foreign.allocaArray
{-# INLINE allocaArray #-}


allocaPeek :: Storable a
           => (Ptr a -> Program (Either VulkanException a) ())
           -> Program r a
allocaPeek f = alloca $ \ptr -> f ptr >> liftIO (Storable.peek ptr)
{-# INLINE allocaPeek #-}


peekArray :: Storable a => Int -> Ptr a -> Program r [a]
peekArray n = liftIO . Foreign.peekArray n
{-# INLINE peekArray #-}

peek :: Storable a => Ptr a -> Program r a
peek = liftIO . Storable.peek
{-# INLINE peek #-}

poke :: Storable a => Ptr a -> a -> Program r ()
poke p v = liftIO $ Storable.poke p v
{-# INLINE poke #-}

ptrAtIndex :: forall a. Storable a => Ptr a -> Int -> Ptr a
ptrAtIndex ptr i = ptr `plusPtr` (i * Storable.sizeOf @a undefined)
{-# INLINE ptrAtIndex #-}


-- | Get size of action output and then get the result,
--   performing data copy.
asListVk :: Storable x
         => (Ptr Word32 -> Ptr x -> Program (Either VulkanException [x]) ())
         -> Program r [x]
asListVk action = alloca $ \counterPtr -> do
  action counterPtr VK_NULL_HANDLE
  counter <- liftIO $ fromIntegral <$> Storable.peek counterPtr
  if counter <= 0
  then pure []
  else allocaArray counter $ \valPtr -> do
    action counterPtr valPtr
    liftIO $ Foreign.peekArray counter valPtr

-- | Allocate an array and release it after continuation finishes.
--   Uses @allocaArray@ from @Foreign@ inside.
--
--   Use `locally` to bound the scope of resource allocation.
mallocArrayRes :: Storable a => Int -> Program r (Ptr a)
mallocArrayRes n = Program $ \_ c -> Foreign.allocaArray n (c . Right)
{-# INLINE mallocArrayRes #-}

-- | Allocate some memory for Storable and release it after continuation finishes.
--   Uses @alloca@ from @Foreign@ inside.
--
--   Use `locally` to bound the scope of resource allocation.
mallocRes :: Storable a => Program r (Ptr a)
mallocRes = Program $ \_ c -> Foreign.alloca (c . Right)
{-# INLINE mallocRes #-}

-- | Temporarily store a list of storable values in memory
--   and release it after continuation finishes.
--   Uses @withArray@ from @Foreign@ inside.
--
--   Use `locally` to bound the scope of resource allocation.
newArrayRes :: Storable a => [a] -> Program r (Ptr a)
newArrayRes xs = Program $ \_ c -> Foreign.withArray xs (c . Right)
{-# INLINE newArrayRes #-}

-- | Keeps the vk struct alive while doing something with a unsafe field (like Ptr).
--
--   If the field has pointers to memory that has its lifetime coupled to the
--   struct, this function can be used to ensure the memory is kept alive.
withUnsafeField :: forall (fname :: Symbol) (struct :: Type) (a :: Type)
                . (CanReadField fname struct)
                => struct -> (FieldType fname struct -> IO a) -> IO a
withUnsafeField vkStruct fun = do
  let ptr = getField @fname vkStruct
  ret <- fun ptr
  touch vkStruct
  return ret

getListCountAndRef :: forall (countFname :: Symbol) (arrayFname :: Symbol) (struct :: Type) (a :: Type)
                   . (CanReadField countFname struct, CanReadField arrayFname struct, Storable a,
                      FieldType arrayFname struct ~ Ptr a,
                      FieldType countFname struct ~ Word32)
                   => struct -> IO [a]
getListCountAndRef vkStruct =
  withUnsafeField @arrayFname vkStruct (Foreign.peekArray (fromIntegral (getField @countFname vkStruct)))