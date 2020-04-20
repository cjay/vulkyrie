{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vulkyrie.Resource
  ( GenericResource
  , auto
  , manual
  , Resource

  , MetaResource
  , destroy
  , create
  , metaResource
  , alloc
  , free

  , BasicResource
  , basicResource

  , resource
  , composeResource
  , onCreate
  , onDestroy
  ) where

import           Graphics.Vulkan.Core_1_0
import           Vulkyrie.Program

class GenericResource res a where
  -- | Makes use of the continuation based resource management built into Program
  auto :: res a -> Program a
  -- | Creates a destructor action along with the resource.
  manual :: res a -> Program (Program (), a)

-- TODO not sure if carrying auto is worth it. Could replace it with manual and
-- Program.later. The fields and constructor of Resource should stay
-- implementation details for that reason.
data Resource a = Resource
  { auto_ :: Program a
  , manual_ :: Program (Program (), a)
  }

instance GenericResource Resource a where
  auto = auto_
  {-# INLINE auto #-}
  manual = manual_
  {-# INLINE manual #-}

-- | A MetaResource is only meant to correctly destroy resources that it created itself.
--   It can create and destroy multiple values though.
--
--   The closure used to create a MetaResource typically retains some of the
--   creation parameters, like the appropriate VkDevice. Thereby it gives the
--   freedom of not (necessarily) carrying around all information needed for
--   destruction along with the resource value.
data MetaResource a = MetaResource
  { destroy :: a -> Program ()
  , create :: Program a
  }

-- | Creates a MetaResource. Drop in replacement for Vulkyrie.Program.allocResource.
metaResource :: (a -> Program ()) -- ^ destroy resource
             -> Program a        -- ^ create resource
             -> MetaResource a
metaResource destroy create = MetaResource {..}
{-# INLINE metaResource #-}

instance GenericResource MetaResource a where
  -- auto :: MetaResource a -> Program a
  auto MetaResource{..} = Vulkyrie.Program.allocResource destroy create
  {-# INLINE auto #-}

  -- manual :: MetaResource a -> Program (a, Program ())
  manual MetaResource{..} = do
    x <- create
    return (destroy x, x)
  {-# INLINE manual #-}


-- | Things that allow alloc and free synonyms to create and destroy.
--
--   This is purely for the right wording. The only instances are
--   VkCommandBuffer, VkDescriptorSet and VkDeviceMemory. These are the only
--   original Vulkan types that have a vkAlloc.. and vkFree.. function.
class AllocFree a where
  -- | Synonym for destroy
  free :: MetaResource a -> (a -> Program ())
  free = destroy
  {-# INLINE free #-}

  -- | Synonym for create
  alloc :: MetaResource a -> Program a
  alloc = create
  {-# INLINE alloc #-}

instance AllocFree VkCommandBuffer
instance AllocFree VkDescriptorSet
instance AllocFree VkDeviceMemory
instance (AllocFree a) => AllocFree [a]


-- TODO should change BasicResource to be able to access destroy directly
-- | Like MetaResource, but is meant to destroy any resources of the correct type.
--
--   Take care not to capture creation parameters in the destruction action.
type BasicResource = MetaResource

-- | Creates a BasicResource. Drop in replacement for Vulkyrie.Program.allocResource.
basicResource :: (a -> Program ()) -- ^ destroy resource
              -> Program a        -- ^ create resource
              -> BasicResource a
basicResource = metaResource


-- | Need this to use MetaResources in the Resource monad
resource :: MetaResource a -> Resource a
resource ma = Resource (auto ma) (manual ma)
{-# INLINE resource #-}

-- | A bit more polymorphic than (>>=) of Resource
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 a -> (a -> res2 b) -> Resource b
composeResource ma fmb = Resource
  (auto ma >>= auto . fmb)
  (do
    (destroyA, a) <- manual $ ma
    (destroyB, b) <- manual $ fmb a
    return (destroyB >> destroyA, b)
  )
{-# INLINE composeResource #-}

instance Functor Resource where
  fmap g fa = Resource (fmap g $ auto fa) (fmap g <$> manual fa)
  {-# INLINE fmap #-}

instance Applicative Resource where
  pure a = Resource (return a) (return (return (), a))
  {-# INLINE pure #-}
  fab <*> fa = Resource
    (auto fab <*> auto fa)
    (do
        (destroyAB, ab) <- manual fab
        (destroyA, a) <- manual fa
        return (destroyA >> destroyAB, ab a)
    )
  {-# INLINE (<*>) #-}

instance Monad Resource where
  (>>=) = composeResource
  {-# INLINE (>>=) #-}

-- | Runs given program when creating the resource. Creation order is top to bottom.
onCreate :: Program a -> Resource a
onCreate prog = Resource prog (prog >>= \a -> return (return (), a))
{-# INLINE onCreate #-}

-- | Runs given program when destroying the resource. Destruction order is bottom to top.
onDestroy :: Program () -> Resource ()
onDestroy prog = Resource (later prog) (return (prog, ()))
{-# INLINE onDestroy #-}

{- probably too rare, not really needed now
asymmetricResource :: GenericResource res a => res r a -> Resource (Resource (), a)
asymmetricResource res = do
  (destr, a) <- onCreate $ manual res
  return (onDestroy destr, a)
{-# INLINE asymmetricResource #-}
-}
