{-# LANGUAGE RankNTypes #-}
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
  -- | Creates a Program.Resource that handles automatic deallocation.
  auto :: res a -> Resource a
  -- | Creates a destructor action along with allocating the resource.
  --
  --   Has to be called within a masked scope. Pass the restore. See Program.resourceMask.
  manual :: (forall b. Program b -> Program b) -> res a -> Program (Program (), a)

instance GenericResource Resource a where
  auto = id
  {-# INLINE auto #-}
  manual = manually
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
  auto MetaResource{..} = allocResource destroy create
  {-# INLINE auto #-}

  manual restore MetaResource{..} = manually restore (allocResource destroy create)
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
resource ma = allocResource (destroy ma) (create ma)
{-# INLINE resource #-}

-- | A bit more polymorphic than (>>=) of Resource.
--
--   Note that the resulting Resource is a GenericResource as well and be further chained using composeResource.
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 a -> (a -> res2 b) -> Resource b
composeResource ma fmb = auto ma >>= auto . fmb
{-# INLINE composeResource #-}

-- | Runs given program when creating the resource. Creation order is top to bottom.
onCreate :: Program a -> Resource a
onCreate = liftProg
{-# INLINE onCreate #-}

-- TODO onDestroy users need to mask, or need better solution
-- | Runs given program when destroying the resource. Destruction order is bottom to top.
onDestroy :: Program () -> Resource ()
onDestroy = later
{-# INLINE onDestroy #-}

{- probably too rare, not really needed now
asymmetricResource :: GenericResource res a => res r a -> Resource (Resource (), a)
asymmetricResource res = do
  (destr, a) <- onCreate $ manual res
  return (onDestroy destr, a)
{-# INLINE asymmetricResource #-}
-}
