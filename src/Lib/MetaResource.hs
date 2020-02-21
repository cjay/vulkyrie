{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib.MetaResource
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
  ) where

import           Graphics.Vulkan.Core_1_0
import           Lib.Program

class GenericResource res a where
  -- | Makes use of the continuation based resource management built into Program
  auto :: res r a -> Program r a
  -- | Creates a destructor action along with the resource.
  manual :: res r a -> Program r (Program' (), a)

data Resource r a = Resource
  { auto_ :: Program r a
  , manual_ :: Program r (Program' (), a)
  }

instance GenericResource Resource a where
  auto = auto_
  {-# INLINE auto #-}
  manual = manual_
  {-# INLINE manual #-}

-- | A MetaResource is only meant to correctly destroy resources that it created itself.
--   It can create and destroy multiple values though.
--
--   It can retain contextual information from the creation, like the
--   appropriate VkDevice. Thereby it gives the freedom of not (necessarily)
--   carrying around all information needed for destruction along with the
--   resource value.
data MetaResource r a = MetaResource { destroy :: a -> Program' (), create :: Program r a }

-- | drop in replacement for Lib.Program.allocResource
metaResource :: (a -> Program' ()) -- ^ destroy resource
             -> Program r a        -- ^ allocate resource
             -> MetaResource r a
metaResource destroy create = MetaResource {..}
{-# INLINE metaResource #-}

instance GenericResource MetaResource a where
  -- auto :: MetaResource r a -> Program r a
  auto MetaResource{..} = allocResource destroy create
  {-# INLINE auto #-}

  -- manual :: MetaResource r a -> Program r (a, Program' ())
  manual MetaResource{..} = do
    x <- create
    return (destroy x, x)
  {-# INLINE manual #-}


-- | This is purely for the right wording
class AllocFree a where
  free :: MetaResource r a -> (a -> Program' ())
  free = destroy
  {-# INLINE free #-}

  alloc :: MetaResource r a -> Program r a
  alloc = create
  {-# INLINE alloc #-}

-- These are the only three things that have a vkAlloc.. and vkFree.. function
instance AllocFree VkCommandBuffer
instance AllocFree VkDescriptorSet
instance AllocFree VkDeviceMemory
instance (AllocFree a) => AllocFree [a]


-- TODO should change BasicResource to be able to access destroy directly
-- | Like MetaResource, but is meant to destroy any resources of the correct type
type BasicResource = MetaResource

-- | drop in replacement for Lib.Program.allocResource
basicResource :: (a -> Program' ()) -- ^ destroy resource
              -> Program r a        -- ^ allocate resource
              -> BasicResource r a
basicResource = metaResource


-- | Need this to use MetaResources in the Resource monad
resource :: MetaResource r a -> Resource r a
resource ma = Resource (auto ma) (manual ma)
{-# INLINE resource #-}

-- | A bit more polymorphic than bind of Resource
composeResource :: (GenericResource res1 a, GenericResource res2 b) => res1 r a -> (a -> res2 r b) -> Resource r b
composeResource ma fmb = Resource
  (auto ma >>= auto . fmb)
  (do
    (destroyA, a) <- manual $ ma
    (destroyB, b) <- manual $ fmb a
    return (destroyB >> destroyA, b)
  )
{-# INLINE composeResource #-}

instance Functor (Resource r) where
  fmap g fa = Resource (fmap g $ auto fa) (fmap g <$> manual fa)
  {-# INLINE fmap #-}

instance Applicative (Resource r) where
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

instance Monad (Resource r) where
  (>>=) = composeResource
  {-# INLINE (>>=) #-}
