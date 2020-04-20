{-# LANGUAGE CPP                   #-}

-- | Collecting flags passed from cabal to restrict CPP to a single module.
module Vulkyrie.BuildFlags where

isDEVELOPMENT :: Bool
#ifdef DEVELOPMENT
isDEVELOPMENT = True
#else
isDEVELOPMENT = False
#endif
{-# INLINE isDEVELOPMENT #-}
