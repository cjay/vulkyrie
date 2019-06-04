module Lib.MonadIO.MVar (
    -- MonadIO(..)

    C.MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  )
  where

import Control.Monad.Trans(MonadIO(..))
import qualified Control.Concurrent as C



newEmptyMVar  :: MonadIO io => io (C.MVar a)
newEmptyMVar   = liftIO $ C.newEmptyMVar

newMVar       :: MonadIO io => a -> io (C.MVar a)
newMVar x      = liftIO $ C.newMVar x

takeMVar      :: MonadIO io => C.MVar a -> io a
takeMVar m     = liftIO $ C.takeMVar m

putMVar       :: MonadIO io => C.MVar a -> a -> io ()
putMVar m x    = liftIO $ C.putMVar m x

readMVar      :: MonadIO io => C.MVar a -> io a
readMVar m     = liftIO $ C.readMVar m

swapMVar      :: MonadIO io => C.MVar a -> a -> io a
swapMVar m x   = liftIO $ C.swapMVar m x

tryTakeMVar   :: MonadIO io => C.MVar a -> io (Maybe a)
tryTakeMVar m  = liftIO $ C.tryTakeMVar m

tryPutMVar    :: MonadIO io => C.MVar a -> a -> io Bool
tryPutMVar m x = liftIO $ C.tryPutMVar m x

isEmptyMVar   :: MonadIO io => C.MVar a -> io Bool
isEmptyMVar m  = liftIO $ C.isEmptyMVar m

