module Lib.MonadIO.IORef (
    -- MonadIO(..)

    R.IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , modifyIORef'
  , atomicModifyIORef
  , atomicModifyIORef'
  , atomicWriteIORef
  , mkWeakIORef

  )
  where

import Control.Monad.Trans(MonadIO(..))
import qualified Data.IORef as R
import System.Mem.Weak (Weak)


newIORef :: MonadIO io => a -> io (R.IORef a)
newIORef x = liftIO $ R.newIORef x

readIORef :: MonadIO io => R.IORef a -> io a
readIORef r = liftIO $ R.readIORef r

writeIORef :: MonadIO io => R.IORef a -> a -> io ()
writeIORef r x = liftIO $ R.writeIORef r x

modifyIORef :: MonadIO io => R.IORef a -> (a -> a) -> io ()
modifyIORef r f = liftIO $ R.modifyIORef r f

modifyIORef' :: MonadIO io => R.IORef a -> (a -> a) -> io ()
modifyIORef' r f = liftIO $ R.modifyIORef' r f

atomicModifyIORef :: MonadIO io => R.IORef a -> (a -> (a, b)) -> io b
atomicModifyIORef r f = liftIO $ R.atomicModifyIORef r f

atomicModifyIORef' :: MonadIO io => R.IORef a -> (a -> (a, b)) -> io b
atomicModifyIORef' r f = liftIO $ R.atomicModifyIORef' r f

atomicWriteIORef :: MonadIO io => R.IORef a -> a -> io ()
atomicWriteIORef r x = liftIO $ R.atomicWriteIORef r x

mkWeakIORef :: MonadIO io => R.IORef a -> IO () -> io (Weak (R.IORef a))
mkWeakIORef ref finalizer = liftIO $ R.mkWeakIORef ref finalizer