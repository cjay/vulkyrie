module Lib.MonadIO.Thread (
    -- MonadIO(..)
    MonadIO(..)
  , C.ThreadId
  , forkIO
  , forkFinally
  , myThreadId
  , killThread
  , throwTo
  , yield
  , threadDelay

  )
  where

import Control.Monad.Trans(MonadIO(..))
import qualified Control.Concurrent as C
import qualified Control.Exception as E

forkIO :: MonadIO io => IO () -> io C.ThreadId
forkIO a = liftIO $ C.forkIO a

forkFinally :: MonadIO io => IO a -> (Either E.SomeException a -> IO ()) -> io C.ThreadId
forkFinally action and_then = liftIO $ C.forkFinally action and_then

myThreadId   :: MonadIO io => io C.ThreadId
myThreadId    = liftIO $ C.myThreadId

killThread   :: MonadIO io => C.ThreadId -> io ()
killThread i  = liftIO $ C.killThread i

throwTo      :: (E.Exception e, MonadIO io) => C.ThreadId -> e -> io ()
throwTo i e   = liftIO $ C.throwTo i e

yield        :: MonadIO io => io ()
yield         = liftIO $ C.yield

threadDelay  :: MonadIO io => Int -> io ()
threadDelay n = liftIO $ C.threadDelay n


