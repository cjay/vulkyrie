module Lib.MonadIO.Chan (
    -- MonadIO(..)
    C.Chan
  , newChan
  , writeChan
  , readChan
  , dupChan
  , unGetChan
  , getChanContents
  , writeList2Chan
  )
  where

import Control.Monad.Trans(MonadIO(..))
import qualified Control.Concurrent as C


newChan            :: MonadIO io => io (C.Chan a)
newChan             = liftIO $ C.newChan

writeChan          :: MonadIO io => C.Chan a -> a -> io ()
writeChan c x       = liftIO $ C.writeChan c x

readChan           :: MonadIO io => C.Chan a -> io a
readChan c          = liftIO $ C.readChan c

dupChan            :: MonadIO io => C.Chan a -> io (C.Chan a)
dupChan c           = liftIO $ C.dupChan c

unGetChan          :: MonadIO io => C.Chan a -> a -> io ()
unGetChan c x       = liftIO $ unGetChan c x

getChanContents    :: MonadIO io => C.Chan a -> io [a]
getChanContents c   = liftIO $ C.getChanContents c

writeList2Chan     :: MonadIO io => C.Chan a -> [a] -> io ()
writeList2Chan c xs = liftIO $ C.writeList2Chan c xs
