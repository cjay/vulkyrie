{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}

module Vulkyrie.Program
    ( Program (..)
    , runProgram
    , MonadIO (..)
      -- * Exception handling
    , VulkanException (..)
    , runVk
    , runVkResult
    , runAndCatchVk
      -- * Logging
    , logDebug
    , logInfo
    , logWarn
    , logError
      -- * Other
    , LoopControl (..)
    , loop
    , asyncRedo
    , occupyThreadAndFork
    , touchIORef
    ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack hiding (logDebug, logInfo, logWarn, logError)
import qualified Control.Monad.Logger.CallStack as Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.String
import           Data.Typeable
import           GHC.Stack
import           Graphics.Vulkan.Core_1_0
import           System.Exit
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Vulkyrie.BuildFlags



data ProgramContext = ProgramContext {}

newtype Program a = Program (ReaderT ProgramContext (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO, MonadUnliftIO)

makeProgramContext :: IO ProgramContext
makeProgramContext = return ProgramContext {}

askProgramContext :: Program ProgramContext
askProgramContext = Program ask

runProgram :: Program a -> IO a
runProgram prog = makeProgramContext >>= flip run prog

run :: ProgramContext -> Program a -> IO a
run ctx (Program prog) = runStdoutLoggingT $ runReaderT prog ctx




newtype VulkanException = VulkanException VkResult
  deriving (Show, Typeable)

instance Exception VulkanException

runVk :: MonadIO m => IO VkResult -> m ()
runVk = void . runVkResult

runVkResult :: MonadIO m => IO VkResult -> m VkResult
runVkResult action = do
  r <- liftIO action
  when (r < VK_SUCCESS) (throwIO $ VulkanException r)
  return r

-- | Calls handler directly for error VkResult codes.
-- TODO rewrite rule?
runAndCatchVk :: MonadIO m => IO VkResult -> (VulkanException -> m ()) -> m ()
runAndCatchVk action handler = do
  r <- liftIO action
  when (r < VK_SUCCESS) (handler $ VulkanException r)



logDebug :: (HasCallStack, MonadLogger m) => String -> m ()
logDebug =
  if isDEVELOPMENT
  then Logger.logDebug . fromString
  else const (pure ())
{-# INLINE logDebug #-}

logInfo :: (HasCallStack, MonadLogger m) => String -> m ()
logInfo = Logger.logInfo . fromString
{-# INLINE logInfo #-}

logWarn :: (HasCallStack, MonadLogger m) => String -> m ()
logWarn = Logger.logWarn . fromString
{-# INLINE logWarn #-}

logError :: (HasCallStack, MonadLogger m) => String -> m ()
logError = Logger.logError . fromString
{-# INLINE logError #-}




data LoopControl a = ContinueLoop | AbortLoop a deriving Eq

loop :: Program (LoopControl a) -> Program a
loop action = go
  where go = action >>= \case
          ContinueLoop -> go
          AbortLoop a -> return a

data RedoSignal = SigRedo | SigExit deriving Eq

-- | Enables deferred deallocation.
asyncRedo :: (Program () -> Program ()) -> Program ()
asyncRedo prog = myThreadId >>= go where
  go parentThreadId = do
    control <- newEmptyMVar
    let trigger = do
          success <- tryPutMVar control SigRedo
          unless success $ throwString "asyncRedo action tried to signal more than once"
          liftIO yield
    -- TODO use forkOS when using unsafe ffi calls?
    -- don't need the threadId
    void $ forkFinally
      (do
        prog trigger
        -- When the redo-thread exits, we only need to signal exit to the parent
        -- if nothing else has been signalled yet.
        void $ tryPutMVar control SigExit
      )
      (\case
        -- TODO proper thread management. at least wrap in some AsyncException.
        Left exception -> throwTo parentThreadId exception
        Right ()       -> return ()
      )
    sig <- takeMVar control
    when (sig == SigRedo) (go parentThreadId)


-- | For C functions that have to run in the main thread as long as the program runs.
--
--   Caveat: The separate thread is not a bound thread, in contrast to the main thread.
--   Use `runInBoundThread` there if you need thread local state for C libs.
occupyThreadAndFork :: Program () -- ^ the program to run in the main thread
                    -> Program () -- ^ the program to run in a separate thread
                    -> Program ()
occupyThreadAndFork mainProg deputyProg = do
  mainThreadId <- myThreadId
  -- TODO proper thread management. at least wrap in some AsyncException.
  void $ forkFinally deputyProg $ \case
    Left exception -> throwTo mainThreadId exception
    Right ()       -> throwTo mainThreadId ExitSuccess
  mainProg


-- | to make sure IORef writes arrive in other threads
-- TODO Investigate cache coherence + IORefs. I'm not 100% sure this does what I want.
touchIORef :: IORef a -> Program ()
touchIORef ref = atomicModifyIORef' ref (\x -> (x, ()))
