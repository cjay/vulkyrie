{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
-- | Provide a `Program` monad to execute vulkan actions, carry state,
--   manage allocated resources, and process exceptions.
--
--   Note the strictness: we don't want to pile up unevaluated thunks in
--   the program state, but @Strict@ pragma would ruin continuation monad;
--   thus, we have to keep a careful balance between strict and lazy functions.
module Vulkyrie.Program
    ( Program (..)
    , runProgram
    , MonadIO (..)
      -- * Threading
    , forkProg
      -- * Resource management
    , later
    , allocResource
    , locally
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
import           Control.Monad.Trans.Reader
import           Control.Monad.Reader.Class
import           Data.String                    (fromString)
import           Data.Typeable
import           GHC.Stack
import           Graphics.Vulkan.Core_1_0
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Vulkyrie.BuildFlags

data ProgramContext = ProgramContext

newtype Program a = Program (ReaderT ProgramContext (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProgramContext, MonadUnliftIO, MonadLogger)

runProgram :: Program a -> IO a
runProgram (Program p) = runStdoutLoggingT $ runReaderT p ProgramContext

-- TODO thread management
forkProg :: Program () -> Program ThreadId
forkProg = forkIO

later :: Program ()
      -> Program ()
later prog = _
{-# INLINE later #-}

allocResource :: (a -> Program ()) -- ^ free resource
              -> Program a -- ^ allocate resource
              -> Program a
allocResource free alloc = _

locally :: Program a
        -> Program a
locally p = _

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

logDebug :: HasCallStack => String -> Program ()
logDebug =
  if isDEVELOPMENT
  then Logger.logDebug . fromString
  else const (pure ())
{-# INLINE logDebug #-}

logInfo :: HasCallStack => String -> Program ()
logInfo = Logger.logInfo . fromString
{-# INLINE logInfo #-}

logWarn :: HasCallStack => String -> Program ()
logWarn = Logger.logWarn . fromString
{-# INLINE logWarn #-}

logError :: HasCallStack => String -> Program ()
logError = Logger.logError . fromString
{-# INLINE logError #-}


data LoopControl a = ContinueLoop | AbortLoop a deriving Eq

loop :: Program (LoopControl a) -> Program a
loop action = go
  where go = action >>= \case
          ContinueLoop -> go
          AbortLoop a -> return a

-- | Enables deferred deallocation.
asyncRedo :: (Program () -> Program ()) -> Program ()
asyncRedo = _

-- | For C functions that have to run in the main thread as long as the program runs.
--
--   Caveat: The separate thread is not a bound thread, in contrast to the main thread.
--   Use `runInBoundThread` there if you need thread local state for C libs.
occupyThreadAndFork :: Program () -- ^ the program to run in the main thread
                    -> Program () -- ^ the program to run in a separate thread
                    -> Program ()
occupyThreadAndFork mainProg deputyProg = do
  mainThreadId <- myThreadId
  void $ forkProg deputyProg
  mainProg


-- | to make sure IORef writes arrive in other threads
-- TODO Investigate cache coherence + IORefs. I'm not 100% sure this does what I want.
touchIORef :: IORef a -> Program ()
touchIORef ref = atomicModifyIORef' ref (\x -> (x, ()))
