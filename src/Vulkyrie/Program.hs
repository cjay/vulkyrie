{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}

module Vulkyrie.Program
    ( Program (..)
    , runProgram
    , MonadIO (..)
      -- * Threading
    , forkProg
    , asyncProg
    , waitProg
      -- * Resource management
    , later
    , allocResource
    , locally
    , manually
    , cleanup
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
import           Data.Either                    (lefts)
import           Data.List.NonEmpty
import           Data.String                    (fromString)
import           Data.Typeable
import           GHC.Stack
import           Graphics.Vulkan.Core_1_0
import           System.Exit
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Vulkyrie.BuildFlags

data ProgramContext
  = ProgramContext
  { destructorsVar :: IORef [IO ()]
  }

newtype Program a = Program (ReaderT ProgramContext (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ProgramContext, MonadUnliftIO, MonadLogger)

makeProgramContext :: IO ProgramContext
makeProgramContext = do
  destructorsVar <- newIORef []
  return ProgramContext{..}

runProgram :: Program a -> IO a
runProgram prog = makeProgramContext >>= flip run prog

run :: ProgramContext -> Program a -> IO a
run ctx (Program p) = do
  let ProgramContext{ destructorsVar } = ctx
  mask $ \restore -> do
    a <- restore (runStdoutLoggingT $ runReaderT p ctx)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
    readIORef destructorsVar >>= cleanup Nothing
    return a

locally :: Program a -> Program a
locally prog = do
  ctx <- ask
  destructorsVar <- newIORef []
  liftIO $ run ctx{ destructorsVar } prog

-- | Has to be called within a masked scope. Pass the restore. Returns
-- destructor list along with result.
manually :: (IO a -> IO a) -> Program a -> Program ([IO ()], a)
manually restore (Program p) = do
  ctx <- ask
  destructorsVar <- newIORef []
  a <- liftIO $
    restore (runStdoutLoggingT $ runReaderT p ctx{ destructorsVar })
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
  destructors <- readIORef destructorsVar
  return (destructors, a)

data CleanupException = CleanupException
  { exceptionCausingCleanup :: Maybe SomeException
  , exceptionsDuringCleanup :: NonEmpty SomeException
  }
  deriving (Show, Typeable)
instance Exception CleanupException

cleanup :: Maybe SomeException -> [IO ()] -> IO ()
cleanup origException destructors =
  lefts <$> mapM try destructors >>= \case
    e : es -> throwIO (CleanupException origException (e :| es))
    [] -> return ()

-- TODO thread management
forkProg :: Program () -> Program ThreadId
forkProg = forkIO . locally

asyncProg :: Program a -> Program (Async a)
asyncProg = async . locally

waitProg :: Async a -> Program a
waitProg = wait

-- TODO restrict to IO. Probably no need for atomic.
later :: Program ()
      -> Program ()
later prog = do
  u <- askUnliftIO
  ProgramContext{ destructorsVar } <- ask
  atomicModifyIORef' destructorsVar $ \ds -> (unliftIO u prog : ds, ())
{-# INLINE later #-}

-- TODO maybe masking during allocation should be default. bracket does it etc
-- | Exception-safe. Allows async exceptions during allocation, so it's fine if
-- the allocation is a complex Program.
allocResource :: (a -> Program ()) -- ^ free resource
              -> Program a -- ^ allocate resource
              -> Program a
allocResource free alloc =
  mask $ \restore -> do
    a <- restore alloc
    later $ free a
    return a

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
      (locally $ do
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
