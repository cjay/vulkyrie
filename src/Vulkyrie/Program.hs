{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}

module Vulkyrie.Program
    ( Program (..)
    , runProgram
    , MonadIO (..)
      -- * Threading
    , threadRes
      -- * Resource management
    , Resource
    , askRegion
    , runResource
    , onDestroy
    , liftProg
    , allocResource
    , locally
    , resourceMask
    , inverseDestruction
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
import           Control.Monad.Trans.Class      (lift)
import           Data.Either                    (lefts)
import           Data.List.NonEmpty (NonEmpty(..))
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




data ResourceContext
  = ResourceContext
  { destructorsVar :: IORef [Program ()]
  }

-- | Resources enable automatic deallocation when the scope ends. Scopes are
-- created with either locally or runResource.
--
-- Not exposing MonadUnliftIO to prevent forkIO and friends from messing with
-- the IORef (use threadRes instead). MonadUnliftIO can be accessed by unwrapping
-- the newtype.
newtype Resource a = Resource (ReaderT ResourceContext Program a)
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO)

makeResourceContext :: Program ResourceContext
makeResourceContext = do
  destructorsVar <- newIORef []
  return ResourceContext{..}

askResourceContext :: Resource ResourceContext
askResourceContext = Resource ask

inContext :: ResourceContext -> Resource a -> Resource a
inContext ctx (Resource res) = liftProg $ runReaderT res ctx

askRegion :: Resource (Resource a -> Resource a)
askRegion = inContext <$> askResourceContext


runResource :: Resource a -> Program a
runResource res = makeResourceContext >>= flip runRes res

-- | Consumes the context. Would need to write empty list to it for reuse.
runRes :: ResourceContext -> Resource a -> Program a
runRes ctx (Resource res) = do
  let ResourceContext{ destructorsVar } = ctx
  mask $ \restore -> do
    a <- restore (runReaderT res ctx)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
    readIORef destructorsVar >>= cleanup Nothing
    return a

locally :: Resource a -> Resource a
locally = liftProg . runResource

resourceMask :: ((forall a. Program a -> Program a) -> Resource b) -> Resource b
resourceMask act = Resource $ do
  ctx <- ask
  lift $ mask $ \restore -> do
    let Resource res = act restore
    runReaderT res ctx

-- TODO: this might behave unexpectedly with nested resources, as all destructor calls are reversed, even within nested ones
inverseDestruction :: Resource a -> Resource a
inverseDestruction (Resource res) = do
  ResourceContext outerDestrVar <- askResourceContext
  liftProg $ mask $ \restore -> do
    ctx <- makeResourceContext
    let ResourceContext{ destructorsVar } = ctx
    restore (runReaderT res ctx)
      `finally` do
        destructors <- readIORef destructorsVar
        modifyIORef' outerDestrVar (reverse destructors <>)

{-
asym :: Resource a -> (a -> Resource b) -> Resource b
asym ra rb =
  resourceMask $ \restore -> do
    ResourceContext{ destructorsVar } <- askResourceContext
    (destroyA, a) <- liftProg $ manually restore ra
    (destroyB, b) <- liftProg $ manually restore (rb a)
      `finally` modifyIORef' destructorsVar (destroyA <>)
    modifyIORef' destructorsVar ((destroyA <> destroyB) <>)
    return b
-}

-- | Has to be called within a masked scope. Pass the restore. Returns
-- destructor action along with result.
manually :: (forall a. Program a -> Program a) -> Resource b -> Program (Program (), b)
manually restore (Resource res) = do
  ctx <- makeResourceContext
  let ResourceContext{ destructorsVar } = ctx
  a <-
    restore (runReaderT res ctx)
      `catch` (\e -> readIORef destructorsVar >>= cleanup (Just e) >> throwIO e)
  return (readIORef destructorsVar >>= cleanup Nothing, a)

-- | Runs given program when destroying the resource. Destruction order is bottom to top.
--
--   Never use this to deallocate, that way you get no exception masking! Use allocResource!
onDestroy :: Program () -> Resource ()
onDestroy prog = do
  ResourceContext{ destructorsVar } <- askResourceContext
  modifyIORef' destructorsVar (prog :)
{-# INLINE onDestroy #-}

liftProg :: Program a -> Resource a
liftProg prog = Resource $ lift prog

-- TODO maybe masking during allocation should be default. bracket does it etc
-- | Exception-safe. Allows async exceptions during allocation, so it's fine if
-- the allocation is a complex Resource.
allocResource :: (a -> Program ()) -- ^ free resource
              -> Program a -- ^ allocate resource
              -> Resource a
allocResource free alloc =
  -- using unwrapped Resource to get MonadUnliftIO for mask
  Resource $ mask $ \restore -> do
    a <- restore $ lift alloc
    ResourceContext{ destructorsVar } <- ask
    modifyIORef' destructorsVar (free a :)
    return a

data CleanupException = CleanupException
  { exceptionCausingCleanup :: Maybe SomeException
  , exceptionsDuringCleanup :: NonEmpty SomeException
  }
  deriving (Show, Typeable)
instance Exception CleanupException

cleanup :: Maybe SomeException -> [Program ()] -> Program ()
cleanup origException destructors =
  lefts <$> mapM try destructors >>= \case
    e : es -> throwIO (CleanupException origException (e :| es))
    [] -> return ()



-- TODO letting owning thread know that child thread exited?
-- TODO this can't clean up threads recursively
threadRes :: Program () -> Resource ThreadId
threadRes prog = allocResource killThread (forkIO prog)



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
