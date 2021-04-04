{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}

module Vulkyrie.Program
    ( Prog
    , OpenResourceContext
    , ClosedResourceContext
    , ResourceContext (..)
    , runProgram
    , closedProgram
    , askResourceContext
    , withResourceContext
    , askRegion
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
    , showt
      -- * Other
    , LoopControl (..)
    , loop
    , touchIORef
    ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack hiding (logDebug)
import qualified Control.Monad.Logger as Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Text
import           GHC.Stack ( HasCallStack, callStack )
import           Graphics.Vulkan.Core_1_0 ( VkResult(VK_SUCCESS) )
import           UnliftIO.Exception
import           UnliftIO.IORef

import           Vulkyrie.BuildFlags
import Data.Coerce (coerce)

-- | Marks a Prog to be using resources.
--
-- This should NEVER occur in the type of a top-level binding outside of this
-- module and the Resource module. Instead, wrap your `Prog OpenResourceContext
-- a` in a Resource. That way, all resource usages are explicit via `auto`.
data OpenResourceContext

-- | Marks a Prog to not be using resources.
--
-- Usually keep the type parameter to Prog open instead, to allow using it
-- within a resource using action. See `closedProgram` for when you need this.
data ClosedResourceContext

data ProgContext = ProgContext

newtype ResourceContext =
  ResourceContext
  { destructorsVar :: IORef [Prog ClosedResourceContext ()]
  }

data Context =
  Context
  {
    resourceContext :: ResourceContext
  , progContext :: ProgContext
  }

newtype Prog r a = Prog (ReaderT Context (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO, MonadUnliftIO)


askResourceContext :: Prog OpenResourceContext ResourceContext
askResourceContext = resourceContext <$> Prog ask

runProgram :: HasCallStack => Prog ClosedResourceContext a -> IO a
runProgram prog = do
  dummy <- newIORef $ impureThrow $ StringException "unexpected use of dummy resource context" callStack
  run (Context (ResourceContext dummy) ProgContext) prog

withResourceContext :: ResourceContext -> Prog OpenResourceContext a -> Prog r a
withResourceContext rctx (Prog prog) = Prog $ do
  ctx <- ask
  lift $ runReaderT prog ctx{ resourceContext = rctx }

-- | For registering a resource with a different (usually enclosing) region scope.
askRegion :: Prog OpenResourceContext (Prog OpenResourceContext a -> Prog r a)
askRegion = withResourceContext <$> askResourceContext

run :: Context -> Prog r a -> IO a
run ctx (Prog prog) = runStdoutLoggingT $ runReaderT prog ctx

-- | Embeds a Prog with closed resource context.
--
-- This allows putting Progs in data structures without having to resort to
-- existential types. Both `Prog r a` and `Prog ClosedResourceContext a`
-- guarantee not to use the ResourceContext, only the polymorphic one can be
-- embedded in resource using actions.
closedProgram :: Prog ClosedResourceContext a -> Prog r a
closedProgram = coerce



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
runAndCatchVk :: MonadIO m => IO VkResult -> (VulkanException -> m ()) -> m VkResult
runAndCatchVk action handler = do
  r <- liftIO action
  when (r < VK_SUCCESS) (handler $ VulkanException r)
  pure r



logDebug :: (HasCallStack, MonadLogger m) => Text -> m ()
logDebug =
  if isDEVELOPMENT
  -- Can't use Control.Monad.Logger.CallStack.logDebug here because then the
  -- location info in the log message refers to this wrapper function.
  then Logger.logDebugCS callStack
  else const (pure ())
{-# INLINE logDebug #-}

-- | Relevant for logging now. Like showt from TextShow, but doesn't require a new typeclass.
-- TODO vulkan-api should generate TextShow instances..
showt :: Show a => a -> Text
showt = pack . show


data LoopControl a = ContinueLoop | AbortLoop a deriving Eq

loop :: Prog r (LoopControl a) -> Prog r a
loop action = go
  where go = action >>= \case
          ContinueLoop -> go
          AbortLoop a -> return a


-- | to make sure IORef writes arrive in other threads
-- TODO Investigate cache coherence + IORefs. I'm not 100% sure this does what I want.
touchIORef :: IORef a -> Prog r ()
touchIORef ref = atomicModifyIORef' ref (\x -> (x, ()))
