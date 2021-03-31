{-# LANGUAGE RankNTypes #-}
module Vulkyrie.Tests where

import Control.Monad
import UnliftIO.Concurrent
import UnliftIO.Exception
import Vulkyrie.Program
import Vulkyrie.Resource
import Vulkyrie.Concurrent

tests :: Prog r ()
tests = do
  resourceTest

resourceTest :: Prog r ()
resourceTest = do
  region $ do
    let ra = res "A" 1
    let rb = res "B" 2
    liftIO $ putStrLn "before"
    void $ auto ra
    liftIO $ putStrLn "between"
    void $ auto rb
    liftIO $ putStrLn "after"

  liftIO $ putStrLn "\n--\n"

  -- res1 with auto
  region $ do
    r1 <- auto res1
    liftIO $ print r1

  liftIO $ putStrLn "\n--\n"

  -- res1 with manual
  mask $ \restore -> do
    (destroyR1, r1) <- manual restore res1
    liftIO $ print r1
    destroyR1

  liftIO $ putStrLn "\n--\n"

  region $ do
    r2 <- auto res2
    liftIO $ print r2

  return ()

-- | simple resource
res :: String -> Int -> MetaResource Int
res name val = metaResource
  (const $ do
      liftIO $ putStrLn $ "destroying " ++ name
      return ()
  )
  (do
      liftIO $ putStrLn $ "creating " ++ name
      return val
  )

-- | composition
res1 :: Resource Int
res1 = Resource $ do
  liftIO $ putStrLn "onCreate before"
  onDestroy $ liftIO $ putStrLn "onDestroy before"
  a <- auto $ res "A1" 1
  liftIO $ putStrLn "onCreate between"
  onDestroy $ liftIO $ putStrLn "onDestroy between"
  b <- auto $ res "B1" (a+3)
  liftIO $ putStrLn "onCreate after"
  onDestroy $ liftIO $ putStrLn "onDestroy after"
  return b

-- | destruction inversion
res2 :: Resource Int
res2 = Resource $ do
  liftIO $ putStrLn "onCreate before"
  onDestroy $ liftIO $ putStrLn "onDestroy before"
  b <- mask $ \restore -> do
    (destroyA, a) <- manual restore $ res "A1" 1
    liftIO $ putStrLn "onCreate between"
    onDestroy $ liftIO $ putStrLn "onDestroy between creation"
    b <- auto $ res "B1" (a+3)
    onDestroy $ liftIO $ putStrLn "onDestroy between destruction"
    onDestroy destroyA
    return b
  liftIO $ putStrLn "onCreate after"
  onDestroy $ liftIO $ putStrLn "onDestroy after"
  return b

threadTest :: IO ()
threadTest = runProgram $ do
  region $ do
    _ <- auto $ threadRes $ do
      logInfo "in the thread"
      void $ throwString  "oh no"
      threadDelay 1000000
      logInfo "end of thread"
    threadDelay 100
    -- throwTo tid (ChildThreadException (toException $ IndexOutOfBounds "foo") "<src loc>")
    -- killThread tid
    logInfo "after throw"
    threadDelay 2000000
    logInfo "end"

threadOwnerTest :: IO ()
threadOwnerTest = runProgram $ do
  region $ do
    owner <- auto threadOwner
    _ <- ownedThread owner $ do
      logInfo "in the thread"
      -- throwString  "oh no"
      threadDelay 1000000
      logInfo "end of thread"
    threadDelay 100
    -- throwTo tid (ChildThreadException (toException $ IndexOutOfBounds "foo") "<src loc>")
    -- killThread tid
    -- logInfo "after throw"
    threadDelay 2000000
    logInfo "end"