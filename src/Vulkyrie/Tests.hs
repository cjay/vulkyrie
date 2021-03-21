module Vulkyrie.Tests where

import Control.Monad
import UnliftIO.Exception
import Vulkyrie.Program
import Vulkyrie.Resource

tests :: Program ()
tests = do
  resourceTest

resourceTest :: Program ()
resourceTest = do
  runResource $ do
    let ra = res "A" 1
    let rb = res "B" 2
    liftIO $ putStrLn "before"
    void $ auto ra
    liftIO $ putStrLn "between"
    void $ auto rb
    liftIO $ putStrLn "after"

  liftIO $ putStrLn "\n--\n"

  -- res1 with auto
  runResource $ do
    r1 <- auto res1
    liftIO $ print r1

  liftIO $ putStrLn "\n--\n"

  -- res1 with manual
  mask $ \restore -> do
    (destroyR1, r1) <- manual restore res1
    liftIO $ print r1
    destroyR1

  liftIO $ putStrLn "\n--\n"

  runResource $ do
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
res1 = do
  liftIO $ putStrLn "onCreate before"
  onDestroy $ liftIO $ putStrLn "onDestroy before"
  a <- resource $ res "A1" 1
  liftIO $ putStrLn "onCreate between"
  onDestroy $ liftIO $ putStrLn "onDestroy between"
  b <- resource $ res "B1" (a+3)
  liftIO $ putStrLn "onCreate after"
  onDestroy $ liftIO $ putStrLn "onDestroy after"
  return b

-- | destruction inversion
res2 :: Resource Int
res2 = do
  liftIO $ putStrLn "onCreate before"
  onDestroy $ liftIO $ putStrLn "onDestroy before"
  b <- resourceMask $ \restore -> do
    (destroyA, a) <- liftProg . manual restore $ res "A1" 1
    liftIO $ putStrLn "onCreate between"
    onDestroy $ liftIO $ putStrLn "onDestroy between creation"
    b <- resource $ res "B1" (a+3)
    onDestroy $ liftIO $ putStrLn "onDestroy between destruction"
    onDestroy destroyA
    return b
  liftIO $ putStrLn "onCreate after"
  onDestroy $ liftIO $ putStrLn "onDestroy after"
  return b
