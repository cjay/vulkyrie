module Vulkyrie.Tests where

import Vulkyrie.Program
import Vulkyrie.Resource

tests :: Program r ()
tests = do
  resourceTest

resourceTest :: Program r ()
resourceTest = do
  locally $ do
    let ra = res "A" 1
    let rb = res "B" 2
    liftIO $ putStrLn "before"
    auto ra
    liftIO $ putStrLn "between"
    auto rb
    liftIO $ putStrLn "after"

  liftIO $ putStrLn "\n--\n"

  -- res1 with auto
  locally $ do
    r1 <- auto res1
    liftIO $ print r1

  liftIO $ putStrLn "\n--\n"

  -- res1 with manual
  (destroyR1, r1) <- manual res1
  liftIO $ print r1
  locally $ destroyR1

  liftIO $ putStrLn "\n--\n"

  locally $ do
    r2 <- auto res2
    liftIO $ print r2

  return ()

-- | simple resource
res :: String -> Int -> MetaResource r Int
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
res1 = do
  onCreate $ liftIO $ putStrLn "onCreate before"
  onDestroy $ liftIO $ putStrLn "onDestroy before"
  a <- resource $ res "A1" 1
  onCreate $ liftIO $ putStrLn "onCreate between"
  onDestroy $ liftIO $ putStrLn "onDestroy between"
  b <- resource $ res "B1" (a+3)
  onCreate $ liftIO $ putStrLn "onCreate after"
  onDestroy $ liftIO $ putStrLn "onDestroy after"
  return b

-- | destruction inversion
res2 = do
  onCreate $ liftIO $ putStrLn "onCreate before"
  onDestroy $ liftIO $ putStrLn "onDestroy before"
  (destroyA, a) <- onCreate . manual $ res "A1" 1
  onCreate $ liftIO $ putStrLn "onCreate between"
  onDestroy $ liftIO $ putStrLn "onDestroy between creation"
  b <- resource $ res "B1" (a+3)
  onDestroy $ liftIO $ putStrLn "onDestroy between destruction"
  onDestroy $ destroyA
  onCreate $ liftIO $ putStrLn "onCreate after"
  onDestroy $ liftIO $ putStrLn "onDestroy after"
  return b
