module Lib.Utils where

import Control.Concurrent

-- | Like forkIO, but prints when the thread starts and ends, and tells if it ends with an exception
debugForkIO :: IO () -> IO ThreadId
debugForkIO action = Control.Concurrent.forkFinally (announce >> action) finish where
  announce = do
    tid <- myThreadId
    putStrLn $ "New Thread (" ++ show tid ++ ")"
  finish res = do
    tid <- myThreadId
    let resStr = case res of Right _ -> "normally"
                             Left ex -> "with an exception: " ++ show ex
    putStrLn $ "Terminated Thread (" ++ show tid ++ ") " ++ resStr
