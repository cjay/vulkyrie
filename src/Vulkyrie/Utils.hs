module Vulkyrie.Utils where

import Control.Monad
import           Numeric.DataFrame
import Control.Concurrent
import qualified Control.Monad.ST        as ST
import qualified Numeric.DataFrame.ST    as ST


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


-- | A threadsafe token counter variable.
newtype NatTokenVar = NatTokenVar (MVar NatTokenState)

data NatTokenState
  = NatTokenState
  { waiting :: [MVar ()]
  , avail :: Int
    -- ^ Number of available tokens. Only gets negative when changeNumTokens reduces the amount.
  , amount :: Int
  }

newNatTokenVar :: Int -> IO NatTokenVar
newNatTokenVar n = do
  state <- newMVar $
    NatTokenState
      { waiting = []
      , avail = n
      , amount = n
      }
  return $ NatTokenVar state

-- | If more tokens are already acquried than the new amount, no waiting threads
-- are woken up until enough tokens have been returned.
changeNumTokens :: NatTokenVar -> Int -> IO ()
changeNumTokens (NatTokenVar stateVar) n = do
  state@NatTokenState{ avail, amount, waiting } <- takeMVar stateVar
  let delta = n - amount
      avail' = avail + delta
      wakeNum = min avail' (length waiting)
      avail'' = avail' - wakeNum
      (wake, waiting') = splitAt wakeNum waiting
  forM_ wake $ \box -> putMVar box ()
  putMVar stateVar $ state
    { waiting = waiting'
    , avail = avail''
    , amount = n
    }

-- | Blocks when all tokens have already been acquired. Unblocking in FIFO order.
acquireToken :: NatTokenVar -> IO ()
acquireToken (NatTokenVar stateVar) = do
  state@NatTokenState{ waiting, avail } <- takeMVar stateVar
  if avail > 0
    then putMVar stateVar $ state { avail = avail - 1 }
    else do
      answerBox <- newEmptyMVar
      putMVar stateVar $ state { waiting = waiting <> pure answerBox }
      takeMVar answerBox

releaseToken :: NatTokenVar -> IO ()
releaseToken (NatTokenVar stateVar) = do
  state@NatTokenState{ waiting, avail } <- takeMVar stateVar
  case waiting of
    w:ws | avail >= 0 -> do
      putMVar w ()
      putMVar stateVar $ state { waiting = ws }
    _ -> putMVar stateVar $ state { avail = avail + 1 }


-- copied from easytensor and fixed for Vulkan:

{-# INLINE mkMat #-}
mkMat ::
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  Mat44f
mkMat
  _11 _12 _13 _14
  _21 _22 _23 _24
  _31 _32 _33 _34
  _41 _42 _43 _44
  = ST.runST $ do
    df <- ST.newDataFrame
    ST.writeDataFrameOff df 0  $ scalar _11
    ST.writeDataFrameOff df 1  $ scalar _12
    ST.writeDataFrameOff df 2  $ scalar _13
    ST.writeDataFrameOff df 3  $ scalar _14
    ST.writeDataFrameOff df 4  $ scalar _21
    ST.writeDataFrameOff df 5  $ scalar _22
    ST.writeDataFrameOff df 6  $ scalar _23
    ST.writeDataFrameOff df 7  $ scalar _24
    ST.writeDataFrameOff df 8  $ scalar _31
    ST.writeDataFrameOff df 9  $ scalar _32
    ST.writeDataFrameOff df 10 $ scalar _33
    ST.writeDataFrameOff df 11 $ scalar _34
    ST.writeDataFrameOff df 12 $ scalar _41
    ST.writeDataFrameOff df 13 $ scalar _42
    ST.writeDataFrameOff df 14 $ scalar _43
    ST.writeDataFrameOff df 15 $ scalar _44
    ST.unsafeFreezeDataFrame df

{-# INLINE perspectiveVk #-}
perspectiveVk :: Float -> Float -> Float -> Float -> Mat44f
perspectiveVk n f fovy aspect = mkMat
  dpw 0   0   0
  0   dph 0   0
  0   0   a   1
  0   0   b   0
  where
    hpd = tan (fovy * 0.5) -- height/distance
    wpd = aspect * hpd; -- width/distance
    dph = recip hpd -- distance/height
    dpw = recip wpd -- distance/width
    fmn = f - n
    a = f / fmn
    b = - f * n / fmn


{-# INLINE orthogonalVk #-}
orthogonalVk :: Float -> Float -> Float -> Float -> Mat44f
orthogonalVk n f w h = mkMat
  iw 0  0 0
  0  ih 0 0
  0  0  a 0
  0  0  b 1
  where
    ih = 2 / h
    iw = 2 / w
    fmn = f - n
    a = 1 / fmn
    b = - n / fmn

scale :: Float -> Float -> Float -> Mat44f
scale x y z = mkMat
  x  0  0 0
  0  y  0 0
  0  0  z 0
  0  0  0 1
