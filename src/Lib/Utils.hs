module Lib.Utils where

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
