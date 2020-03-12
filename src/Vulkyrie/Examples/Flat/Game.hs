{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Vulkyrie.Examples.Flat.Game
  ( GameState (..)
  , Event (..)
  , initialGameState
  , runGame
  ) where

import           Apecs
import           Control.Concurrent
import           Control.Monad
import           Graphics.UI.GLFW   (Key, KeyState)
import qualified Graphics.UI.GLFW   as GLFW
import           Numeric.DataFrame


newtype Position = Position Vec2i deriving Show
instance Component Position where type Storage Position = Map Position

data Wall = Wall deriving Show
instance Component Wall where type Storage Wall = Map Wall

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Time = Time Double deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

-- what about ''Camera? see Apecs.Gloss

-- creates type World and function initWorld :: IO World
makeWorld "World" [''Position, ''Player, ''Wall, ''Time]

type System' a = System World a

initialize :: System' ()
initialize = do
  _ <- newEntity (Player, Position (vec2 0 0))
  let wall p = newEntity (Wall, Position p) >> return ()
  mapM_ wall [vec2 x y | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y == 0)]
  return ()

handleEvent :: Event -> System' ()
handleEvent (KeyEvent key keyState) =
  let motion =
        case (key, keyState) of
          (GLFW.Key'Left, GLFW.KeyState'Released)  -> vec2 (-1) 0
          (GLFW.Key'Right, GLFW.KeyState'Released) -> vec2 1 0
          (GLFW.Key'Up, GLFW.KeyState'Released)    -> vec2 0 (-1)
          (GLFW.Key'Down, GLFW.KeyState'Released)  -> vec2 0 1
          _                                        -> 0
  in cmap $ \(Player, Position pos) -> Position (pos + motion)
handleEvent (Tick time) = set global (Time time)


data Event
  = KeyEvent Key KeyState
  | Tick Double


runGame :: MVar GameState -> Chan Event -> IO ()
runGame gsVar eventChan = do
  w <- initWorld
  _ <- forkIO $ forever $ do
    threadDelay 16667
    t <- GLFW.getTime >>= \case
      Just time -> return time
      Nothing -> error "GLFW.getTime failed"
    writeChan eventChan (Tick t)
  runWith w $ do
    initialize
    forever $ do
      event <- liftIO $ readChan eventChan
      handleEvent event
      -- now the consequences of the event can propagate
      -- modifyMVar_ gsVar gameStateUpdate -- that would need unliftIO
      oldGs <- liftIO $ takeMVar gsVar
      newGs <- gameStateUpdate oldGs
      liftIO $ putMVar gsVar newGs

-- | This is the glue between game logic and graphics code.
--   It carries only what the graphics part needs.
data GameState
  = GameState
  { camPos :: Vec2f
  , walls :: [Vec2i]
  }

initialGameState :: GameState
initialGameState = GameState
  { camPos = 0
  , walls = []
  }

gameStateUpdate :: GameState -> System' GameState
gameStateUpdate oldGs = do
  mPos <- cfold (\_ (Player, Position pos) -> Just pos) Nothing
  let Vec2 x y = case mPos of
        Just pl -> pl
        Nothing -> error "player entity doesn't exist"
  walls <- cfold (\ws (Wall, Position w) -> w:ws) []
  return oldGs { camPos = Vec2 (fromIntegral x) (fromIntegral y), walls}
