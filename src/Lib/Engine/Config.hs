module Lib.Engine.Config where

data SyncMode = VSyncTriple | VSync | NoSync deriving (Eq, Ord, Show)

data Flag = Validation deriving (Eq, Ord, Show)
