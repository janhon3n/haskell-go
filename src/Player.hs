{-# LANGUAGE DeriveGeneric #-}

module Player where
import Board
import GHC.Generics

data Player = Player { playerSide :: Side
    , captured :: Int
    , hasPassed :: Bool
    , hasFinished :: Bool
    , finalScore :: Int
 } deriving (Eq, Show, Generic)
