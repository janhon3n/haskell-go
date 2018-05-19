{-# LANGUAGE DeriveGeneric #-}

module GameState where
import GHC.Generics
import Board

data Player = Player { playerSide :: Side
    , captured :: Int
    , hasPassed :: Bool
    , hasFinished :: Bool
    , finalScore :: Int
 } deriving (Eq, Show, Generic)

data GameState = GameState { board :: Board
    , prevBoard :: Board
    , playerInTurn :: Player
    , otherPlayer :: Player
    , gameOver :: Bool
    } deriving (Eq, Show, Generic)

initialState :: BoardDimensions -> GameState
initialState boardDimensions = do
    GameState { board = emptyBoard boardDimensions
        , prevBoard = emptyBoard boardDimensions
        , playerInTurn = Player Black 0 False False 0
        , otherPlayer = Player White 0 False False 0
        , gameOver = False
    }
    
sideInTurn :: GameState -> Side
sideInTurn state = playerSide (playerInTurn state)