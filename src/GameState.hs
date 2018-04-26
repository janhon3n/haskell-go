{-# LANGUAGE DeriveGeneric #-}

module GameState where
import GHC.Generics
import Board
import Player

data GameState = GameState { board :: Board
   , boardHistory :: [Board]
   , playerInTurn :: Player
   , otherPlayer :: Player
   , gameOver :: Bool
   } deriving (Eq, Show, Generic)

initialState :: BoardDimensions -> GameState
initialState boardDimensions = do
    GameState { board = emptyBoard boardDimensions
        , boardHistory = []
        , playerInTurn = Player Black 0 False False 0
        , otherPlayer = Player White 0 False False 0
        , gameOver = False
    }
    
sideInTurn :: GameState -> Side
sideInTurn state = playerSide (playerInTurn state)