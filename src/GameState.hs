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

initialState :: BoardDimensions -> (PlayerType, PlayerType) -> GameState
initialState boardDimensions ptypes = do
    GameState { board = emptyBoard boardDimensions
        , boardHistory = []
        , playerInTurn = Player (fst ptypes) Black 0 False False 0
        , otherPlayer = Player (snd ptypes) White 0 False False 0
        , gameOver = False
    }
    
sideInTurn :: GameState -> Side
sideInTurn state = playerSide (playerInTurn state)