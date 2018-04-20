{-# LANGUAGE DeriveGeneric #-}

module Go where
import Board
import Region
import Player
import GameState
import Move
import GHC.Generics

getScore :: Board -> Side -> Int
getScore board side = do
    1

playGame :: IO Int
playGame = do
    return 0

playLoop :: GameState -> (Int, Int)
playLoop state = do
    (0,0)



chooseValidMove :: GameState -> Move
chooseValidMove state = chooseValidMove' state 10

chooseValidMove' :: GameState -> Int -> Move
chooseValidMove' state 0 = Move Finishing (0,0)
chooseValidMove' state triesLeft = do
    let place = choosePlace (playerInTurn state) (board state)
    let move = Move StonePlacing place
    if (moveIsValid state move)
        then move
        else chooseValidMove' state (triesLeft - 1)