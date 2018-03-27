module Go where
import Board

playGame :: Int
playGame = do
    let gameBoard = emptyBoard (9,9)
    0