module Go where
import Board

playGame :: IO Int
playGame = do
    let gameBoard = emptyBoard (9,9)
    return 0