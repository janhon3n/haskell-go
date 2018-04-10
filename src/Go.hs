module Go where
import Board
import Region
import UI
import Player

getScore :: Board -> Side -> Int
getScore board side = do
    0


playGame :: IO Int
playGame = do
    let gameBoard = emptyBoard (9,9)
    return 0