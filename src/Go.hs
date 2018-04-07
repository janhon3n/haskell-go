module Go where
import Board
import Region
import UI
import Player

getScore :: Board -> Side -> Int
getScore board side = do
    let emptyRegions = filter (\r -> getRegionType board r == RegionType Empty) (getRegions board)
    let scoreRegions = filter (\r -> getBorderType board r == RegionType (Stone side)) emptyRegions
    sum $ map length scoreRegions


playGame :: IO Int
playGame = do
    let gameBoard = emptyBoard (9,9)
    return 0