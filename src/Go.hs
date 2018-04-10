module Go where
import Board
import Region
import UI
import Player

testBoard = [[Empty, Empty, Stone White, Empty], [Stone Black, Empty, Stone White, Empty], [Empty, Stone Black, Stone White, Stone White], [Stone Black, Stone Black, Empty, Stone White]]

getScore :: Board -> Side -> Int
getScore board side = do
    1


playGame :: IO Int
playGame = do
    let gameBoard = emptyBoard (9,9)
    return 0

playLoop :: Board -> Board -> (Int, Int) -> Side -> (Int, Int)
playLoop board pastBoard scores sideInTurn = do
    (0,0)

removeCaptured :: Board -> Place -> Side -> (Board, Int)
removeCaptured board place side = do
    let capturedRegions = filter (\r -> getBorderType board r == (RegionType (Stone side))) $ map (getUniformRegion board) $ filter (\p -> dataAtPlace board p == (Stone (opposite side))) (getAdjacentPlaces board place)
    let points = sum (map length capturedRegions)
    foldl (\b r -> fillRegion b r Empty) board capturedRegions


testBoard = [[Empty, Empty, Stone White, Empty], [Stone Black, Empty, Stone White, Empty], [Empty, Stone Black, Stone White, Stone White], [Stone Black, Stone Black, Empty, Stone White]]
