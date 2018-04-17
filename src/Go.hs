module Go where
import Board
import Region
import UI
import Player

data GameState = GameState { board :: Board
    , boardHistory :: [Board]
    , playerInTurn :: Player
    , otherPlayer :: Player
    } deriving (Eq, Show)

sideInTurn :: GameState -> Side
sideInTurn state = getSide (playerInTurn state)

getScore :: Board -> Side -> Int
getScore board side = do
    1

playGame :: IO Int
playGame = do
    let gameBoard = emptyBoard (9,9)
    return 0

playLoop :: GameState -> (Int, Int)
playLoop state = do
    (0,0)

moveIsValid :: GameState -> Place -> Bool
moveIsValid state place = do
    placeIsValid (board state) place &&
        dataAtPlace (board state) place == Empty &&
        checkIfSuicide (board state) place (sideInTurn state) /= True &&
        not (length (boardHistory state) >= 2 &&
        (boardHistory state) !! 1 == (addStoneToBoard (board state) place (sideInTurn state)))

executeMove :: GameState -> Place -> GameState
executeMove state place = do
    let newHistory = (board state) : (boardHistory state)
    let newBoard = addStoneToBoard (board state) place (sideInTurn state)
    let (newBoard', capturedAmount) = removeCaptured newBoard place (sideInTurn state)
    GameState {board = newBoard', boardHistory = newHistory, playerInTurn = (otherPlayer state), otherPlayer = addCaptured (playerInTurn state) capturedAmount }

checkIfSuicide :: Board -> Place -> Side -> Bool
checkIfSuicide board place side = do
    and $ map (\place -> dataAtPlace board place == (Stone (opposite side))) $ getAdjacentPlaces board place

removeCaptured :: Board -> Place -> Side -> (Board, Int)
removeCaptured board place side = do
    let capturedRegions = filter (\r -> getBorderType board r == (RegionType (Stone side))) $ map (getUniformRegion board) $ filter (\p -> dataAtPlace board p == (Stone (opposite side))) (getAdjacentPlaces board place)
    let points = sum (map length capturedRegions)
    let board' = foldl (\b r -> fillRegion b r Empty) board capturedRegions
    (board', points)

testBoard = [[Empty, Empty, Stone White, Empty], [Stone Black, Empty, Stone White, Empty], [Empty, Stone Black, Stone White, Stone White], [Stone Black, Stone Black, Empty, Stone White]]

    