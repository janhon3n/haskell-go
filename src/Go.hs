module Go where
import Board
import Region
import UI
import Player

data Captured = Captured (Side, Int) (Side, Int) deriving (Eq, Show)

addCaptured :: Captured -> Side -> Int -> Captured
addCaptured (Captured tuple1 tuple2) side amount = do
    if side == fst tuple1
        then (Captured (side, snd tuple1 + amount) tuple2) 
        else (Captured tuple1 (side, snd tuple2 + amount)) 

data GameState = GameState { board :: Board
    , boardHistory :: [Board]
    , sideInTurn :: Side
    , captured :: Captured
    } deriving (Eq, Show)

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
    let newCaptured = addCaptured (captured state) (sideInTurn state) capturedAmount
    let newSideInTurn = opposite (sideInTurn state)
    GameState {board = newBoard, boardHistory = newHistory, captured = newCaptured, sideInTurn = newSideInTurn }

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
testState = GameState { board = testBoard
        , boardHistory = []
        , sideInTurn = Black
        , captured = Captured (Black, 1) (White, 0)
        }
    