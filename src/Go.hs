{-# LANGUAGE DeriveGeneric #-}

module Go where
import Board
import Region
import UI
import Player
import GHC.Generics

data MoveType = StonePlacing | Passing | Finishing deriving (Eq, Show, Generic)
data Move = Move {
    moveType :: MoveType,
    place :: Place
} deriving (Eq, Show, Generic)

data GameState = GameState { board :: Board
    , boardHistory :: [Board]
    , playerInTurn :: Player
    , otherPlayer :: Player
    , gameOver :: Bool
    } deriving (Eq, Show, Generic)

sideInTurn :: GameState -> Side
sideInTurn state = playerSide (playerInTurn state)

initialState :: BoardDimensions -> (PlayerType, PlayerType) -> GameState
initialState boardDimensions ptypes = do
    GameState { board = emptyBoard boardDimensions
        , boardHistory = []
        , playerInTurn = Player (fst ptypes) Black 0 False False
        , otherPlayer = Player (snd ptypes) White 0 False False
        , gameOver = False
    }

getScore :: Board -> Side -> Int
getScore board side = do
    1

playGame :: IO Int
playGame = do
    return 0

playLoop :: GameState -> (Int, Int)
playLoop state = do
    (0,0)

moveIsValid :: GameState -> Move -> Bool
moveIsValid state (Move Passing _) = (gameOver state /= True) &&
        (hasPassed (playerInTurn state) /= True || hasPassed (otherPlayer state) /= True)

moveIsValid state (Move Finishing _) = not $ gameOver state

moveIsValid state (Move StonePlacing place) = do
    if (gameOver state) then False
    else do
        placeIsValid (board state) place &&
            dataAtPlace (board state) place == Empty &&
            checkIfSuicide (board state) place (sideInTurn state) /= True &&
            not (length (boardHistory state) >= 2 &&
            (boardHistory state) !! 1 == (addStoneToBoard (board state) place (sideInTurn state)))

checkIfSuicide :: Board -> Place -> Side -> Bool
checkIfSuicide board place side = do
    and $ map (\place -> dataAtPlace board place == (Stone (opposite side))) $ getAdjacentPlaces board place
            
{- Move MUST be checked to be valid before executing it! -}
executeMove :: GameState -> Move -> GameState
executeMove state@(GameState board boardHistory playerInTurn otherPlayer gameOver) (Move moveType movePlace) = do
    case moveType of
        Passing -> do
            let newOtherPlayer = (Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn) True False)
            GameState board boardHistory otherPlayer newOtherPlayer gameOver

        Finishing -> do
            if (hasFinished otherPlayer) then GameState board boardHistory otherPlayer playerInTurn True
            else do
                let newOtherPlayer = (Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn) True True)
                GameState board boardHistory otherPlayer newOtherPlayer gameOver

        StonePlacing -> do
            let newHistory = board : boardHistory
            let newBoard = addStoneToBoard board movePlace (sideInTurn state)
            let (newBoard', capturedAmount) = removeCaptured newBoard movePlace (sideInTurn state)
            let newPlayerInTurn = otherPlayer
            let newOtherPlayer = (Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn + capturedAmount) False False)
            GameState newBoard' newHistory newPlayerInTurn newOtherPlayer False

removeCaptured :: Board -> Place -> Side -> (Board, Int)
removeCaptured board place side = do
    let capturedRegions = filter (\r -> getBorderType board r == (RegionType (Stone side))) $ map (getUniformRegion board) $ filter (\p -> dataAtPlace board p == (Stone (opposite side))) (getAdjacentPlaces board place)
    let points = sum (map length capturedRegions)
    let board' = foldl (\b r -> fillRegion b r Empty) board capturedRegions
    (board', points)

testBoard = [[Empty, Empty, Stone White, Empty], [Stone Black, Empty, Stone White, Empty], [Empty, Stone Black, Stone White, Stone White], [Stone Black, Stone Black, Empty, Stone White]]
