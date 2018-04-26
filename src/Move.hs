{-# LANGUAGE DeriveGeneric #-}

module Move where
import Board
import GameState
import Player
import Region
import GHC.Generics

   
data MoveType = StonePlacing | Passing | Finishing deriving (Eq, Show, Generic)
data Move = Move {
    moveType :: MoveType,
    place :: Place
} deriving (Eq, Show, Generic)

moveIsValid :: GameState -> Move -> Bool
moveIsValid state (Move Passing _) = (gameOver state /= True) &&
         (hasPassed (playerInTurn state) /= True || hasPassed (otherPlayer state) /= True) &&
         (hasFinished (otherPlayer state) /= True)

moveIsValid state (Move Finishing _) = not $ gameOver state

moveIsValid state move@(Move StonePlacing place) = do
      if (gameOver state) then False
      else do
         placeIsValid (board state) place &&
            dataAtPlace (board state) place == Empty &&
            checkIfSuicide (board state) place (sideInTurn state) /= True &&
            not (length (boardHistory state) >= 2 &&
            (boardHistory state) !! 0 == board (executeMove state move))

checkIfSuicide :: Board -> Place -> Side -> Bool
checkIfSuicide board place side = do
      let board' = addStoneToBoard board place side
      let (board'', _) = removeCaptured board' place side
      and $ map (\place -> dataAtPlace board'' place == (Stone (opposite side))) $ getAdjacentPlaces board'' place
            
{- Move MUST be checked to be valid before executing it! -}
executeMove :: GameState -> Move -> GameState
executeMove state@(GameState board boardHistory playerInTurn otherPlayer gameOver) (Move moveType movePlace) = do
      case moveType of
         Passing -> do
            let newOtherPlayer = (Player (playerSide playerInTurn) (captured playerInTurn) True False 0)
            GameState board boardHistory otherPlayer newOtherPlayer gameOver

         Finishing -> do
            if (hasFinished otherPlayer)
            then do
                  let p1 = Player (playerSide playerInTurn) (captured playerInTurn) True True (captured playerInTurn + (getScore board (playerSide playerInTurn)))
                  let p2 = Player (playerSide otherPlayer) (captured otherPlayer) True True (captured otherPlayer + (getScore board (playerSide otherPlayer)))
                  GameState board boardHistory p1 p2 True
            else do
                  let newOtherPlayer = Player (playerSide playerInTurn) (captured playerInTurn) True True 0
                  let newPlayerInTurn = Player (playerSide otherPlayer) (captured otherPlayer) True True (captured otherPlayer + (getScore board (playerSide otherPlayer)))
                  GameState board boardHistory otherPlayer newOtherPlayer gameOver

         StonePlacing -> do
            let newHistory = board : boardHistory
            let newBoard = addStoneToBoard board movePlace (sideInTurn state)
            let (newBoard', capturedAmount) = removeCaptured newBoard movePlace (sideInTurn state)
            let newPlayerInTurn = otherPlayer
            let newOtherPlayer = (Player (playerSide playerInTurn) (captured playerInTurn + capturedAmount) False False 0)
            GameState newBoard' newHistory newPlayerInTurn newOtherPlayer False

removeCaptured :: Board -> Place -> Side -> (Board, Int)
removeCaptured board place side = do
    let capturedRegions = filter (\r -> getBorderType board r == (RegionType (Stone side))) $ map (getUniformRegion board) $ filter (\p -> dataAtPlace board p == (Stone (opposite side))) (getAdjacentPlaces board place)
    let points = sum (map length capturedRegions)
    let board' = foldl (\b r -> fillRegion b r Empty) board capturedRegions
    (board', points)
