{-# LANGUAGE DeriveGeneric #-}

module Move where
import Board
import GameState
import Region
import GHC.Generics
import Data.List

   
data MoveType = StonePlacing | Passing | Finishing deriving (Eq, Show, Generic)
data Move = Move {
    moveType :: MoveType,
    place :: Place
} deriving (Eq, Show, Generic)

moveIsValid :: GameState -> Move -> Bool
moveIsValid state (Move Passing _) = (gameOver state /= True) && (passIsAvailable state)
moveIsValid state (Move Finishing _) = not $ gameOver state

moveIsValid state move@(Move StonePlacing place) = do
      if (gameOver state) then False
      else do
         placeIsValid (board state) place &&
            dataAtPlace (board state) place == Empty &&
            checkIfSuicide (board state) place (sideInTurn state) /= True &&
            (prevBoard state) /= fst (placeStone (board state) place (sideInTurn state))

passIsAvailable :: GameState -> Bool
passIsAvailable state@(GameState board prevBoard playerInTurn otherPlayer gameOver) = do
   if (hasFinished otherPlayer || hasPassed playerInTurn) then False else True

{- Moves where placed stone is captured right away (== suicide) are not allowed -}
checkIfSuicide :: Board -> Place -> Side -> Bool
checkIfSuicide board place side = isDead board' (getUniformRegion board' place)
      where (board', _) = placeStone board place side

{- Move MUST be checked to be valid before executing it! -}
executeMove :: GameState -> Move -> GameState
executeMove state@(GameState board prevBoard playerInTurn otherPlayer gameOver) (Move moveType movePlace) = do
      case moveType of
         Passing -> do
            let newOtherPlayer = (Player (playerSide playerInTurn) (captured playerInTurn) True False 0)
            GameState board prevBoard otherPlayer newOtherPlayer gameOver

         Finishing -> do
            if (hasFinished otherPlayer)
            then do
                  let p1 = Player (playerSide playerInTurn) (captured playerInTurn) True True (captured playerInTurn + (getScore board (playerSide playerInTurn)))
                  let p2 = Player (playerSide otherPlayer) (captured otherPlayer) True True (captured otherPlayer + (getScore board (playerSide otherPlayer)))
                  GameState board prevBoard p1 p2 True
            else do
                  let newOtherPlayer = Player (playerSide playerInTurn) (captured playerInTurn) True True 0
                  let newPlayerInTurn = Player (playerSide otherPlayer) (captured otherPlayer) True True (captured otherPlayer + (getScore board (playerSide otherPlayer)))
                  GameState board prevBoard otherPlayer newOtherPlayer gameOver

         StonePlacing -> do
            let (newBoard, capturedAmount) = placeStone board movePlace (sideInTurn state)
            let newPlayerInTurn = otherPlayer
            let newOtherPlayer = (Player (playerSide playerInTurn) (captured playerInTurn + capturedAmount) False False 0)
            GameState newBoard board newPlayerInTurn newOtherPlayer False


type CapturedAmount = Int

placeStone :: Board -> Place -> Side -> (Board, CapturedAmount)
placeStone board place side = do
    let newBoard = addStoneToBoard board place side
    removeCaptured newBoard place side


getCapturedRegion :: Board -> Place -> Side -> Region
getCapturedRegion board place side = foldl (\reg p ->
        if elem p reg || dataAtPlace board p /= Stone (opposite side)
            then reg
            else do
                let newReg = (getUniformRegion board p)
                if isDead board newReg then union reg newReg else reg)
            [] $ getAdjacentPlaces board place

{- Removes captured regions that are connected to given place -}
removeCaptured :: Board -> Place -> Side -> (Board, CapturedAmount)
removeCaptured board place side = do
    let captured = getCapturedRegion board place side
    (fillRegion board captured Empty, length captured)