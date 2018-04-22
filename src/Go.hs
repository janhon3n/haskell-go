{-# LANGUAGE DeriveGeneric #-}

module Go where
import GHC.Generics
import Board
import Player
import Region

data GameState = GameState { board :: Board
   , boardHistory :: [Board]
   , playerInTurn :: Player
   , otherPlayer :: Player
   , gameOver :: Bool
   } deriving (Eq, Show, Generic)

initialState :: BoardDimensions -> (PlayerType, PlayerType) -> GameState
initialState boardDimensions ptypes = do
    GameState { board = emptyBoard boardDimensions
        , boardHistory = []
        , playerInTurn = Player (fst ptypes) Black 0 False False 0
        , otherPlayer = Player (snd ptypes) White 0 False False 0
        , gameOver = False
    }
    
sideInTurn :: GameState -> Side
sideInTurn state = playerSide (playerInTurn state)

passIsAvailable :: GameState -> Bool
passIsAvailable state@(GameState board boardHistory playerInTurn otherPlayer gameOver) = do
   if (hasFinished otherPlayer || hasPassed playerInTurn) then False else True

moveIsValid :: GameState -> Move -> Bool
moveIsValid state (Move Passing _) = (gameOver state /= True) && (passIsAvailable state)
moveIsValid state (Move Finishing _) = not $ gameOver state

moveIsValid state move@(Move StonePlacing place) = do
      if (gameOver state) then False
      else do
         placeIsValid (board state) place &&
            dataAtPlace (board state) place == Empty &&
            isSuicide state move /= True &&
            not (length (boardHistory state) >= 2 &&
            (boardHistory state) !! 0 == board (executeMove state move))

isSuicide :: GameState -> Move -> Bool
isSuicide state move@(Move StonePlacing place) = do
      let newState = executeMove state move
      isDead (board newState) (getUniformRegion (board newState) place)
            
{- Move MUST be checked to be valid before executing it! -}
executeMove :: GameState -> Move -> GameState
executeMove state@(GameState board boardHistory playerInTurn otherPlayer gameOver) (Move moveType movePlace) = do
      case moveType of
         Passing -> do
            let newOtherPlayer = (Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn) True False 0)
            GameState board boardHistory otherPlayer newOtherPlayer gameOver

         Finishing -> do
            if (hasFinished otherPlayer)
            then do
                  let p1 = Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn) True True (captured playerInTurn + (getScore board (playerSide playerInTurn)))
                  let p2 = Player (playerType otherPlayer) (playerSide otherPlayer) (captured otherPlayer) True True (captured otherPlayer + (getScore board (playerSide otherPlayer)))
                  GameState board boardHistory p1 p2 True
            else do
                  let newOtherPlayer = Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn) True True 0
                  let newPlayerInTurn = Player (playerType otherPlayer) (playerSide otherPlayer) (captured otherPlayer) True True (captured otherPlayer + (getScore board (playerSide otherPlayer)))
                  GameState board boardHistory otherPlayer newOtherPlayer gameOver

         StonePlacing -> do
            let newHistory = board : boardHistory
            let newBoard = addStoneToBoard board movePlace (sideInTurn state)
            let (newBoard', capturedAmount) = removeCaptured newBoard movePlace (sideInTurn state)
            let newPlayerInTurn = otherPlayer
            let newOtherPlayer = (Player (playerType playerInTurn) (playerSide playerInTurn) (captured playerInTurn + capturedAmount) False False 0)
            GameState newBoard' newHistory newPlayerInTurn newOtherPlayer False

removeCaptured :: Board -> Place -> Side -> (Board, Int)
removeCaptured board place side = do
   let places = filter (\p -> dataAtPlace board p == (Stone (opposite side))) $ getAdjacentPlaces board place
   let adjacentRegions = foldl (\regs p -> do
         let newReg = getUniformRegion board p
         if elem newReg regs
            then regs
            else newReg : regs
         ) [] places
   let capturedRegions = filter (isDead board) adjacentRegions
   let points = sum $ map length capturedRegions
   let board' = foldl (\b r -> setRegionContent b r Empty) board capturedRegions
   (board', points)

chooseValidMove :: GameState -> Move
chooseValidMove state = chooseValidMove' state 10

chooseValidMove' :: GameState -> Int -> Move
chooseValidMove' state 0 = Move Finishing (0,0)
chooseValidMove' state triesLeft = do
    let move = chooseMove (playerInTurn state) (board state) (passIsAvailable state)
    if (moveIsValid state move)
        then move
        else chooseValidMove' state (triesLeft - 1)
        
executeAITurn :: GameState -> GameState
executeAITurn state = executeMove state $ chooseValidMove state