{-# LANGUAGE DeriveGeneric #-}

module Player where
import Board
import Region
import GHC.Generics
   
data MoveType = StonePlacing | Passing | Finishing deriving (Eq, Show, Generic)
data Move = Move {
    moveType :: MoveType,
    place :: Place
} deriving (Eq, Show, Generic)

data PlayerType = Human | RandomAI | TreeAI deriving (Eq, Show, Generic)

data Player = Player { playerType :: PlayerType
    , playerSide :: Side
    , captured :: Int
    , hasPassed :: Bool
    , hasFinished :: Bool
    , finalScore :: Int
 } deriving (Eq, Show, Generic)


addCaptured :: Player -> Int -> Player
addCaptured (Player ptype side captured hasPassed hasFinished finalScore) amount = Player ptype side (captured + amount) hasPassed hasFinished finalScore

chooseMove :: Player -> Board -> Bool -> Move
chooseMove player@(Player playerType playerSide captured hasPassed hasFinished finalScore) board passAvailable = case playerType of
        Human -> Move Passing (0,0)
        RandomAI -> Move Passing (0,0)
        TreeAI -> Move Passing (0,0)


evaluateBoard :: Board -> Side -> Int
evaluateBoard board side = do
    (getScore board side) - (getScore board (opposite side))

data MinMaxPhase = Min | Max
type Depth = Int
data BoardNode = BoardNode Board Side

getChildNodes :: BoardNode -> [BoardNode]
getChildNodes node@(BoardNode board side) = do
    map (\p -> BoardNode (fst (placeStone board p)) (opposite side)) getAvailablePlaces board side

alphaBetaEvaluate :: BoardNode -> Depth -> (Float, Float) -> MinMaxPhase -> Float
alphaBetaEvaluate (BoardNode board side) 0 (alpha, beta) minmax = do
    if minmax == Max
        then evaluateBoard board side
        else evaluateBoard board (opposite side)

alphaBetaEvaluate node@(BoardNode board side) depth (alpha, beta) minmax = do
    let children = getChildNodes node
