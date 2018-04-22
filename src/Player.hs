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


data BoardTree = BoardTree Board [BoardTree]

addCaptured :: Player -> Int -> Player
addCaptured (Player ptype side captured hasPassed hasFinished finalScore) amount = Player ptype side (captured + amount) hasPassed hasFinished finalScore

chooseMove :: Player -> Board -> Bool -> Move
chooseMove player@(Player playerType playerSide captured hasPassed hasFinished finalScore) board passAvailable = case playerType of
        Human -> Move Passing (0,0)
        RandomAI -> Move Passing (0,0)
        TreeAI -> Move Passing (0,0)

evaluateBoard :: Board -> Side -> Int
evaluateBoard board side = do
    getScore board side