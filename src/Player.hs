{-# LANGUAGE DeriveGeneric #-}

module Player where
import Board
import GHC.Generics

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

class DecisionMaker a where
    choosePlace :: a -> Board -> Place

instance DecisionMaker Player where
    choosePlace player@(Player playerType playerSide captured hasPassed hasFinished finalScore) board = case playerType of
        Human -> (0,0)
        RandomAI -> (0,0)
        TreeAI -> (0,0)
