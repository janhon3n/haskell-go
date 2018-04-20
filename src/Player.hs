{-# LANGUAGE DeriveGeneric #-}

module Player where
import Board
import UI
import GHC.Generics

data PlayerType = Human | RandomAI | TreeAI deriving (Eq, Show, Generic)
data Player = Player { playerType :: PlayerType
    , playerSide :: Side
    , captured :: Int
    , hasPassed :: Bool
    , hasFinished :: Bool
 } deriving (Eq, Show, Generic)

data BoardTree = BoardTree Board [BoardTree]

addCaptured :: Player -> Int -> Player
addCaptured (Player ptype side captured hasPassed hasFinished) amount = Player ptype side (captured + amount) hasPassed hasFinished

class DecisionMaker a where
    choosePlace :: a -> Board -> IO Place

instance DecisionMaker Player where
    choosePlace player@(Player playerType playerSide captured hasPassed hasFinished) board = case playerType of
        Human -> getPlace playerSide (getDimensions board)
        RandomAI -> return $ chooseFirstEmptyPlace player board (0,0)
        TreeAI -> return (0,0)

chooseFirstEmptyPlace :: Player -> Board -> Place -> Place
chooseFirstEmptyPlace player board place = do
    if (dataAtPlace board place == Empty)
        then place
        else chooseFirstEmptyPlace player board (nextPlace board place)
