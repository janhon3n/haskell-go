module Player where
import Board
import UI

data PlayerType = Human Side | DumbAi Side | SmartAi Side deriving (Show)

class DecisionMaker a where
    choosePlace :: a -> Board -> IO Place

instance DecisionMaker PlayerType where
    choosePlace (Human side) board = do
        getPlace side (getDimensions board)
    
    choosePlace (DumbAi side) board = return $ choosePlace' (DumbAi side) board (0,0)
    choosePlace (SmartAi side) board = return $ choosePlace' (SmartAi side) board (0,0)

choosePlace' :: PlayerType -> Board -> Place -> Place
choosePlace' (DumbAi side) board place = do
    if (dataAtPlace board place == Empty)
        then place
        else choosePlace' (DumbAi side) board (nextPlace board place)

choosePlace'' :: PlayerType -> Board -> Int -> Place
choosePlace'' (SmartAi side) board place = 