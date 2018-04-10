module Player where
import Board
import UI

data PlayerType = Human Side | DumbAi Side | SmartAi Side deriving (Show)
data BoardTree = BoardTree Board [BoardTree]


class DecisionMaker a where
    choosePlace :: a -> Board -> IO Place

instance DecisionMaker PlayerType where
    choosePlace (Human side) board = do
        getPlace side (getDimensions board)
    
    choosePlace (DumbAi side) board = return $ choosePlace' (DumbAi side) board (0,0)
    choosePlace (SmartAi side) board = return $ choosePlace' (DumbAi side) board (0,0)

choosePlace' :: PlayerType -> Board -> Place -> Place
choosePlace' (DumbAi side) board place = do
    if (dataAtPlace board place == Empty)
        then place
        else choosePlace' (DumbAi side) board (nextPlace board place)

choosePlaceWithTree :: PlayerType -> Board -> Place
choosePlaceWithTree (SmartAi side) board = do
    let boardTree = createBoardTree board 3
    (0,0)

createBoardTree :: Board -> Int -> Side -> BoardTree
createBoardTree board side 0 = BoardTree board []
createBoardTree board side depth = BoardTree board (map (\p = createBoardTree (placeStone board side p) (depth-1) (oppositeSide side)) (getFreePlaces board))

getFreePlaces :: Board -> [Place]
getFreePlaces board = filter (placeIsFree board) [(rows,cols) | rows <- [0..(rowCount board - 1)], cols <- [0..(columnCount board -1)]]

placeIsFree :: Board -> Place -> Bool
placeIsFree board place = placeIsValid board place && dataAtPlace board place == Empty