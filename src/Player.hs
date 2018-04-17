module Player where
import Board
import UI

data PlayerType = Human | DumbAi | SmartAi deriving (Eq, Show)
data Player = Player PlayerType Side Int deriving (Eq, Show)
data BoardTree = BoardTree Board [BoardTree]

getSide :: Player -> Side
getSide (Player _ side _) = side

addCaptured :: Player -> Int -> Player
addCaptured (Player ptype side captured) amount = Player ptype side (captured + amount)

class DecisionMaker a where
    choosePlace :: a -> Board -> IO Place

instance DecisionMaker Player where
    choosePlace (Player Human side points) board = do
        getPlace side (getDimensions board)
    
    choosePlace (Player DumbAi side points) board = return $ choosePlace' (Player DumbAi side points) board (0,0)
    choosePlace (Player SmartAi side points) board = return $ choosePlace' (Player DumbAi side points) board (0,0)

choosePlace' :: Player -> Board -> Place -> Place
choosePlace' (Player DumbAi side points) board place = do
    if (dataAtPlace board place == Empty)
        then place
        else choosePlace' (Player DumbAi side points) board (nextPlace board place)

choosePlaceWithTree :: Player -> Board -> Place
choosePlaceWithTree (Player SmartAi side points) board = do
    let boardTree = createBoardTree board 3
    (0,0)

createBoardTree :: Board -> Int -> Side -> BoardTree
createBoardTree board 0 side = BoardTree board []
-- createBoardTree board side depth = BoardTree board (map (\p -> createBoardTree (placeStone board side p) (depth-1) (oppositeSide side)) (getFreePlaces board))

getFreePlaces :: Board -> [Place]
getFreePlaces board = filter (placeIsFree board) [(rows,cols) | rows <- [0..(rowCount board - 1)], cols <- [0..(columnCount board -1)]]

placeIsFree :: Board -> Place -> Bool
placeIsFree board place = placeIsValid board place && dataAtPlace board place == Empty