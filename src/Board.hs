{-# LANGUAGE DeriveGeneric #-}

module Board where
import GHC.Generics

data Side = Black | White deriving (Eq, Show, Generic)

opposite :: Side -> Side
opposite Black = White
opposite White = Black

{- Place = paikan koordinaatit laudalla (rivi, sarake) -}
type Place = (Int, Int)

getAdjacentPlaces :: Board -> Place -> [Place]
getAdjacentPlaces board (row, col) = filter (placeIsValid board) [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

{- PlaceData = paikan sisältö (tyhjä, musta tai valkoinen -}
data PlaceData = Empty | Stone {
    side :: Side
 } deriving (Eq, Show, Generic)
type BoardDimensions = (Int, Int)

{- esim [Empty, Empty, Stone Black, Stone White] -}
type Row = [PlaceData]
type Board = [Row]

{- Empty board creation -}
emptyBoard :: BoardDimensions -> Board
emptyBoard dim = replicate (fst dim) (emptyRow (snd dim))

emptyRow :: Int -> Row
emptyRow size = replicate size Empty

getDimensions :: Board -> BoardDimensions
getDimensions board = (length board, length (board !! 0))

rowCount :: Board -> Int
rowCount board = fst (getDimensions board)

columnCount :: Board -> Int
columnCount board = snd (getDimensions board)

dataAtPlace :: Board -> Place -> PlaceData
dataAtPlace board place = board !! (fst place) !! (snd place)

{- Returns true if place is inside the board -}
placeIsValid :: Board -> Place -> Bool
placeIsValid [] _ = False
placeIsValid board (row, col) = not $ row >= rowCount board || row < 0 || col >= columnCount board || col < 0

{- TODO Add a PlaceData to the board for position Place -}
addStoneToBoard :: Board -> Place -> Side -> Board
addStoneToBoard board place side = setDataAtPlace board place (Stone side)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newValue (x:xs) = case n of
    0 -> newValue : xs
    _ -> x:replaceNth (n-1) newValue xs

setDataAtPlace :: Board -> Place -> PlaceData -> Board
setDataAtPlace board (row, col) placeData = replaceNth row (replaceNth col placeData (board !! row)) board

nextPlace :: Board -> Place -> Maybe Place
nextPlace board place = case place of
    (row, col) | col == (columnCount board) - 1 && row == (rowCount board) - 1 -> Nothing
    (row, col) | col == (columnCount board) - 1 -> Just (row+1, 0)
    (row, col) -> Just (row, col+1)
