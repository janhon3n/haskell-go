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
getAdjacentPlaces board (row,col) = filter (placeIsValid board) [(row+1, col), (row-1, col), (row, col+1), (row, col-1)]

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
placeIsValid board place = case place of
    (row, col) | row < 0 -> False
    (row, col) | col < 0 -> False
    (row, col) | row >= length board -> False
    (row, col) | col >= length (board !! 0) -> False
    _ -> True

{- TODO Add a PlaceData to the board for position Place -}
addStoneToBoard :: Board -> Place -> Side -> Board
addStoneToBoard board place side = setDataAtPlace board place (Stone side)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newValue (x:xs) =
        if n == 0 
            then newValue : xs
            else x:replaceNth (n-1) newValue xs

setDataAtPlace :: Board -> Place -> PlaceData -> Board
setDataAtPlace board place placeData = replaceNth (fst place) (replaceNth (snd place) placeData (board !! (fst place))) board
    
nextPlace :: Board -> Place -> Place
nextPlace board place = do
    if (snd place >= snd (getDimensions board) - 1)
        then (fst place + 1, 0)
        else (fst place, snd place + 1)