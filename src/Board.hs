module Board where

data Side = Black | White deriving (Show)

type Place = (Int, Int)
data PlaceData = Empty | Side deriving (Eq, Show)

type BoardDimensions = (Int, Int)

type Row = [PlaceData]
type Board = [Row]

emptyBoard :: BoardDimensions -> Board
emptyBoard dim = replicate (fst dim) (emptyRow (snd dim))

emptyRow :: Int -> Row
emptyRow size = replicate size Empty

dataAtPlace :: Board -> Place -> PlaceData
dataAtPlace board place = board !! (fst place) !! (snd place)

placeIsValid :: Board -> Place -> Bool
placeIsValid [] _ = False
placeIsValid board place = if fst place >= length board || snd place >= length (board !! 0)
    then False
    else True

{- TODO Add a PlaceData to the board for position Place -}
updateBoard :: Board -> Place -> PlaceData -> Board
updateBoard b p pd = b