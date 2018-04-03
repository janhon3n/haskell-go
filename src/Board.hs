module Board where

data Side = Black | White deriving (Show, Eq)

{- Place = paikan koordinaatit laudalla -}
type Place = (Int, Int)
{- PlaceData = paikan sisältö (tyhjä, musta tai valkoinen -}
data PlaceData = Empty | Piece Side deriving (Eq, Show)

type BoardDimensions = (Int, Int)

type Row = [PlaceData]
type Board = [Row]

getDimensions :: Board -> BoardDimensions
getDimensions board = (length board, length (board !! 0))

{- Empty board creation -}
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
updateBoard board place placeData = board