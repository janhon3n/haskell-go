module Board where

data Side = Black | White deriving (Show, Eq)

{- Place = paikan koordinaatit laudalla (rivi, sarake) -}
type Place = (Int, Int)

leftOf :: Place -> Place
leftOf place = (fst place, snd place - 1)
rightOf :: Place -> Place
leftOf place = (fst place, snd place + 1)
upOf :: Place -> Place
leftOf place = (fst place - 1, snd place)
downOf :: Place -> Place
leftOf place = (fst place + 1, snd place)


{- PlaceData = paikan sisältö (tyhjä, musta tai valkoinen -}
data PlaceData = Empty | Stone Side deriving (Eq, Show)

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

dataAtPlace :: Board -> Place -> PlaceData
dataAtPlace board place = board !! (fst place) !! (snd place)


{- Returns true if place is inside the board -}
placeIsValid :: Board -> Place -> Bool
placeIsValid [] _ = False
placeIsValid board place = if fst place >= length board || snd place >= length (board !! 0)
    then False
    else True

{- TODO Add a PlaceData to the board for position Place -}
addStoneToBoard :: Board -> Place -> PlaceData -> Board
addStoneToBoard board place placeData = board

{- Turha ehkä -}
nextPlace :: Board -> Place -> Place
nextPlace board place = do
    if (snd place >= snd (getDimensions board) - 1)
        then (fst place + 1, 0)
        else (fst place, snd place + 1)


{- TODO -}
getScore :: Board -> Side -> Int
getScore board side = do
    let regions = detectEmptyRegions board [] (0,0)
    0

