module Board where

data Side = Black | White deriving (Show, Eq)

opposite :: Side -> Side
opposite Black = White
opposite White = Black

{- Place = paikan koordinaatit laudalla (rivi, sarake) -}
type Place = (Int, Int)

leftOf :: Place -> Place
leftOf place = (fst place, snd place - 1)
rightOf :: Place -> Place
rightOf place = (fst place, snd place + 1)
upOf :: Place -> Place
upOf place = (fst place - 1, snd place)
downOf :: Place -> Place
downOf place = (fst place + 1, snd place)

getAdjacentPlaces :: Board -> Place -> [Place]
getAdjacentPlaces board place = filter (placeIsValid board) [downOf place, upOf place, leftOf place, rightOf place]

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

rowCount :: Board -> Int
rowCount board = fst (getDimensions board)

columnCount :: Board -> Int
columnCount board = snd (getDimensions board)

dataAtPlace :: Board -> Place -> PlaceData
dataAtPlace board place = board !! (fst place) !! (snd place)


{- Returns true if place is inside the board -}
placeIsValid :: Board -> Place -> Bool
placeIsValid [] _ = False
placeIsValid board place = if fst place >= length board || fst place < 0 || snd place >= length (board !! 0) || snd place < 0
    then False
    else True



{- TODO Add a PlaceData to the board for position Place -}
addStoneToBoard :: Board -> Place -> Side -> Board
addStoneToBoard board place side = do
    if(not (placeIsValid board place) || (dataAtPlace board place /= Empty))
        then board
        else setDataAtPlace board place (Stone side)

setDataAtPlace :: Board -> Place -> PlaceData -> Board
setDataAtPlace board place placeData = do
    if(placeIsValid board place)
        then map (\(rowIndex, row) -> map (\(columnIndex, dataa) -> if (rowIndex, columnIndex) == place then placeData else dataa) (zip [0..] row)) (zip [0..] board)
        else board
    
{- Turha ehkä -}
nextPlace :: Board -> Place -> Place
nextPlace board place = do
    if (snd place >= snd (getDimensions board) - 1)
        then (fst place + 1, 0)
        else (fst place, snd place + 1)
