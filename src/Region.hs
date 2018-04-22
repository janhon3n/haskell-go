module Region where
import Board
import Data.List

data RegionType = Undefined | RegionType PlaceData deriving (Eq, Show)

type Region = [Place]

getUniformRegions :: Board -> [Region]
getUniformRegions board = getUniformRegions' board (0,0) []

getUniformRegions' :: Board -> Place -> [Region] -> [Region]
getUniformRegions' board place foundRegions = do
    if placeIsValid board place /= True
        then foundRegions
        else if length (filter (\r -> elem place r) foundRegions) == 0
            then getUniformRegions' board (nextPlace board place) ((getUniformRegion board place) : foundRegions)
            else getUniformRegions' board (nextPlace board place) foundRegions

getUniformEmptyRegions :: Board -> [Region]
getUniformEmptyRegions board = filter (\r -> getRegionType board r == RegionType Empty) $ getUniformRegions board

placeIsInRegion :: Region -> Place -> Bool
placeIsInRegion region place = elem place region

getRegionType :: Board -> Region -> RegionType
getRegionType board [] = Undefined
getRegionType board region = do
    let dataTypes = nub $ map (dataAtPlace board) region
    if length dataTypes > 1
        then Undefined
        else RegionType (dataTypes !! 0)

getUniformRegion :: Board -> Place -> Region
getUniformRegion board place = findUniformRegion board [] place

findUniformRegion :: Board -> Region -> Place -> Region
findUniformRegion board [] place = do
    let region = [place]
    foldl (findUniformRegion board) region (getAdjacentPlaces board place)

findUniformRegion board region place = do
    if (RegionType (dataAtPlace board place) == (getRegionType board region) && placeIsInRegion region place /= True)
        then do
            let updatedRegion = region ++ [place]
            foldl (findUniformRegion board) updatedRegion (getAdjacentPlaces board place)
        else region

getBorderType :: Board -> Region -> RegionType
getBorderType board region = getRegionType board $ getBorderRegion board region

getBorderRegion :: Board -> Region -> Region
getBorderRegion board region = getBorderRegion' board region region

getBorderRegion' :: Board -> Region -> Region -> Region
getBorderRegion' board region [] = []
getBorderRegion' board region regionToCheck@(place:restOfRegion) = do
    let borderPlaces = filter (\p -> placeIsValid board p && placeIsInRegion region p /= True) (getAdjacentPlaces board place)
    union borderPlaces (getBorderRegion' board region restOfRegion)

setRegionContent :: Board -> Region -> PlaceData -> Board
setRegionContent board region dat = foldl (\b p -> setDataAtPlace b p dat) board region

{- Adds adjacent regions of type (PlaceData) to the region -}
expandRegion :: Board -> Region -> PlaceData -> Region
expandRegion board region placeData = do
    let borderRegion = getBorderRegion board region
    let newExpanse = foldl (\reg place -> if dataAtPlace board place == placeData
            then union reg (getUniformRegion board place)
            else reg)
                [] borderRegion
    union region newExpanse

getLiberties :: Board -> Region -> Region
getLiberties board region = filter (\p -> dataAtPlace board p == Empty) $
        foldl (\r p -> union r (getAdjacentPlaces board p)) [] region

isDead :: Board -> Region -> Bool
isDead board region = length (getLiberties board region) == 0

getScore :: Board -> Side -> Int
getScore board side = do
    let controlledPlaces = filter (\r ->
            getRegionType board r == RegionType (Empty) &&
            getBorderType board r == RegionType (Stone side))
                $ getUniformRegions board
    sum $ map length controlledPlaces