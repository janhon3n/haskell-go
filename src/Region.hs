module Region where
import Board
import Data.List

data RegionType = Undefined | RegionType PlaceData deriving (Eq, Show)

type Region = [Place]

getUniformRegions :: Board -> [Region]
getUniformRegions board = getUniformRegions' board (0,0) []

getUniformRegions' :: Board -> Region -> [Place] -> PlaceData -> Region
getUniformRegions' board foundRegion [] placeData = foundRegion
getUniformRegions' board foundRegion (placeToCheck:places) placeData =
        if dataAtPlace board placeToCheck == placeData
            then getUniformRegions' board (placeToCheck:foundRegion) (places ++ (getAdjacentPlaces board placeToCheck)) placeData
            else getUniformRegions' board foundRegion places placeData

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
getUniformRegion board place = expandRegion board [] place

expandRegion :: Board -> Region -> Place -> Region
expandRegion board [] place = do
    let region = [place]
    foldl (expandRegion board) region (getAdjacentPlaces board place)

expandRegion board region place = do
    if (RegionType (dataAtPlace board place) == (getRegionType board region) && placeIsInRegion region place /= True)
        then do
            let updatedRegion = region ++ [place]
            foldl (expandRegion board) updatedRegion (getAdjacentPlaces board place)
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

fillRegion :: Board -> Region -> PlaceData -> Board
fillRegion board region dat = foldl (\b p -> setDataAtPlace b p dat) board region