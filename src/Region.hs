module Region where
import Board
import Data.List

data RegionType = Undefined | RegionType PlaceData deriving (Eq, Show)

type Region = [Place]

getRegions :: Board -> [Region]
getRegions board = getRegions' board (0,0) []

getRegions' :: Board -> Place -> [Region] -> [Region]
getRegions' board place foundRegions = do
    if placeIsValid board place /= True
        then foundRegions
        else if length (filter (\r -> elem place r) foundRegions) == 0
            then getRegions' board (nextPlace board place) ((getRegion board place) : foundRegions)
            else getRegions' board (nextPlace board place) foundRegions

placeIsInRegion :: Region -> Place -> Bool
placeIsInRegion region place = elem place region
 
getRegionType :: Board -> Region -> RegionType
getRegionType board [] = Undefined
getRegionType board region = do
    let dataTypes = nub $ map (dataAtPlace board) region
    if length dataTypes > 1
        then Undefined
        else RegionType (dataTypes !! 0)

getRegion :: Board -> Place -> Region
getRegion board place = expandRegion board [] place

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

getBorderRegion :: Board -> Region -> Region
getBorderRegion board region = getBorderRegion' board region region

getBorderRegion' :: Board -> Region -> Region -> Region
getBorderRegion' board region [] = []
getBorderRegion' board region regionToCheck@(place:restOfRegion) = do
    let borderPlaces = filter (\p -> placeIsValid board p && placeIsInRegion region p /= True) (getAdjacentPlaces board place)
    union borderPlaces (getBorderRegion' board region restOfRegion)
