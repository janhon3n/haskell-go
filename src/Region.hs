module Region where
import Board

type RegionType = Maybe PlaceData
data BorderType = Undefined | BorderType Side deriving (Eq, Show)

type Region = [Place]

placeIsInRegion :: Region -> Place -> Bool
placeIsInRegion region place = elem place region
 
createRegion :: Board -> Place -> Region
createRegion board place = do


getRegionType :: Board -> Region -> RegionType
getRegionType board [] = Nothing
getRegionType board region = Just (dataAtPlace board (region !! 0))

expandRegion :: Board -> Region -> Place -> Region
expandRegion board [] place = do
    let region = [place]
    foldl (expandRegion board) region (getAdjacentPlaces board place)

expandRegion board region place = do
    if (Just (dataAtPlace board place) == (getRegionType board region) && placeIsInRegion region place /= True)
        then do
            let updatedRegion = region ++ [place]
            foldl (expandRegion board) updatedRegion (getAdjacentPlaces board place)
        else region
