module Region where
import Board
import Data.List

data RegionType = Undefined | RegionType PlaceData deriving (Eq, Show)

type Region = [Place]

getUniformRegions :: Board -> [Region]
getUniformRegions board = getUniformRegions' board (Just (0,0)) []

getUniformRegions' :: Board -> Maybe Place -> [Region] -> [Region]
getUniformRegions' board Nothing foundRegions = foundRegions
getUniformRegions' board (Just place) foundRegions = 
    case (length (filter (\r -> elem place r) foundRegions)) of
            {- place is not in any found region -}
            0 -> getUniformRegions' board (nextPlace board place) ((getUniformRegion board place) : foundRegions)
            _ -> getUniformRegions' board (nextPlace board place) foundRegions

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
getUniformRegion board place = findUniformRegion board [] [place] (dataAtPlace board place)

findUniformRegion :: Board -> Region -> [Place] -> PlaceData -> Region
findUniformRegion board foundRegion [] placeData = foundRegion
findUniformRegion board foundRegion (placeToCheck:places) placeData =
        case dataAtPlace board placeToCheck == placeData of
            True -> do
                let newPlacesToCheck = filter (\p -> not $ p `elem` foundRegion || p `elem` places) $ getAdjacentPlaces board placeToCheck
                findUniformRegion board (placeToCheck:foundRegion) (places ++ newPlacesToCheck) placeData
            False -> findUniformRegion board foundRegion places placeData

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

getLiberties :: Board -> Region -> Region
getLiberties board region = filter (\p -> dataAtPlace board p == Empty) $
        foldl (\r p -> union r (getAdjacentPlaces board p)) [] region

isDead :: Board -> Region -> Bool
isDead board region = length (getLiberties board region) == 0

getScore :: Board -> Side -> Int
getScore board side = sum $ map length controlledPlaces
    where controlledPlaces = filter (\r ->
            getRegionType board r == RegionType (Empty) &&
            getBorderType board r == RegionType (Stone side))
                $ getUniformRegions board
