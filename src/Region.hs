module Region where
import Board

type RegionType = PlaceData
data BorderType = Undefined | BorderType Side deriving (Eq, Show)

data Region = Region RegionType [Place]

findRegionsOfType :: Board -> RegionType -> [Region]
findRegionsOfType board regionType = do

findRegionsOfType' :: Board -> RegionType -> [Region] -> Place -> [Region]
findRegionsOfType' board regionType createdRegions placeToCheck = do
    if (placeIsValid board placeToCheck /= True) then createdRegions
        else do
            let newRegions = addPlaceToRegions createdRegions regionType placeToCheck
            findRegionsOfType' board regionType newRegions (nextPlace placeToCheck)

addPlaceToRegions :: Board -> RegionType -> [Region] -> Place -> [Region]
addPlaceToRegions board regionType regions place = do
    let data = dataAtPlace board place
    if (data /= regionType)
        then regions
        else do
            if (dataAtPlace board (leftOf place) == regionType)
                then 
                else if (dataAtPlace board (leftOf place) == regionType)
                    then 
                    else regions ++ (Region regionType [place])
