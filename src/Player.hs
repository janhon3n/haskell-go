{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wall #-} 

module Player where
import Board
import Region
import GHC.Generics
   
data MoveType = StonePlacing | Passing | Finishing deriving (Eq, Show, Generic)
data Move = Move {
    moveType :: MoveType,
    place :: Place
} deriving (Eq, Show, Generic)

data PlayerType = Human | RandomAI | TreeAI deriving (Eq, Show, Generic)

data Player = Player { playerType :: PlayerType
    , playerSide :: Side
    , captured :: Int
    , hasPassed :: Bool
    , hasFinished :: Bool
    , finalScore :: Int
 } deriving (Eq, Show, Generic)


addCaptured :: Player -> Int -> Player
addCaptured (Player ptype side captured hasPassed hasFinished finalScore) amount = Player ptype side (captured + amount) hasPassed hasFinished finalScore

chooseMove :: Player -> Board -> Bool -> Move
chooseMove player@(Player playerType playerSide captured hasPassed hasFinished finalScore) board passAvailable = case playerType of
        Human -> Move Passing (0,0)
        RandomAI -> Move Passing (0,0)

        TreeAI -> do
            case chooseBestPlaceWithAlphaBetaSearch board playerSide 0 of
                Nothing -> Move Finishing (0,0)
                Just p -> Move StonePlacing p

data MinMaxPhase = Min | Max deriving (Show, Eq)
switchPhase :: MinMaxPhase -> MinMaxPhase
switchPhase Min = Max
switchPhase Max = Min
type Depth = Int
data BoardNode = BoardNode Board Side deriving (Show, Eq)

branchingFactor :: Int
branchingFactor = 2

chooseBestPlaceWithAlphaBetaSearch :: Board -> Side -> Depth -> Maybe Place
chooseBestPlaceWithAlphaBetaSearch board side depth = do
    case selectEvenly (getAvailablePlaces board side) branchingFactor of
        [] -> Nothing
        (place:tail) -> do
            let evalFunc = (\p -> alphaBetaEvaluate (BoardNode (fst (placeStone board p side)) side) depth (0,0) Min)
            Just $ selectBestPlace tail place (evalFunc place) evalFunc

selectBestPlace :: [Place] -> Place -> Double -> (Place -> Double) -> Place
selectBestPlace [] bestPlace currentBestValue evalFunc = bestPlace
selectBestPlace (placeToCheck:tail) bestPlace currentBestValue evalFunc =
        case evalFunc placeToCheck of
            value | value <= currentBestValue -> selectBestPlace tail bestPlace currentBestValue evalFunc
            value -> selectBestPlace tail placeToCheck value evalFunc

alphaBetaEvaluate :: BoardNode -> Depth -> (Double, Double) -> MinMaxPhase -> Double
alphaBetaEvaluate node@(BoardNode board side) depth (alpha, beta) minmax = case depth of
    d | d > 0 -> do
        let children = getChildNodes node branchingFactor
        if minmax == Max
            then maximum $ map (\n -> alphaBetaEvaluate n (depth-1) (alpha, beta) (switchPhase minmax)) children 
            else minimum $ map (\n -> alphaBetaEvaluate n (depth-1) (alpha, beta) (switchPhase minmax)) children
    
    d -> if minmax == Max
            then evaluateBoard board side
            else evaluateBoard board (opposite side)

getChildNodes :: BoardNode -> Int -> [BoardNode]
getChildNodes node@(BoardNode board side) count = do
    case getAvailablePlaces board side of
        [] -> []
        places -> map (\p -> BoardNode (fst (placeStone board p side)) (opposite side)) $ selectEvenly places count

evaluateBoard :: Board -> Side -> Double
evaluateBoard board side = do
    fromIntegral $ 25

pickEveryNth :: [a] -> Int -> [a]
pickEveryNth xs n = case drop (n-1) xs of
    (y:ys) -> y : pickEveryNth ys n
    [] -> []

selectEvenly :: [a] -> Int -> [a]
selectEvenly list count = do
    if (length list) <= count then list
        else pickEveryNth list (floor $ (fromIntegral (length list) :: Double) / (fromIntegral count :: Double))

