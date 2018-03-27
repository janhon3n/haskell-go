module Player where
import Board

data PlayerType = Human Side deriving (Show)

class DecisionMaker a where
    choosePlace :: a -> Board -> IO Place

instance DecisionMaker PlayerType where
    choosePlace (Human side) board = do
        putStrLn $ show side ++ ", select place: "
        line <- getLine
        if length line < 2
            then choosePlace (Human side) board
            else do
                let place = (fromEnum (line !! 0), fromEnum (line !! 1))
                if placeIsValid board place
                    then return place
                    else choosePlace (Human side) board