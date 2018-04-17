module UI where
import Board

class Drawable a where
    draw :: a -> String

instance Drawable Side where
    draw Black = "B"
    draw White = "W"

instance Drawable PlaceData where
    draw Empty = "."
    draw (Stone p) = draw p


drawBoard :: Board -> IO ()
drawBoard board = do
    let characters = take (snd (getDimensions board)) ['A'..]
    putStrLn $ "   " ++ (concat $ map (\num -> ' ':num:[]) characters)
    mapM (\(row, index) -> drawRow row index) (zip board [1..])
    return ()


drawRow :: Row -> Int -> IO ()
drawRow row index = do
    let drawedRow = concat (map (\x -> " " ++ (draw x)) row)
    if index < 10 
        then putStrLn $ (show index) ++ "  " ++ drawedRow
        else putStrLn $ (show index) ++ " " ++ drawedRow


getPlace :: Side -> BoardDimensions -> IO Place
getPlace side dimensions = do
    putStr $ (show side) ++ ", select your place: "
    input <- getLine
    if (length input) < 2
        then getPlace side dimensions
        else do
            let col = fromEnum (head input) - 64
            let row = read (tail input)
            if row < 1 || row > fst dimensions || col < 1 || col > snd dimensions
                then getPlace side dimensions
                else return (row, col)