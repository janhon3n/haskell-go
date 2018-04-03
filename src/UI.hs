module UI where
import Board

class Drawable a where
    draw :: a -> String

instance Drawable Side where
    draw Black = "X"
    draw White = "O"

instance Drawable PlaceData where
    draw Empty = "."
    draw (Piece p) = draw p

drawBoard :: Board -> IO ()
drawBoard board = do
    let numbers = take (snd (getDimensions board)) ['A'..]
    putStrLn $ "  " ++ numbers
    mapM (\x -> drawRow x) board
    return ()

drawRow :: Row -> IO ()
drawRow row = putStrLn $ "  " ++ concat (map draw row)