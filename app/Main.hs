module Main where
import GoHttpServer

main :: IO ()
main = do
    putStrLn "Launching haskell-go..."
    startServer
    return ()