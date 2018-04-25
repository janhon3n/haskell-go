module Main where
import GoHttpServer
import Go
import Player

main :: IO ()
main = do
    let gs = initialState (3,3) (TreeAI, TreeAI)
    let gs' =  executeAITurn $ executeAITurn $ executeAITurn gs
    print gs'
    return ()