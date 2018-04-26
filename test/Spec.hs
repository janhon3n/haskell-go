import Test.HUnit
import GameState
import Board
import Move
import Region

removeCapturedTest = TestCase (assertEqual "Captured stones removed"
   (do
      let b2 = [[Empty, Stone Black, Stone Black, Empty],
            [Stone Black, Empty, Empty, Stone Black],
            [Empty, Stone Black, Stone Black, Empty],
            [Stone White, Stone White, Empty, Stone White]]
      (b2, 2))
      (do
         let b = [[Empty, Stone Black, Stone Black, Empty],
               [Stone Black, Stone White, Stone White, Stone Black],
               [Empty, Stone Black, Stone Black, Empty],
               [Stone White, Stone White, Empty, Stone White]]
         removeCaptured b (1,0) Black)
   )

stateTest1 = TestCase (assertEqual "GameState updated correctly" 
      [[Empty,Stone {side = Black},Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Stone {side = Black},Stone {side = Black},Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Stone {side = White},Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
      (do
         let places = [(1,1), (5,3), (0,1), (0,0), (1,0)]
         board $ foldl (\s p -> executeMove s (Move StonePlacing p)) (initialState (9,9)) places)
   )

tests = TestList [TestLabel "stateTest1" stateTest1,
            TestLabel "removeCapturedTest" removeCapturedTest]

main :: IO ()
main = do
   runTestTT tests
   return ()
   