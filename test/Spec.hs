import Test.HUnit
import Dwt
import qualified Data.List as List
                           
main = runTestTT testList

testList = TestList 
  [ -- TestLabel "tStmt" tStmt -- why was I doing that, if these work?
    tGraph
  ]

g1 = mkGraph [   (0, MmStr "dog"   )
               , (1, MmStr "wants" )
               , (2, MmStr "needs" )
               , (3, MmStr "water" )
               , (4, MmStr "brandy")
               , (5, MmTrip        ) ]
            [ (5,0,MmLab1), (5,1,MmLab2), (5,3,MmLab3) ]

g1' =   insMmTrip (0,1,3)
      $ insMmStr "brandy" $ insMmStr "water" 
      $ insMmStr "needs"  $ insMmStr "wants" 
      $ insMmStr "dog" mmEmpty

tGraph = TestCase $ do
  assertBool "insert node" $ insMmStr "nerp" mmEmpty 
    == mkGraph [(0,MmStr "nerp")] []
  assertBool "insert edge" (g1 == g1')
--  assertBool "query" (relvs g1 (Nothing, Just 1, Nothing) == 
