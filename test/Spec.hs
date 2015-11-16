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
               , (5, MmTrip        )
               , (6, MmTrip        ) ]
            [   (5,0,MmLab1), (5,1,MmLab2), (5,4,MmLab3)
              , (6,0,MmLab1), (6,2,MmLab2), (6,3,MmLab3) ]

g1' =   insMmTrip (0,2,3) $ insMmTrip (0,1,4)
      $ insMmStr "brandy" $ insMmStr "water" 
      $ insMmStr "needs"  $ insMmStr "wants" 
      $ insMmStr "dog" mmEmpty
-- would "foldInsMmTrip" not be good? reduce these to two exprs:
  -- one command & list of strings
  -- one command & list of trips

tGraph = TestCase $ do
  assertBool "insMmStr" $ insMmStr "nerp" mmEmpty 
    == mkGraph [(0,MmStr "nerp")] []
  assertBool "insMmTrip & insMmStr" $ g1 == g1'
  assertBool "mmRelvs" $ mmRelvs g1 (Nothing, Just 1, Nothing) == [5]

