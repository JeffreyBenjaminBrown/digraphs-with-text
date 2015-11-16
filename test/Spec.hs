import Test.HUnit
import Dwt
import qualified Data.List as List
                           
main = runTestTT testList

testList = TestList 
  [ -- TestLabel "tStmt" tStmt -- why was I doing that, if these work?
    tInsert
    , tRelvs
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

tInsert = TestCase $ do
  assertBool "insMmStr" $ insMmStr "nerp" mmEmpty 
    == mkGraph [(0,MmStr "nerp")] []
  assertBool "insMmTrip & insMmStr" $ g1 == g1'

tRelvs = TestCase $ do
  assertBool "mmRelvs 0-- some" $ mmRelvs g1 (Just 0, Nothing, Nothing) == [5,6]
    -- TODO: Order matters; that could have been [6,5]. Use Set instead.
  assertBool "mmRelvs 1-- none" $ mmRelvs g1 (Just 1, Nothing, Nothing) == []
  assertBool "mmRelvs -1- some" $ mmRelvs g1 (Nothing, Just 1, Nothing) == [5]
  assertBool "mmRelvs -0- none" $ mmRelvs g1 (Nothing, Just 0, Nothing) == []
  assertBool "mmRelvs --4 some" $ mmRelvs g1 (Nothing, Nothing, Just 4) == [5]
  assertBool "mmRelvs --0 none" $ mmRelvs g1 (Nothing, Nothing, Just 0) == []
  assertBool "mmRelvs 01- some" $ mmRelvs g1 (Just 0, Just 1, Nothing) == [5]
  assertBool "mmRelvs 02- some" $ mmRelvs g1 (Just 0, Just 2, Nothing) == [6]
  assertBool "mmRelvs 03- none" $ mmRelvs g1 (Just 0, Just 3, Nothing) == []
  assertBool "mmRelvs -14 some" $ mmRelvs g1 (Nothing, Just 1, Just 4) == [5]
  assertBool "mmRelvs -20 none" $ mmRelvs g1 (Nothing, Just 2, Just 0) == []
  assertBool "mmRelvs 023 some" $ mmRelvs g1 (Just 0, Just 2, Just 3) == [6]
  assertBool "mmRelvs 024 none" $ mmRelvs g1 (Just 0, Just 2, Just 4) == []
