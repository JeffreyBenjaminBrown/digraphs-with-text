-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List
    import Data.Maybe (fromJust)

    import Control.Monad.Except -- from mtl library

    main = runTestTT testList

    testList = TestList
      [   TestLabel "tSubInTplt" tSubInTplt
        , TestLabel "tInsert" tInsert
        , TestLabel "tMatchRel"  tMatchRel
        , TestLabel "tShowExpr" tShowExpr
        , TestLabel "tGelemM" tGelemM
        , TestLabel "tInsRelM" tInsRelM
      ]

-- "globals"
    g1,g1' :: Mindmap
    g1 = mkGraph
      [   (0, Str "dog"       )
        , (1, stringToTplt "_ wants _" )
        , (2, stringToTplt "_ needs _" )
        , (3, Str "water"     )
        , (4, Str "brandy"    )
        , (5, Rel 2           )
        , (6, Rel 2           )
        , (7, stringToTplt "_ needs _ for _")
        , (8, Rel 3           ) 
        , (9, stringToTplt "statement _ is _")
        , (10, Str "dubious"  )
        , (11, Rel 2          )
      ] [ (5,1, AsTplt), (5,0, AsPos 1), (5,4,AsPos 2)
        , (6,2, AsTplt), (6,0, AsPos 1), (6,3,AsPos 2)
        , (8,7, AsTplt), (8,0, AsPos 1), (8,3,AsPos 2), (8,4,AsPos 3) 
        , (11,9,AsTplt), (11,5,AsPos 1), (11,10,AsPos 2)
      ]

    g1' =   insRel 9 [5,10] 
          $ insStr "dubious"    $ insTplt "statement _ is _"
          $ insRel 7 [0,3,4]    $ insTplt "_ needs _ for _"
          $ insRel 2 [0,3]      $ insRel 1 [0,4]
          $ insStr "brandy"     $ insStr "water"
          $ insTplt "_ needs _" $ insTplt "_ wants _"
          $ insStr "dog"        $ empty :: Mindmap

-- tests
    tSubInTplt = TestCase $ do
      assertBool "1" $ subInTplt (fromJust $ lab g1 1) ["man","peace"]
        == "man wants peace"

    tInsert = TestCase $ do
      assertBool "insRel & insStr" $ g1 == g1'

    tMatchRel = TestCase $ do
      assertBool "1--"  $ matchRel g1 [Just 1,  Nothing, Nothing] == [5]
      assertBool "-0-"  $ matchRel g1 [Nothing, Just 0,  Nothing] == [5,6]
      assertBool "--3"  $ matchRel g1 [Nothing, Nothing, Just 4 ] == [5]
      assertBool "---4" $ matchRel g1 [Nothing, Nothing, Nothing, Just 4] == [8]

    tShowExpr = TestCase $ do
      assertBool "expr 5" $ showExpr g1 5 == "5:1 [0: dog] wants [4: brandy]"
      assertBool "expr 11" $ showExpr g1 11 == 
        "11:9 statement [5:1 [0: dog] wants [4: brandy]] is [10: dubious]"

    tGelemM = TestCase $ do
      assertBool "1" $ gelemM g1 0 == Right ()

    tInsRelM = TestCase $ do
      assertBool "1" $ (insRelM 2 [0,0] g1 :: Either String Mindmap)
            == (Right $ insRel  2 [0,0] g1)
      assertBool "2" $ (insRelM 15 [0,0] g1 :: Either String Mindmap)
            == Left "Node not in Mindmap"
      assertBool "3" $ (insRelM 2 [100,0] g1 :: Either String Mindmap)
            == Left "Node not in Mindmap"
