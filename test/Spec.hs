-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List
    import Data.Maybe (fromJust)
    import Data.Either

    import Control.Monad.Except -- from mtl library

    main = runTestTT testList

    testList = TestList
      [   TestLabel "tSubInTplt" tSubInTplt
        , TestLabel "tInsert" tInsert
        , TestLabel "tMatchRel"  tMatchRel
        , TestLabel "tShowExpr" tShowExpr
        , TestLabel "tGelemM" tGelemM
        , TestLabel "tInsRelM" tInsRelM
        , TestLabel "tTpltAt" tTpltAt
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

    g1' =   insRelUsf 9 [5,10] 
          $ insStr "dubious"    $ insTplt "statement _ is _"
          $ insRelUsf 7 [0,3,4]    $ insTplt "_ needs _ for _"
          $ insRelUsf 2 [0,3]      $ insRelUsf 1 [0,4]
          $ insStr "brandy"     $ insStr "water"
          $ insTplt "_ needs _" $ insTplt "_ wants _"
          $ insStr "dog"        $ empty :: Mindmap

-- tests
    tSubInTplt = TestCase $ do
      assertBool "1" $ subInTplt (fromJust $ lab g1 1) ["man","peace"]
        == "man wants peace"

    tInsert = TestCase $ do
      assertBool "insRelUsf & insStr" $ g1 == g1'

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
      assertBool "1" $ gelemM g1 100 == Left "gelemM: Node not in Mindmap"

    tInsRelM = TestCase $ do
      assertBool "1" $ (insRel 2 [0,0] g1 :: Either String Mindmap)
            == (Right $ insRelUsf  2 [0,0] g1)
      assertBool "2" $ (insRel 15 [0,0] g1 :: Either String Mindmap)
            == Left "gelemM: Node not in Mindmap"
      assertBool "3" $ (insRel 2 [100,0] g1 :: Either String Mindmap)
            == Left "gelemM: Node not in Mindmap"
      assertBool "4" $ (insRel 2 [1,1,1] g1 :: Either String Mindmap)
            == Left "nodesMatchTplt: Tplt Arity /= number of member Nodes"
      assertBool "5" $ (insRel 0 [1,1,1] g1 :: Either String Mindmap)
            == Left "tpltAt: Node does not index a Tplt"

    tTpltAt = TestCase $ do
      assertBool "j1" $ tpltArity (Tplt 3 []) == Right 3
      assertBool "j2" $ isLeft $ tpltArity (Str "nog")
      assertBool "j3" $ tpltArity (Str "rig") == Left "tpltArity: Expr not a Tplt"
