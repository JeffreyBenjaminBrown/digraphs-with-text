-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List

    main = runTestTT testList

    testList = TestList
      [   TestLabel "tSubInTplt" tSubInTplt
        , TestLabel "tInsert" tInsert
        , TestLabel "tInsert'" tInsert'
        , TestLabel "tRelvs"   tRelvs
        , TestLabel "tRelvs'"   tRelvs
      ]

-- "globals"
  -- orig
    g1,g1' :: Mindmap
    g1 = mkGraph
      [   (0, MmString "dog"       )
        , (1, Tplt 2 "_ wants _" )
        , (2, Tplt 2 "_ needs _" )
        , (3, MmString "water"     )
        , (4, MmString "brandy"    )
        , (5, Rel 2           )
        , (6, Rel 2           )
        , (7, Tplt 3 "_ needs _ for _")
        , (8, Rel 3           ) 
        , (9, Tplt 2 "statement _ is _")
        , (10, MmString "dubious"  )
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

  -- next gen
    g2,g2' :: Mindmap'
    g2 = mkGraph
      [   (0, MmString' "dog"       )
        , (1, stringToTplt "_ wants _" )
        , (2, stringToTplt "_ needs _" )
        , (3, MmString' "water"     )
        , (4, MmString' "brandy"    )
        , (5, Rel' 2           )
        , (6, Rel' 2           )
        , (7, stringToTplt "_ needs _ for _")
        , (8, Rel' 3           ) 
        , (9, stringToTplt "statement _ is _")
        , (10, MmString' "dubious"  )
        , (11, Rel' 2          )
      ] [ (5,1, AsTplt), (5,0, AsPos 1), (5,4,AsPos 2)
        , (6,2, AsTplt), (6,0, AsPos 1), (6,3,AsPos 2)
        , (8,7, AsTplt), (8,0, AsPos 1), (8,3,AsPos 2), (8,4,AsPos 3) 
        , (11,9,AsTplt), (11,5,AsPos 1), (11,10,AsPos 2)
      ]

    g2' =   insRel' 9 [5,10] 
          $ insStr' "dubious"     $ insTplt' "statement _ is _"
          $ insRel' 7 [0,3,4]    $ insTplt' "_ needs _ for _"
          $ insRel' 2 [0,3]      $ insRel' 1 [0,4]
          $ insStr' "brandy"     $ insStr' "water"
          $ insTplt' "_ needs _" $ insTplt' "_ wants _"
          $ insStr' "dog"        $ empty :: Mindmap'

-- tests
    tSubInTplt = let t1 = "_ needs _"
                     t2 = "Does _ need _?"
                     t3 = "_ needs _!"
                     (a,b) = ("skeletor","love")
      in TestCase $ do
        assertBool "1" $ subInTplt t1 [a,b] == "skeletor needs love"
        assertBool "2" $ subInTplt t2 [a,b] == "Does skeletor need love?"
        assertBool "3" $ subInTplt t3 [a,b] == "skeletor needs love!"

    tInsert = TestCase $ do
      assertBool "insRel & insStr" $ g1 == g1'

    tInsert' = TestCase $ do
      assertBool "insRel & insStr" $ g2 == g2'

    tRelvs = TestCase $ do
      assertBool "1--"  $ mmRelps g1 [Just 1, Nothing, Nothing] == [5]
      assertBool "-0-"  $ mmRelps g1 [Nothing, Just 0, Nothing] == [5,6]
      assertBool "--3"  $ mmRelps g1 [Nothing, Nothing, Just 4] == [5]
      assertBool "---4" $ mmRelps g1 [Nothing, Nothing, Nothing, Just 4] == [8]

    tRelvs' = TestCase $ do
      assertBool "1--"  $ mmRelps' g2 [Just 1,  Nothing, Nothing] == [5]
      assertBool "-0-"  $ mmRelps' g2 [Nothing, Just 0,  Nothing] == [5,6]
      assertBool "--3"  $ mmRelps' g2 [Nothing, Nothing, Just 4 ] == [5]
      assertBool "---4" $ mmRelps' g2 [Nothing, Nothing, Nothing, Just 4] == [8]
