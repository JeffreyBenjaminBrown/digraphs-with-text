-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List

    main = runTestTT testList

    testList = TestList
      [   TestLabel "tInsert" tInsert
        , TestLabel "tRelvs"   tRelvs
      ]

-- "globals"
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

-- tests
  -- next gen
    tInsert = TestCase $ do
      assertBool "insRel & insStr" $ g1 == g1'

    tRelvs = TestCase $ do
      assertBool "1--"  $ mmRelps g1 [Just 1, Nothing, Nothing] == [5]
      assertBool "-0-"  $ mmRelps g1 [Nothing, Just 0, Nothing] == [5,6]
      assertBool "--3"  $ mmRelps g1 [Nothing, Nothing, Just 4] == [5]
      assertBool "---4" $ mmRelps g1 [Nothing, Nothing, Nothing, Just 4] == [8]
