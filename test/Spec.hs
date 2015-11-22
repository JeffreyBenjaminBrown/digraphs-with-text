-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List

    main = runTestTT testList

    testList = TestList
      [ tInsert -- TestLabel syntax: needed only if you need names
        , TestLabel "tRelvs" tRelvs
      ]

-- globals
    g1,g1' :: Mindmap
    g1 = mkGraph
      [   (0, MmString "dog"       )
        , (1, MmString "_ wants _" )
        , (2, MmString "_ needs _" )
        , (3, MmString "water"     )
        , (4, MmString "brandy"    )
        , (5, Rel 2           )
        , (6, Rel 2           )
        , (7, MmString "_ needs _ for _")
        , (8, Rel 3           ) 
        , (9, MmString "statement _ is _")
        , (10, MmString "dubious"  )
        , (11, Rel 2          )
      ] [ (5,1, RelTplt), (5,0, RelPos 1), (5,4,RelPos 2)
        , (6,2, RelTplt), (6,0, RelPos 1), (6,3,RelPos 2)
        , (8,7, RelTplt), (8,0, RelPos 1), (8,3,RelPos 2), (8,4,RelPos 3) 
        , (11,9,RelTplt), (11,5,RelPos 1), (11,10,RelPos 2)
      ]

    g1' = insRel 9 [5,10] 
          $ insStr "dubious"    $ insStr "statement _ is _"
          $ insRel 7 [0,3,4]    $ insStr "_ needs _ for _"
          $ insRel 2 [0,3]      $ insRel 1 [0,4]
          $ insStr "brandy"     $ insStr "water"
          $ insStr "_ needs _"  $ insStr "_ wants _"
          $ insStr "dog" $ empty :: Mindmap

-- tests
    tInsert = TestCase $ do
      assertBool "insRel & insStr" $ g1 == g1'

    tRelvs = TestCase $ do
      assertBool "1--"  $ mmRelps g1 [Just 1, Nothing, Nothing] == [5]
      assertBool "-0-"  $ mmRelps g1 [Nothing, Just 0, Nothing] == [5,6]
      assertBool "--3"  $ mmRelps g1 [Nothing, Nothing, Just 4] == [5]
      assertBool "---4" $ mmRelps g1 [Nothing, Nothing, Nothing, Just 4] == [8]
