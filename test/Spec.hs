-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List

    main = runTestTT testList

    testList = TestList
      [ tInsert' -- the TestLabel syntax is useful only if you want names
        , TestLabel "tRelvs'" tRelvs'
      ]

    mn = Nothing
    mj = Just

-- globals
    g1,g1' :: Mindmap
    g1 = mkGraph
      [   (0, StrExpr "dog"       )
        , (1, StrExpr "_ wants _" )
        , (2, StrExpr "_ needs _" )
        , (3, StrExpr "water"     )
        , (4, StrExpr "brandy"    )
        , (5, RelExpr 2           )
        , (6, RelExpr 2           )
        , (7, StrExpr "_ needs _ for _")
        , (8, RelExpr 3           ) ]
      [   (5,1,RelTemplate), (5,0,RelPosition 1), (5,4,RelPosition 2)
        , (6,2,RelTemplate), (6,0,RelPosition 1), (6,3,RelPosition 2)
        , (8,7,RelTemplate), (8,0,RelPosition 1), (8,3,RelPosition 2), (8,4,RelPosition 3) ]

    g1' =   insRelExpr 7 [0,3,4]
          $ insStrExpr "_ needs _ for _"
          $ insRelExpr 2 [0,3]      $ insRelExpr 1 [0,4]
          $ insStrExpr "brandy"     $ insStrExpr "water"
          $ insStrExpr "_ needs _"  $ insStrExpr "_ wants _"
          $ insStrExpr "dog" $ empty :: Mindmap

-- tests
    tInsert' = TestCase $ do
      assertBool "insRelExpr & insStrExpr" $ g1 == g1'

    tRelvs' = TestCase $ do
      assertBool "1--"  $ mmRelps g1 [Just 1, Nothing, Nothing] == [5]
      assertBool "-0-"  $ mmRelps g1 [Nothing, Just 0, Nothing] == [5,6]
      assertBool "--3"  $ mmRelps g1 [Nothing, Nothing, Just 4] == [5]
      assertBool "---4" $ mmRelps g1 [Nothing, Nothing, Nothing, Just 4] == [8]
