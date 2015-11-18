-- setup
    import Test.HUnit
    import Dwt
    import qualified Data.List as List

    main = runTestTT testList

    testList = TestList
      [ -- TestLabel "tStmt" tStmt -- why was I doing that, if these work?
        tInsert
        , tInsert'
        , TestLabel "tRelvs" tRelvs
        , TestLabel "tRelvs'" tRelvs'
      ]

    mn = Nothing
    mj = Just

-- obsoleting
  -- globals
    g1,g1' :: Mindmap
    g1 = mkGraph [   (0, StrExpr "dog"   )
                   , (1, StrExpr "_ wants _" )
                   , (2, StrExpr "_ needs _" )
                   , (3, StrExpr "water" )
                   , (4, StrExpr "brandy")
                   , (5, RelExpr 3       )
                   , (6, RelExpr 3       ) ]
                [   (5,0,RelPosition 1), (5,1,RelPosition 2), (5,4,RelPosition 3)
                  , (6,0,RelPosition 1), (6,2,RelPosition 2), (6,3,RelPosition 3) ]

    g1' =   insRelExpr (0,2,3)      $ insRelExpr (0,1,4)
          $ insStrExpr "brandy"     $ insStrExpr "water"
          $ insStrExpr "_ needs _"  $ insStrExpr "_ wants _"
          $ insStrExpr "dog" $ empty :: Mindmap

  -- tests
    tInsert = TestCase $ do
      assertBool "insStrExpr" $ insStrExpr "nerp" (empty :: Mindmap)
        == mkGraph [(0, StrExpr "nerp")] []
      assertBool "insRelExpr & insStrExpr" $ g1 == g1'

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

-- replacing that with this
  -- globals
    g2,g2' :: Mindmap
    g2 = mkGraph
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

    g2' =   insRelExpr' 7 [0,3,4]
          $ insStrExpr "_ needs _ for _"
          $ insRelExpr' 2 [0,3]     $ insRelExpr' 1 [0,4]
          $ insStrExpr "brandy"     $ insStrExpr "water"
          $ insStrExpr "_ needs _"  $ insStrExpr "_ wants _"
          $ insStrExpr "dog" $ empty :: Mindmap

  -- tests
    tInsert' = TestCase $ do
      assertBool "insStrExpr" $ insStrExpr "nerp" (empty :: Mindmap)
        == mkGraph [(0, StrExpr "nerp")] []
      assertBool "insRelExpr' & insStrExpr" $ g2 == g2'

    tRelvs' = TestCase $ do
      assertBool "1--" $ mmRelvs' g2 [Just 1, Nothing, Nothing] == [5]
      assertBool "-0-" $ mmRelvs' g2 [Nothing, Just 0, Nothing] == [5,6]
      assertBool "--3" $ mmRelvs' g2 [Nothing, Nothing, Just 4] == [5]
      assertBool "---4" $ mmRelvs' g2 [Nothing, Nothing, Nothing, Just 4] == [8]
