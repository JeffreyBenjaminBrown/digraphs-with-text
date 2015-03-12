{- how to run
    :l Test
    runTestTT testList
-}

-- imports
    import MyGraph
    import TotalDescendents
    import Test.HUnit

    import qualified Data.Set as S
    import qualified Data.Map as M
    import qualified Data.Maybe as Mb

-- list of tests
    testList = TestList 
      [ TestLabel "tAddPrd" tAddPrd
      , TestLabel "tAddPrdLab" tAddPrdLab
      , TestLabel "tRmPrd" tRmPrd
      , TestLabel "tRmPrdLab" tRmPrdLab
      , TestLabel "tGraph" tGraph
      , TestLabel "tRelsIfNodeExists" tRelsIfNodeExists
      , TestLabel "tPrdsNotYetCleared" tPrdsNotYetCleared
      , TestLabel "tAddToPrdMap" tAddToPrdMap
      , TestLabel "tVisitPrds" tVisitPrds
      , TestLabel "tTotalDescendents" tTotalDescendents
      ]

-- Gnode
    tAddPrd = TestCase $ do
      let gn1 = blank 1
          gn2 = blank 2
          gn1' = addPrd gn2 gn1
          gn1'' = addPrd gn2 gn1'
      assertBool "gn1 has nothing gn1' does not" 
        (diff gn1 gn1' == gn1) 
      assertBool "gn1' has one more prd than gn1" 
        (diff gn1' gn1 == gn1 { prds = S.fromList [2] } )
      assertBool "idempotence" (gn1' == gn1'')

    tAddPrdLab = TestCase $ do
      let gn1 = blank 1
          gn2 = blank 2
          gn1' = addPrdLab (glab gn2) gn1
          gn1'' = addPrdLab (glab gn2) gn1'
      assertBool "gn1 has nothing gn1' does not" 
        (diff gn1 gn1' == gn1) 
      assertBool "gn1' has one more prd than gn1" 
        (diff gn1' gn1 == gn1 { prds = S.fromList [2] } )
      assertBool "idempotence" (gn1' == gn1'')

    tRmPrd = TestCase $ do
      let gn1 = blank 1
          gn2 = addPrd gn1 $ blank 2
          gn2' =  rmPrd gn1 gn2
          gn2'' = rmPrd gn1 gn2'
      assertBool "gn2 has a prd that gn2' does not" 
        (diff gn2 gn2' == gn2)
      assertBool "gn2' has nothing that gn2 does not" 
        (isInNoEdges $ diff gn2' gn2)
      assertBool "idempotence" (gn2' == gn2'')

    tRmPrdLab = TestCase $ do
      let gn1 = blank 1
          gn2 = addPrd gn1 $ blank 2
          gn2' =  rmPrdLab (glab gn1) gn2
          gn2'' = rmPrdLab (glab gn1) gn2'
      assertBool "gn2 has a prd that gn2' does not" 
        (diff gn2 gn2' == gn2)
      assertBool "gn2' has nothing that gn2 does not" 
        (isInNoEdges $ diff gn2' gn2)
      assertBool "idempotence" (gn2' == gn2'')

-- Graph
    tGraph = TestCase $ do
      -- this also tests grDiff, modUsingLabels, addEdge, rmEdge
      let gr = blankGraphFromList [1,2,3]
          gr' = modUsingLabels addEdge 1 2 gr
          gr'' = modUsingLabels addEdge 1 2 gr
          hr = modUsingLabels rmEdge 1 2 gr
      assertBool "lost nothing"
        (grDiff gr gr' == M.empty)
      assertBool "gained something"
        (grDiff gr' gr == M.delete 3 gr' && M.delete 3 gr' /= M.empty)
      assertBool "idempotence" (gr' == gr'')
      assertBool "undo" (hr == gr)

-- Test graphs
    sharkDiamond :: Graph
       -- 1--->--->-3-->-5
       --  \        v
       --   \->-2->-4
    sharkDiamond = gr'
      where gr  = blankGraphFromList [1,2,3,4,5]
            gr' = modUsingLabels addEdge 1 2 $
                  modUsingLabels addEdge 1 3 $
                  modUsingLabels addEdge 2 4 $
                  modUsingLabels addEdge 3 4 $
                  modUsingLabels addEdge 3 5 gr

    chain5 :: Graph
    chain5 = gr'
      where gr = blankGraphFromList [1,2,3,4,5]
            gr' = modUsingLabels addEdge 1 2 $
                  modUsingLabels addEdge 2 3 $
                  modUsingLabels addEdge 3 4 $
                  modUsingLabels addEdge 4 5 gr

-- TotalDescendents
    tRelsIfNodeExists = TestCase $ do
      let g = sharkDiamond
          p4 = relsIfNodeExists prds (4 :: Glab) g
          s3 = relsIfNodeExists sucs (3 :: Glab) g
          emp = relsIfNodeExists prds (7 :: Glab) g
      assertBool "4's parents = {2,3}" (p4 == S.fromList [2,3])
      assertBool "3's children = {4,5}" (p4 == S.fromList [2,3])
      assertBool "7 not found" (emp == S.empty)

    tPrdsNotYetCleared = TestCase $ do
      let g = sharkDiamond
      assertBool "4's parents less [3] = [2]"
        ( prdsNotYetCleared 4 (S.fromList [3,5]) g == S.fromList [2] )

    tAddToPrdMap = TestCase $ do
      let g = sharkDiamond
      assertBool "parents of 4 and 5 are 2 and 3."
        ( addToPredMap (S.fromList [4,5]) M.empty g 
          == M.fromList [(4, S.fromList [2,3]),
                         (5, S.fromList [3])] )

    tVisitSucs = TestCase $ do
      let tdSnv = S.fromList [3 :: Glab]
          tdSv = S.empty
          undet = M.empty
          ds = (tdSnv, tdSv, undet, sharkDiamond)
          (tdSnv', tdSv', undet', g') = visitTdSnvSucs ds
      assertBool "graph unchanged" (sharkDiamond == g')
      assertBool "tdSnv empty" (tdSnv' == S.empty)
      assertBool "new tdSv = old tdSnv" (tdSv' == tdSnv)
      assertBool "undet is right" (undet' == M.fromList [
          (4, S.fromList [2,3]),
          (5, S.fromList [3  ])]  )

    tVisitPrds = TestCase $ do
      let tdSnv = S.empty
          tdSv = S.fromList [3 :: Glab]
          undet  = M.fromList [ (4, S.fromList [2,3])
                           , (5, S.fromList [3  ])  
                           ]
          ds = (tdSnv, tdSv, undet, sharkDiamond)
          (tdSnv', tdSv', undet', g') = visitUndetPrds ds
      assertBool "graph unchanged" (sharkDiamond == g')
      assertBool "tdSv unchanged" (tdSv == tdSv')
      assertBool "4 still undet, but less its parent 3" 
        (undet' == M.fromList [ (4, S.fromList [2] ) ] )
      assertBool "5 identified as pure" (tdSnv' == S.fromList [5])

    tTotalDescendents = TestCase $ do
      assertBool "3's totalDescendents are only itself and 5"
        (totalDescendents (S.fromList [3]) sharkDiamond == S.fromList [3,5])
      assertBool "{2,3}'s totalDescendents are themselves and {4,5}"
        (totalDescendents (S.fromList [2,3]) sharkDiamond == S.fromList [2..5])

-- REMINDERS!
  -- When adding functions, remember to list them in "testList" at top.
  -- Each test aborts at its first failure.

-- EOF

