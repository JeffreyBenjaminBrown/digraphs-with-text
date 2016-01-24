    module TGraph where

    import Dwt
    import TData
    import Test.HUnit

    import qualified Data.Map as Map
    import qualified Data.Set as Set
    import Data.Maybe (fromJust)
    import Data.Either
    import qualified Control.Lens.Lens as L -- not crit; for (&), only used once

    tGraph = TestList [ TestLabel "tBuildGraph" tBuildGraph
                      , TestLabel "tAskMinor"   tAskMinor
                      , TestLabel "tAskNodes"   tAskNodes
                      ]

  -- buildGraph
    tBuildGraph = TestList [ TestLabel "tSubInTplt" tSubInTplt
                           , TestLabel "tInsert" tInsert
                           , TestLabel "tInsRelM" tInsRelM
                           , TestLabel "tInsColl" tInsColl
                           , TestLabel "tPartitionRelSpec" tPartitionRelSpec
                           , TestLabel "tInsRelSpec" tInsRelSpec
                           , TestLabel "tChNonRelAt" tChNonRelAt
                           , TestLabel "tChMbr" tChMbr]

    tSubInTplt = TestCase $ do
      assertBool "1" $ subInTplt (fromJust $ lab g1 1) ["man","peace"]
        == "man wants peace"
      assertBool "2"
        $ (lab g1 1 L.& fromJust L.& subInTplt $ ["man","peace"])
        == "man wants peace"

    tInsert = TestCase $ do
      assertBool "stringToTplt (and thereby splitStringForTplt), insRelUsf, insStr, insTplt" $ g1 == g1Alt

    tInsRelM = TestCase $ do
      assertBool "1" $ (insRel 2 [0,0] g1 :: Either String Mindmap)
            == (Right $ insRelUsf  2 [0,0] g1)
      assertBool "2" $ (insRel 15 [0,0] g1 :: Either String Mindmap)
            == Left "gelemM: Node 15 absent."
      assertBool "3" $ (insRel 2 [100,0] g1 :: Either String Mindmap)
            == Left "gelemM: Node 100 absent."
      assertBool "4" $ (insRel 2 [1,1,1] g1 :: Either String Mindmap)
            == Left "nodesMatchTplt: Tplt Arity /= number of member Nodes."
      assertBool "5" $ (insRel 0 [1,1,1] g1 :: Either String Mindmap)
            == Left "tpltAt: LNode 0 not a Tplt."

    tInsColl = TestCase $ do
      let gg = fromRight $ insColl (j 10) n [0,3,4] g1
          collMbrEdge = CollEdge CollMbr
      assertBool "new 12th node" 
        $ (lab' $ fromJust $ fst $ match 12 gg) == Coll
      assertBool "3 new edges" 
        $ lsuc gg 12 == [(0, collMbrEdge), (3, collMbrEdge), (4, collMbrEdge)
                        ,(10, CollEdge CollName)]
      assertBool "only 1 new node, only 4 new edges"
        $    (length $ nodes g1) + 1 == (length $ nodes gg)
          && (length $ edges g1) + 4 == (length $ edges gg)

    tPartitionRelSpec = TestCase $ do
      let (vs,ns) = partitionRelSpec relSpec
      assertBool "1" $ Set.fromList (Map.toList vs)
        == Set.fromList [ (RelTplt, It)
                        , (Mbr 2,   Any) ]
      assertBool "2" $ Set.fromList (Map.toList ns)
        == Set.fromList [ (Mbr 1, 0) ]

    tInsRelSpec = TestCase $ do
      let (vs,ns) = partitionRelSpec relSpec
          Right g2 = insRelSpec relSpec g1
      assertBool "node" $ (fromJust $ lab g2 12) == RelSpecExpr vs
      assertBool "only 1 more edge" $ 
        (length $ edges g1) + 1 == (length $ edges g2)
      assertBool "the edge" $ hasLEdge g2 (12, 0, RelEdge $ Mbr 1)

    tChNonRelAt = TestCase $ do
      let gCat = fromRight $ chNonUserAt g1 0 $ Str "cat"
      let gUses = fromRight $ chNonUserAt g1 1 $ stringToTplt "_ uses _"
      assertBool "change Str" $ 
        Str "cat" == (lab' $ fromJust $ fst $ match 0 $ gCat)
      assertBool "change Tplt" $ 
        stringToTplt "_ uses _" == (lab' $ fromJust $ fst $ match 1 $ gUses)
      assertBool "not in graph" $
        isLeft $ chNonUserAt g1 15  $ stringToTplt "_ uses _"
      assertBool "change Rel" $
        isLeft $ chNonUserAt g1 11  $ Coll
      assertBool "constructor mismatch" $
        isLeft $ chNonUserAt g1 4  $ stringToTplt "_ is _" -- LNode 4 is a Str

    tChMbr = TestCase $ do
      let gDogDog = fromRight $ chRelMbr g1 5 0 (Mbr 2)
      let gImpossible         = chRelMbr g1 5 0 (Mbr 99) -- 99 is too high
      assertBool "1" $ showExpr Map.empty gDogDog 5 
                       == "5:1 \171\&0: dog\187 wants \171\&0: dog\187"
        -- (\& == empty string) is to distinguish from longer number
        -- if special char followed by a non-digit, no \& necessary
      assertBool "RelPos out of range" $ isLeft gImpossible

  -- ask, minor
    tAskMinor = TestList [ TestLabel "tGelemM" tGelemM
                         , TestLabel "tHasLEdgeM" tHasLEdgeM
                         , TestLabel "tIsTplt" tIsTplt
                         , TestLabel "tTpltAt" tTpltAt
                         , TestLabel "tTpltForRelAt" tTpltForRelAt
                         , TestLabel "tTpltArity" tTpltArity ]

    tGelemM = TestCase $ do
      assertBool "1" $ gelemM g1 0 == Right ()
      assertBool "2" $ gelemM g1 100 == Left "gelemM: Node 100 absent."

    tHasLEdgeM = TestCase $ do
      assertBool "has it" $ hasLEdgeM g1 (5,0,RelEdge $ Mbr 1) == Right ()
      assertBool "lacks it" $ isLeft $ hasLEdgeM g1 (5,0,RelEdge $ Mbr 2)

    tIsTplt = TestCase $ do
      assertBool "is template" $ isTplt g1 1 == Right True
      assertBool "is not template" $ isTplt g1 0 == Right False
      assertBool "missing" $ isLeft $ isTplt g1 (-1)

    tTpltAt = TestCase $ do
      assertBool "normal" $ tpltAt g1 1 == ( Right $ Tplt [""," wants ",""] )
      assertBool "notATplt" $ isLeft $ tpltAt g1 0
      assertBool "absent" $ isLeft $ tpltAt g1 (-1)

    tTpltForRelAt = TestCase $ do
      assertBool "normal" $ relTplt g1 5 ==
        ( Right $ Tplt [""," wants ",""] )
      assertBool "not a Rel" $ isLeft $ relTplt g1 1
      assertBool "absent" $ isLeft $ relTplt g1 (-1)

    tTpltArity = TestCase $ do
      assertBool "arity 0" $
        tpltArity (Tplt ["no args possible here"]) == 0
      assertBool "arity 1" $ 
        tpltArity (Tplt ["one arg","possible here"]) == 1
      -- ? how to test the following
      -- assertBool "Str is not Tplt" $ tpltArity (Str "nog")
        -- == error "tpltArity: Expr not a Tplt."

  -- ask [Node]
    tAskNodes = TestList [ TestLabel "tUsers" tUsers
                         , TestLabel "tSpecUsers" tSpecUsers
                         , TestLabel "tMatchRel" tMatchRel
                         ]

    tUsers = TestCase $ do
      assertBool "1" $ users g1 0 == Right [5,6,8]
      assertBool "2" $ isLeft $ (users g1 100 :: Either String [Dwt.Node])

    tSpecUsers = TestCase $ do
      assertBool "with Arity" $ specUsers g1 0 (Mbr 1) == Right [5,6,8]
      assertBool "without Arity" $ specUsersUsf g1 0 (Mbr 1) == [5,6,8]

    tMatchRel = TestCase $ do
      assertBool "dog in first pos"     $ matchRel g1 relSpec == Right [5,6,8]
      assertBool "nothing should match" $ matchRel g1 relSpecNonsense == Right []
