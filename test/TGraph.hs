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
                      , TestLabel "tChase"      tChase 
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
      assertBool "mkTplt (and thereby splitStringForTplt), insRelUsf, insStr, insTplt" $ g1 == g1Alt

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
      let gg = fromRight $ insColl (j 10) [0,3,4] g1
          collMbrEdge = CollEdge CollMbr
          nextNode = head $ newNodes 1 g1
      assertBool "new 12th node" 
        $ (lab' $ fromJust $ fst $ match nextNode gg) == Coll
      assertBool "3 new edges" $ lsuc gg nextNode 
        == [ (0, collMbrEdge), (3, collMbrEdge), (4, collMbrEdge)
           , (10, CollEdge CollPrinciple)]
      assertBool "only 1 new node, only 4 new edges"
        $    (length $ nodes g1) + 1 == (length $ nodes gg)
          && (length $ edges g1) + 4 == (length $ edges gg)

    tPartitionRelSpec = TestCase $ do
      let (vs,ns) = partitionRelSpec tRelSpec
      assertBool "1" $ Set.fromList (Map.toList vs)
        == Set.fromList [ (RelTplt, It)
                        , (Mbr 2,   Any) ]
      assertBool "2" $ Set.fromList (Map.toList ns)
        == Set.fromList [ (Mbr 1, 0) ]

    tInsRelSpec = TestCase $ do
      let (vs,ns) = partitionRelSpec tRelSpec
          Right g2 = insRelSpec tRelSpec g1
          [newNode] = newNodes 1 g1
      assertBool "node" $ (fromJust $ lab g2 newNode) == RelSpecExpr vs
      assertBool "only 1 more edge" $ 
        (length $ edges g1) + 1 == (length $ edges g2)
      assertBool "the edge" $ hasLEdge g2 (newNode, 0, RelEdge $ Mbr 1)

    tChNonRelAt = TestCase $ do
      let gCat = fromRight $ chNonUser g1 0 $ Str "cat"
      let gUses = fromRight $ chNonUser g1 1 $ mkTplt "_ uses _"
      assertBool "change Str" $ 
        Str "cat" == (lab' $ fromJust $ fst $ match 0 $ gCat)
      assertBool "change Tplt" $ 
        mkTplt "_ uses _" == (lab' $ fromJust $ fst $ match 1 $ gUses)
      assertBool "not in graph" $
        isLeft $ chNonUser g1 15  $ mkTplt "_ uses _"
      assertBool "change Rel" $
        isLeft $ chNonUser g1 11  $ Coll
      assertBool "constructor mismatch" $
        isLeft $ chNonUser g1 4  $ mkTplt "_ is _" -- LNode 4 is a Str

    tChMbr = TestCase $ do
      let gDogDog = fromRight $ chRelMbr g1 5 0 (Mbr 2)
      let gImpossible         = chRelMbr g1 5 0 (Mbr 99) -- 99 is too high
      assertBool "1" $ showExpr gDogDog 5
                       == "5:1 0: dog #wants 0: dog"
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
      assertBool "is template" $ isRight $ isTpltM g1 1
      assertBool "is not template" $ isLeft $ isTpltM g1 0
      assertBool "missing" $ isLeft $ isTpltM g1 (-1)

    tTpltAt = TestCase $ do
      assertBool "normal" $ tpltAt g1 1 == ( Right $ Tplt ["","wants",""] )
      assertBool "notATplt" $ isLeft $ tpltAt g1 0
      assertBool "absent" $ isLeft $ tpltAt g1 (-1)

    tTpltForRelAt = TestCase $ do
      assertBool "normal" $ relTplt g1 5 ==
        ( Right $ Tplt ["","wants",""] )
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
      assertBool "with Arity" $ usersInRole g1 0 (Mbr 1) == Right [5,6,8]
      assertBool "without Arity" $ usersInRoleUsf g1 0 (Mbr 1) == [5,6,8]

    tMatchRel = TestCase $ do
      assertBool "dog in first pos"     $ matchRel g1 tRelSpec == Right [5,6,8]
      assertBool "nothing should match" $ matchRel g1 tRelSpecNonsense == Right []

  -- chase and helpers
    tChase = TestList [ TestLabel "tHas1Up" tHas1Up
                      , TestLabel "tFork1Up" tFork1Up
                      , TestLabel "tValidRole"tValidRole
                      , TestLabel "tRelElts" tRelElts
                      ]

    tHas1Up = TestCase $ do
      assertBool "has 1 Up" $ has1Up tRelSpecNonsense
      assertBool "has no Up" $ not $ has1Up tRelSpec

    tFork1Up = TestCase $ do -- todo, incomplete
      assertBool "no Up vars, should fail"
        $ isLeft $ fork1Up g1 0 tRelSpec
      assertBool "dog(ana) wants brandy(kata)" 
        $ fork1Up g1 0 tRelSpec2 == Right [4]

    tValidRole = TestCase $ do
      assertBool "Tplt: valid role" $ isRight $ validRole g1 5 RelTplt
      assertBool "Mbr 0: not valid role" $ isLeft $  validRole g1 5 (Mbr 0)
      assertBool "Mbr 1: valid role" $ isRight $ validRole g1 5 (Mbr 1)
      assertBool "Mbr 3: too big, invalid role" $ isLeft $  validRole g1 5 (Mbr 3)

    tRelElts = TestCase $ do
      assertBool "dog wants water -> dog" $ relElts g1 5 [Mbr 1] == Right [0]
      assertBool "dog wants water -> dog" $ relElts g1 5 [RelTplt] == Right [1]
      assertBool "dog wants water -> dog" $ isLeft $ relElts g1 5 [Mbr 3]
