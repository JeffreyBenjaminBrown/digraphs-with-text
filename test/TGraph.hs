module TGraph where

import Dwt hiding (fromRight)
import Data.Graph.Inductive
import TData
import Dwt.Search.Base (relElts, validMbrRole, users, relTplt, tpltAt
                       , _usersInRole)
import Test.HUnit hiding (Node)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Either
import qualified Control.Lens.Lens as L -- not crit; for (&), only used once

tGraph = TestList [ TestLabel "tBuildGraph" tBuildGraph
                  , TestLabel "tAskMinor"   tAskMinor
                  , TestLabel "tAskNodes"   tAskNodes
                  , TestLabel "tChase"      tChase
                  , TestLabel "tRecursiveSearch" tRecursiveSearch
                  ]

-- ==== buildGraph
tBuildGraph = TestList [ TestLabel "tSubInTplt" tSubInTplt
                       , TestLabel "tInsert" tInsert
                       , TestLabel "tInsRelM" tInsRelM
                       -- , TestLabel "tInsColl" tInsColl
                       , TestLabel "tPartitionRelSpec" tPartitionRelSpec
                       , TestLabel "tInsRelSpec" tInsRelSpec
                       -- , TestLabel "tChLeafAt" tChLeafAt
                       -- , TestLabel "tChMbr" tChMbr
                       ]

tSubInTplt = TestCase $ do
  assertBool "1" $ subInTplt (fromJust $ lab g1 1) ["man","peace"]
    == "man #wants peace"
  assertBool "2"
    $ (lab g1 1 L.& fromJust L.& subInTplt $ ["man","peace"])
    == "man #wants peace"

tInsert = TestCase $ do
  assertBool "mkTplt (and thereby splitStringForTplt), insRelUsf, insWord, insTplt" $ g1 == g1Alt

tInsRelM = TestCase $ do
  assertBool "1" $ (insRel 2 [0,0] g1 :: Either DwtErr RSLT)
        == (Right $ insRelUsf  2 [0,0] g1)
  assertBool "2" $
    let Left (a,_,_) = (insRel 15 [0,0] g1 :: Either DwtErr RSLT)
    in a == FoundNo
  assertBool "3" $
    let Left (a,_,_) = (insRel 2 [100,0] g1 :: Either DwtErr RSLT)
    in a == FoundNo
  assertBool "4" $
    let Left (a,_,_) = (insRel 2 [1,1,1] g1 :: Either DwtErr RSLT)
    in a == ArityMismatch
  assertBool "5" $
    let Left (a,_,_) = (insRel 0 [1,1,1] g1 :: Either DwtErr RSLT)
    in a == NotTplt

--    tInsColl = TestCase $ do
--      let gg = fromRight $ insColl (Just 10) [0,3,4] g1 :: RSLT
--          collMbrEdge = CollEdge CollMbr
--          nextNode = head $ newNodes 1 g1
--      assertBool "new 12th node" 
--        $ (lab' $ fromJust $ fst $ match nextNode gg) == Coll
--      assertBool "3 new edges" $ lsuc gg nextNode 
--        == [ (0, collMbrEdge), (3, collMbrEdge), (4, collMbrEdge)
--           , (10, CollEdge CollPrinciple)]
--      assertBool "only 1 new node, only 4 new edges"
--        $    (length $ nodes g1) + 1 == (length $ nodes gg)
--          && (length $ edges g1) + 4 == (length $ edges gg)

tPartitionRelSpec = TestCase $ do
  let Right (vs,ns) = partitionRelSpec g1 tRelSpec
  assertBool "1" $ Set.fromList (Map.toList vs)
    == Set.fromList [ (TpltRole, It)
                    , (Mbr 2,   Any) ]
  assertBool "2" $ Set.fromList (Map.toList ns)
    == Set.fromList [ (Mbr 1, 0) ]

tInsRelSpec = TestCase $ do
  let Right (vs,_) = partitionRelSpec g1 tRelSpec
      Right g2 = insRelSpec tRelSpec g1
      [newNode] = newNodes 1 g1
  assertBool "node" $ lab g2 newNode == Just (RelSpecExpr vs)
  assertBool "only 1 more edge" $ 
    (length $ edges g1) + 1 == (length $ edges g2)
  assertBool "the edge" $ hasLEdge g2 (newNode, 0, RelEdge $ Mbr 1)

--    tChNonRelAt = TestCase $ do
--      let gCat = fromRight $ chLeaf g1 0 $ Word "cat"
--      let gUses = fromRight $ chLeaf g1 1 $ mkTplt "_ uses _"
--      assertBool "change Word" $ 
--        Word "cat" == (lab' $ fromJust $ fst $ match 0 $ gCat)
--      assertBool "change Tplt" $ 
--        mkTplt "_ uses _" == (lab' $ fromJust $ fst $ match 1 $ gUses)
--      assertBool "not in graph" $
--        isLeft $ chLeaf g1 15  $ mkTplt "_ uses _"
--      assertBool "change Rel" $
--        isLeft $ chLeaf g1 11  $ Coll
--      assertBool "constructor mismatch" $
--        isLeft $ chLeaf g1 4  $ mkTplt "_ is _" -- LNode 4 is a Word

--    tChMbr = TestCase $ do
--      let gDogDog = fromRight $ chRelRole g1 5 0 (Mbr 2)
--      let gImpossible         = chRelRole g1 5 0 (Mbr 99) -- 99 is too high
--      assertBool "1" $ showExpr gDogDog 5
--                       == "5:1 0: dog ##wants 0: dog"
--      assertBool "MbrPos out of range" $ isLeft gImpossible

-- ==== ask, minor
tAskMinor = TestList [ TestLabel "tGelemM" tGelemM
                     , TestLabel "tHasLEdgeM" tHasLEdgeM
                     , TestLabel "tIsTplt" tIsTplt
                     , TestLabel "tTpltAt" tTpltAt
                     , TestLabel "tTpltForRelAt" tTpltForRelAt
                     , TestLabel "tTpltArity" tTpltArity ]

tGelemM = TestCase $ do
  assertBool "1" $ gelemM g1 0 == Right ()
  assertBool "2" $ let Left (a,_,_) = gelemM g1 100 in a == FoundNo

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
  -- assertBool "Word is not Tplt" $ tpltArity (Word "nog")
    -- == error "tpltArity: Expr not a Tplt."

-- ==== ask [Node]
tAskNodes = TestList [ TestLabel "tUsers" tUsers
                     , TestLabel "tSpecUsers" tSpecUsers
                     , TestLabel "tMatchRel" tMatchRel
                     ]

tUsers = TestCase $ do
  assertBool "1" $ users g1 0 == Right [5,6,8]
  assertBool "2" $ isLeft $ (users g1 100 :: Either DwtErr [Node])

tSpecUsers = TestCase $ do
  assertBool "with Arity" $ _usersInRole g1 0 (Mbr 1) == Right [5,6,8]

tMatchRel = TestCase $ do
  assertBool "dog in first pos"     $ matchRelSpecNodes g1 tRelSpec == Right [5,6,8]
  assertBool "nothing should match" $ matchRelSpecNodes g1 tRelSpecNonsense == Right []

-- ==== chase and helpers
tChase = TestList [ TestLabel "tHas1Up" tHas1Dir
                  , TestLabel "tFork1Dir" tFork1Dir
                  , TestLabel "tValidRole"tValidRole
                  , TestLabel "tRelElts" tRelElts
                  ]

tHas1Dir = TestCase $ do
  assertBool "has 1 Up" $ has1Dir Up tRelSpecNonsense
  assertBool "has no Up" $ not $ has1Dir Up tRelSpec

tFork1Dir = TestCase $ do -- todo, incomplete
  assertBool "searching Down, and no Up vars; should fail"
    $ isLeft $ fork1Dir g1 (At 0) (Down, tRelSpec)
  assertBool "dog(ana) wants brandy(kata)" 
    $ fork1Dir g1 (At 0) (Down, tRelSpec2) == Right [4]

tValidRole = TestCase $ do
  assertBool "Tplt: valid role" $ isRight $ validMbrRole g1 5 TpltRole
  assertBool "Mbr 0: not valid role" $ isLeft $  validMbrRole g1 5 (Mbr 0)
  assertBool "Mbr 1: valid role" $ isRight $ validMbrRole g1 5 (Mbr 1)
  assertBool "Mbr 3: too big, invalid role" $ isLeft $ validMbrRole g1 5 (Mbr 3)

tRelElts = TestCase $ do
  assertBool "dog wants water -> dog" $ relElts g1 5 [Mbr 1] == Right [0]
  assertBool "dog wants water -> dog" $ relElts g1 5 [TpltRole] == Right [1]
  assertBool "dog wants water -> dog" $ isLeft $ relElts g1 5 [Mbr 3]

tRecursiveSearch = TestCase $ do
  let g = mkGraph [(0,Word "a"),(1,Word "b"),(2,Tplt ["","is",""]),(3,Rel),(4,Word "c"),(5,Tplt ["","uses",""]),(6,Rel),(7,Word "d"),(8,Rel),(9,Word "f"),(10,Rel),(11,Word "g"),(12,Rel)] [(3,0,RelEdge (Mbr 1)),(3,1,RelEdge (Mbr 2)),(3,2,RelEdge TpltRole),(6,0,RelEdge (Mbr 1)),(6,4,RelEdge (Mbr 2)),(6,5,RelEdge TpltRole),(8,1,RelEdge (Mbr 1)),(8,2,RelEdge TpltRole),(8,7,RelEdge (Mbr 2)),(10,1,RelEdge (Mbr 1)),(10,2,RelEdge TpltRole),(10,9,RelEdge (Mbr 2)),(12,2,RelEdge TpltRole),(12,7,RelEdge (Mbr 1)),(12,11,RelEdge (Mbr 2))]
      rspec = Map.fromList [(TpltRole, NodeSpec $ QLeaf $ mkTplt "_ is _"),(Mbr 1, VarSpec Up), (Mbr 2, VarSpec Down)]
  assertBool "1" $ dwtDfs g (Down, rspec) [0] == Right [0,1,7,11,9]
  assertBool "2" $ dwtBfs g (Down, rspec) [0] == Right [0,1,7,9,11]
