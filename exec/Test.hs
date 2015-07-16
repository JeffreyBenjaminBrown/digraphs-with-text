import Test.HUnit
import Dwt
import qualified Dwt.Util as Util
import qualified Data.Map as Map
                           
main = runTestTT testList

testList = TestList 
  [ -- TestLabel "tStmt" tStmt -- why was I doing that, if these work?
    tCountHoles
  , tGraphTypes
  , tAddRelIdxToNode
  , tInsertAfterPos
  , tAdd_ToGraph
  ]

tCountHoles = TestCase $ do
  let s = ""
      t = "_"
      u = "_ has _"
  assertBool "counting"
    (Prelude.map Util.countHoles [s,t,u] == [0,1,2])

tInsertAfterPos = TestCase $ do 
  assertBool ("0: prepend")     $ Util.insertAfterPos 0 1 [0,0] == [1,0,0]
  assertBool ("null: fine")     $ Util.insertAfterPos 0 1 []    == [1]
  assertBool ("too small: prepend")$ Util.insertAfterPos (-1) 1 [] == [1]
  assertBool ("too big: append")   $ Util.insertAfterPos 2 1 [] == [1]
  assertBool ("1: after first") $ Util.insertAfterPos 1 6 [0,0] == [0,6,0]

tAddRelIdxToNode = TestCase $ do
  let s = Stmt "_ needs _"
      ri = RelIdx 1
      before = StmtNode { _stmt = s, _relIdxs = [] }
      after  = StmtNode { _stmt = s, _relIdxs = [ri] }
      couldHappen  = StmtNode { _stmt = s, _relIdxs = [ri,ri] }
  assertBool "The tAddRelIdxToNode comment." 
    (addRelIdxToNode ri before == after)
  assertBool "addRelIdxToNode can cause dups."
    (addRelIdxToNode ri after == couldHappen)

tGraphTypes = TestCase $ do -- bigger graph
  let si = StmtIdx 1
      sni = StmtNodeIdx si
      s = Stmt "_ needs _"
      sn = StmtNode { _stmt = s, _relIdxs = [ri] }
      si2 = StmtIdx 2
      sni2 = StmtNodeIdx si2
      s2 = Stmt "dog"
      sn2 = StmtNode { _stmt = s2, _relIdxs = [ri] }
      si3 = StmtIdx 3
      sni3 = StmtNodeIdx si3
      s3 = Stmt "collar"
      sn3 = StmtNode { _stmt = s3, _relIdxs = [ri2] } -- ri2: false (it's ri1)
      ri = RelIdx 1
      rni = RelNodeIdx ri
      r = Rel { _tplt = si, _mbrs = [sni2, sni3] }
        -- dog needs collar
      rn = RelNode  { _rel  = r, _relIdxs = [] }
      ri2 = RelIdx 2
      rni2 = RelNodeIdx ri2
      r2 = Rel { _tplt = si, _mbrs = [sni2, sni2] }
        -- dog needs dog
      rn2 = RelNode  { _rel  = r2, _relIdxs = [] }
      g = Graph { 
        _nodeMap = Map.insert sni sn $ Map.insert sni2 sn2
          $ Map.insert sni3 sn3 $ Map.insert rni rn 
          $ Map.insert rni2 rn2 $ Map.empty
        , _maxStmtIdx = 3
        , _maxRelIdx = 2 }
  assertBool "1"     (graphSupportsRelIdxInNode g sni ri)
  assertBool "1.1"   (isTpltOfRel       r sni    )
  assertBool "1.1.5" (not $ isTpltOfRel r sni2   )
  assertBool "1.3"   (not $ isMbrOfRel   r sni    )
  assertBool "2" (graphSupportsRelIdxInNode   g sni2 ri)
  assertBool "2.1" (not $ isTpltOfRel r sni2    )
  assertBool "2.3" (      isMbrOfRel   r sni2    )
  assertBool "3" (graphSupportsRelIdxInNode         g sni3 ri)
  assertBool "3.1" (not $ graphSupportsRelIdxInNode g sni3 ri2)
    -- Works even on sni3, which has false relIdxs field -- good, because it checks ri for containing ni, not the reverse.
  assertBool "4" (      hasValidRelIdxs g sni)
  assertBool "5" (      hasValidRelIdxs g sni2)
  assertBool "6" (not $ hasValidRelIdxs g sni3)
  assertBool "7" (      hasValidRelIdxs g rni)

tAdd_ToGraph = TestCase $ do -- bigger graph
  let g = addStmtToGraph (Stmt "dog")                  -- StmtIdx 3
        $ addStmtToGraph (Stmt "fur")                  -- StmtIdx 2
        $ addStmtToGraph (Stmt "_ has a|some _")       -- StmtIdx 1
        $ emptyGraph
      g' = addRelToGraph ( Rel -- "dog has a|some fur" -- RelIdx 1
                           (StmtIdx 1)
                           [StmtNodeIdx (StmtIdx 3)
                           , StmtNodeIdx (StmtIdx 2) ] )
         $ g
  putStr $ show g' -- Works! Visually inspected. Even idxs as hoped.
    ++ "\n"
  assertBool "1" True

-- eof

