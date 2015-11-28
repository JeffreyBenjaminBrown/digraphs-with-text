-- how to run
  -- import tests
  -- runTestTT testList

-- imports
  import Test.HUnit
  
  import Dwt
  import Reln
  import qualified Data.List as List
  import qualified Data.Map as Map

-- list of tests
  testList = TestList 
    [ TestLabel "gnodeConn" tGnodeConn
    , TestLabel "gnodeDisc" tGnodeDisc
    ]
  
-- tests
  tGnodeConn = TestCase $ do
    let g1 = Gnode { gIdx = GnodeIdx 1, tx = "first!", conns = Map.empty }
        g2 = Gnode { gIdx = GnodeIdx 2, tx = "second!", conns = Map.empty }
        r  = Reln {dirn = Succ, dtIdx = DirdTxIdx (-1)}
        g2' = g2 {conns = Map.fromList [(r, [GnodeIdx {gnodeIdx = 1}])]} 
    assertBool "should change" ( (gnodeConn g1 r g2) == g2' )
    assertBool "should not change" ( (gnodeConn g1 r g2') == g2' )

  tGnodeDisc = TestCase $ do
    let g1 = Gnode { gIdx = GnodeIdx 1, tx = "first!", conns = Map.empty }
        g2 = Gnode { gIdx = GnodeIdx 2, tx = "second!", conns = Map.empty }
        r  = Reln {dirn = Succ, dtIdx = DirdTxIdx (-1)}
        g2' = g2 {conns = Map.fromList [(r, [GnodeIdx {gnodeIdx = 1}])]} 
    assertBool "should change" ( (gnodeDisc g1 r g2') == g2 )
    -- assertBool "should not change" ( (gnodeDisc g1 r g2) == g2 )
      -- the result for asking for an already disconnected disconnect is currently undefined
      -- TO DO, safety: Is this bad?

-- EOF

