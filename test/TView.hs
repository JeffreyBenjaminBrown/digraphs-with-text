    module TView where

    import Dwt.Graph
    import Dwt.View
    import TData

    import Test.HUnit
    import qualified Data.Map as Map

    tView = TestList [ TestLabel "tShowExpr" tShowExpr
                     ]

    tShowExpr = TestCase $ do
      assertBool "expr 5" $ showExpr Map.empty g1 5 
                            == "5:1 \171\&0: dog\187 wants \171\&4: brandy\187"
      assertBool "expr 11" $ showExpr Map.empty g1 11
        == "11:9 statement \171\&5:1 \171\&0: dog\187 wants \171\&4: brandy\187\187 is \171\&10: dubious\187"
      assertBool "expr 11" $ showExpr (Map.fromList [(0,"SUB")]) g1 11 
        == "11:9 statement \171\&5:1 \171SUB\187 wants \171\&4: brandy\187\187 is \171\&10: dubious\187"
