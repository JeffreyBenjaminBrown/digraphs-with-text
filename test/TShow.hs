    module TShow where

    import Dwt.Show
    import TData

    import Test.HUnit
    import qualified Data.Map as Map

    tView = TestList [ TestLabel "tShowExpr" tShowExpr
                     ]

    tShowExpr = TestCase $ do
      assertBool "expr 5" $ showExpr g1 5 == "5:1 0: dog ##wants 4: brandy"
      assertBool "expr 11" $ showExpr g1 11 ==
        "11:9 5:1 0: dog ##wants 4: brandy ####is 10: dubious"
--      assertBool "expr 11" $ showExpr (Map.fromList [(0,"SUB")]) g1 11 
--        == "11:9 statement \171\&5:1 \171SUB\187 wants \171\&4: brandy\187\187 is \171\&10: dubious\187"
