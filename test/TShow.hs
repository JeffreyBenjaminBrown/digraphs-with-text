module TShow where

import Dwt.Show
import TData

import Test.HUnit

tShow = TestList [ TestLabel "tShowExpr" tShowExpr
                 ]

tShowExpr = TestCase $ do
  assertBool "expr 5" $ showExpr g1 5 == "dog ##wants brandy"
  assertBool "expr 5, verbose"
    $ showExprVerbose g1 5 == "5:1 0: dog ##wants 4: brandy"
  assertBool "expr 11" $ showExprVerbose g1 11 ==
    "11:9 5:1 0: dog ##wants 4: brandy ####is 10: dubious"
