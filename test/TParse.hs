module TParse where

import Dwt
import TData
import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)


tParse = TestList [ TestLabel "tParseInner" tParseInner
                  ]

tParseInner = TestList [ TestLabel "tJoint" tJoint
                       ]

tJoint = TestCase $ do
  assertBool "1" $
    joint 2 (Joint "") (Leaf "hi") (Leaf "there")
    == BothX (EO False 2) (Leaf "hi") (Joint "") [] (Leaf "there")
  assertBool "2" $
    rightConcat (Joint "") (Leaf "na")
    (BothX (EO True 3) (Leaf "left") (Joint "zaba") [] (Leaf "right"))
    == BothX (EO True 3) (Leaf "left") (Joint "zaba") [(Leaf "right",Joint "")] (Leaf "na")
  assertBool "3" $
    leftConcat (Joint "new") (Leaf "new")
    (BothX (EO True 4) (Leaf "left") (Joint "j") [] (Leaf "right"))
    == BothX (EO True 4) (Leaf "new") (Joint "new") [(Leaf "left",Joint "j")] (Leaf "right")
