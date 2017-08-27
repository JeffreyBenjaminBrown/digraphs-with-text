module TParse where

import Dwt
import Test.HUnit

import Text.Megaparsec (parse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)


tParse = TestList [ TestLabel "tParseInner" tParseInner
                  ]

tParseInner = TestList [ TestLabel "tHash" tHash
                       ]

tHash = TestCase $ do
  assertBool "1" $
    hash 2 (JointX "") (Leaf "hi") (Leaf "there")
    == RelX (EO True 2) (Leaf "hi") (JointX "") [] (Leaf "there")
  assertBool "2" $
    rightConcat (JointX "") (Leaf "na")
    (RelX (EO True 3) (Leaf "left") (JointX "zaba") [] (Leaf "right"))
    == RelX (EO True 3) (Leaf "left") (JointX "zaba") [(Leaf "right",JointX "")] (Leaf "na")
  assertBool "3" $
    leftConcat (JointX "new") (Leaf "new")
    (RelX (EO True 4) (Leaf "left") (JointX "j") [] (Leaf "right"))
    == RelX (EO True 4) (Leaf "new") (JointX "new") [(Leaf "left",JointX "j")] (Leaf "right")
  assertBool "4" $ p == Right a where
      p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = RelX
          (EO True 3)
          (RelX
            (EO True 1)
            (Leaf "dogs")
            (JointX "like") []
            (Leaf "you"))
          (JointX "becase") []
          (RelX
            (EO True 2)
            (RelX
              (EO True 1)
              (Leaf "you")
              (JointX "give or misplace")
              [ (RelX
                  (EO False 1)
                  (Leaf "bones")
                  (JointX "that") []
                  (Leaf "reek super nasty")
                , JointX "to")]
              (Leaf "dogs"))
            (JointX "sometimes") []
            (Leaf ""))

