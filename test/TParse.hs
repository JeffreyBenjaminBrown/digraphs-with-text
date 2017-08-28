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
    hash 2 (JointX "") (LeafX "hi") (LeafX "there")
    == RelX (EO True 2) (LeafX "hi") (JointX "") [] (LeafX "there")
  assertBool "2" $
    rightConcat (JointX "") (LeafX "na")
    (RelX (EO True 3) (LeafX "left") (JointX "zaba") [] (LeafX "right"))
    == RelX (EO True 3) (LeafX "left") (JointX "zaba") [(LeafX "right",JointX "")] (LeafX "na")
  assertBool "3" $
    leftConcat (JointX "new") (LeafX "new")
    (RelX (EO True 4) (LeafX "left") (JointX "j") [] (LeafX "right"))
    == RelX (EO True 4) (LeafX "new") (JointX "new") [(LeafX "left",JointX "j")] (LeafX "right")
  assertBool "4" $ p == Right a where
      p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = RelX
          (EO True 3)
          (RelX
            (EO True 1)
            (LeafX "dogs")
            (JointX "like") []
            (LeafX "you"))
          (JointX "becase") []
          (RelX
            (EO True 2)
            (RelX
              (EO True 1)
              (LeafX "you")
              (JointX "give or misplace")
              [ (RelX
                  (EO False 1)
                  (LeafX "bones")
                  (JointX "that") []
                  (LeafX "reek super nasty")
                , JointX "to")]
              (LeafX "dogs"))
            (JointX "sometimes") []
            (LeafX ""))

