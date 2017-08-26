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
    hash 2 (Joint "") (Leaf "hi") (Leaf "there")
    == RelX (EO True 2) (Leaf "hi") (Joint "") [] (Leaf "there")
  assertBool "2" $
    rightConcat (Joint "") (Leaf "na")
    (RelX (EO True 3) (Leaf "left") (Joint "zaba") [] (Leaf "right"))
    == RelX (EO True 3) (Leaf "left") (Joint "zaba") [(Leaf "right",Joint "")] (Leaf "na")
  assertBool "3" $
    leftConcat (Joint "new") (Leaf "new")
    (RelX (EO True 4) (Leaf "left") (Joint "j") [] (Leaf "right"))
    == RelX (EO True 4) (Leaf "new") (Joint "new") [(Leaf "left",Joint "j")] (Leaf "right")
  assertBool "4" $ p == Right a where
      p = parse expr "dogs #like you ###becase you #(give or misplace) (bones #that reek like demons) #to dogs ##sometimes"
      a = RelX
          (EO True 3)
          (RelX
            (EO True 1)
            (Leaf "dogs")
            (Joint "like") []
            (Leaf "you"))
          (Joint "becase") []
          (RelX
            (EO True 2)
            (RelX
              (EO True 1)
              (Leaf "you")
              (Joint "give or misplace")
              [ (RelX
                  (EO False 1)
                  (Leaf "bones")
                  (Joint "that") []
                  (Leaf "reek like demons")
                , Joint "to")]
              (Leaf "dogs"))
            (Joint "sometimes") []
            (Leaf ""))

