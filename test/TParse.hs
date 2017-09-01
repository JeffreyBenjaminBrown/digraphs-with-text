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
    hash 2 (JointX "") (LeafX $ Word "hi") (LeafX $ Word "there")
    == RelX (EO True 2) [JointX ""] [(LeafX $ Word "hi")
                                    ,(LeafX $ Word "there")]
  assertBool "2" $
    rightConcat (JointX "") (LeafX $ Word "na")
    (RelX (EO True 3) [JointX "zaba"] [(LeafX $ Word "left")
                                      ,(LeafX $ Word "right")])
    == RelX (EO True 3) [JointX "zaba",JointX ""] [(LeafX $ Word "left")
                                        ,(LeafX $ Word "right")
                                        ,(LeafX $ Word "na")]
  assertBool "3"
    $ leftConcat (JointX "new") (LeafX $ Word "new")
      (RelX (EO True 4) [JointX "j"] [(LeafX $ Word "left")
                                    ,(LeafX $ Word "right")])
    == RelX (EO True 4) [JointX "new",JointX "j"]
      [(LeafX $ Word "new"),(LeafX $ Word "left"),(LeafX $ Word "right")]
  assertBool "4" $ p == Right a where
      p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = RelX 
            (EO True 3) 
            [JointX "becase"] 
            [RelX 
              (EO True 1) 
              [JointX "like"] 
              [LeafX (Word "dogs")
              ,LeafX (Word "you")]
            ,RelX 
              (EO True 2) 
              [JointX "sometimes"] 
              [RelX 
                (EO True 1) 
                [JointX "give or misplace", JointX "to"] 
                [LeafX (Word "you")
                ,RelX 
                  (EO False 1) 
                  [JointX "that"] 
                  [LeafX (Word "bones")
                  ,LeafX  (Word "reek super nasty")]
                ,LeafX (Word "dogs")]
              ,Absent]]
