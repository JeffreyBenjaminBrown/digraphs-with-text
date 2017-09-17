{-# LANGUAGE OverloadedStrings #-}

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
  assertBool "4" $ either (const True) (const False) $ parse expr "" ")("

  assertBool "1"
    $ hash 2 ("") (QLeaf $ Word "hi", disregardedEo)
                  (QLeaf $ Word "there", disregardedEo)
    == (QRel disregardedEo [""] [(QLeaf $ Word "hi")
                                ,(QLeaf $ Word "there")]
       , EO True 2)

  assertBool "2" $
    rightConcat ("") (QLeaf $ Word "na", disregardedEo)
    (QRel disregardedEo ["zaba"] [(QLeaf $ Word "left")
                                 ,(QLeaf $ Word "right")]
    , EO True 3)
    == (QRel disregardedEo ["zaba",""] [(QLeaf $ Word "left")
                                        ,(QLeaf $ Word "right")
                                        ,(QLeaf $ Word "na")]
       , EO True 3)

  assertBool "3"
    $ leftConcat ("new") (QLeaf $ Word "new", disregardedEo)
      (QRel disregardedEo ["j"] [(QLeaf $ Word "left")
                                ,(QLeaf $ Word "right")]
      , EO True 4)
    == (QRel disregardedEo ["new","j"]
        [(QLeaf $ Word "new")
        ,(QLeaf $ Word "left")
        ,(QLeaf $ Word "right")]
       , EO True 4)

  let p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = QRel (EO True 3) 
            ["becase"] 
            [QRel (EO True 1) 
              ["like"] 
              [QLeaf (Word "dogs")
              ,QLeaf (Word "you")]
            ,QRel (EO True 2) 
              ["sometimes"] 
              [QRel (EO True 1) 
                ["give or misplace", "to"] 
                [QLeaf (Word "you")
                ,QRel (EO False 1) 
                  ["that"] 
                  [QLeaf (Word "bones")
                  ,QLeaf  (Word "reek super nasty")]
                ,QLeaf (Word "dogs")]
              ,Absent]]
    in assertBool "4" $ p == Right a
