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

  assertBool "1" $
    hash 2 ("") (QQLeaf $ Word "hi") (QQLeaf $ Word "there")
    == QRel (EO True 2) [""] [(QQLeaf $ Word "hi")
                                    ,(QQLeaf $ Word "there")]

  assertBool "2" $
    rightConcat ("") (QQLeaf $ Word "na")
    (QRel (EO True 3) ["zaba"] [(QQLeaf $ Word "left")
                                      ,(QQLeaf $ Word "right")])
    == QRel (EO True 3) ["zaba",""] [(QQLeaf $ Word "left")
                                        ,(QQLeaf $ Word "right")
                                        ,(QQLeaf $ Word "na")]

  assertBool "3"
    $ leftConcat ("new") (QQLeaf $ Word "new")
      (QRel (EO True 4) ["j"] [(QQLeaf $ Word "left")
                                    ,(QQLeaf $ Word "right")])
    == QRel (EO True 4) ["new","j"]
      [(QQLeaf $ Word "new"),(QQLeaf $ Word "left"),(QQLeaf $ Word "right")]
  assertBool "4" $ p == Right a where
      p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = QRel 
            (EO True 3) 
            ["becase"] 
            [QRel 
              (EO True 1) 
              ["like"] 
              [QQLeaf (Word "dogs")
              ,QQLeaf (Word "you")]
            ,QRel 
              (EO True 2) 
              ["sometimes"] 
              [QRel 
                (EO True 1) 
                ["give or misplace", "to"] 
                [QQLeaf (Word "you")
                ,QRel 
                  (EO False 1) 
                  ["that"] 
                  [QQLeaf (Word "bones")
                  ,QQLeaf  (Word "reek super nasty")]
                ,QQLeaf (Word "dogs")]
              ,Absent]]
  -- WARNING: assertions added below this line sometimes fail, even though
  -- they work above it. for instance, the following:
  -- assertBool "broken" $ True
