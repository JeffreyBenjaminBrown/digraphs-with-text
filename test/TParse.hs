{-# LANGUAGE OverloadedStrings #-}

module TParse where

import Dwt
import Test.HUnit
import Text.Megaparsec (parse)


tParse = TestList [ TestLabel "tParseInner" tParseInner
                  ]

tParseInner = TestList [ TestLabel "tHash" tHash
                       ]

tHash = TestCase $ do
  assertBool "4" $ either (const True) (const False) $ parse exprSum "" ")("

  assertBool "1"
    $ hash 2 ("") (QLeaf $ Word "hi", disregardedEo)
                  (QLeaf $ Word "there", disregardedEo)
    == (QRel [""] [(QLeaf $ Word "hi")
                                ,(QLeaf $ Word "there")]
       , EO True 2)

  assertBool "2" $
    rightConcat ("") (QLeaf $ Word "na", disregardedEo)
    (QRel ["zaba"] [(QLeaf $ Word "left")
                                 ,(QLeaf $ Word "right")]
    , EO True 3)
    == (QRel ["zaba",""] [(QLeaf $ Word "left")
                                        ,(QLeaf $ Word "right")
                                        ,(QLeaf $ Word "na")]
       , EO True 3)

  assertBool "3"
    $ leftConcat ("new") (QLeaf $ Word "new", disregardedEo)
      (QRel ["j"] [(QLeaf $ Word "left")
                                ,(QLeaf $ Word "right")]
      , EO True 4)
    == (QRel ["new","j"]
        [(QLeaf $ Word "new")
        ,(QLeaf $ Word "left")
        ,(QLeaf $ Word "right")]
       , EO True 4)

  assertBool "longParse" $ longParse == Right bigQNode
  assertBool "smallerParse" $ smallerParse == Right smallerQNode

smallerParse = parse exprSum ""
  "you #(give or misplace) bones that reek nasty #to dogs"
smallerQNode = QRel ["give or misplace", "to"] 
                    [ QLeaf (Word "you")
                     , QLeaf $ Word "bones that reek nasty"
                     , QLeaf (Word "dogs")]

longParse = parse exprSum ""
  "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
bigQNode = QRel ["becase"] 
             [ QRel ["like"] 
               [ QLeaf (Word "dogs")
               , QLeaf (Word "you")]
             , QRel ["sometimes"] 
               [QRel ["give or misplace", "to"] 
                 [ QLeaf (Word "you")
                 , QRel ["that"] 
                   [ QLeaf (Word "bones")
                   , QLeaf  (Word "reek super nasty")]
                 , QLeaf (Word "dogs")]
               , Absent]]
