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
  assertBool "4" $ either (const True) (const False) $ parse expr "" ")("

  assertBool "1"
    $ hash 2 ("") (HashNonRel $ QLeaf $ Word "hi")
                  (HashNonRel $ QLeaf $ Word "there")
    == (Hash (EO True 2) $ QRel False [""] [(QLeaf $ Word "hi")
                                     ,(QLeaf $ Word "there")] )

  assertBool "2" $
    rightConcat ("") (HashNonRel $ QLeaf $ Word "na")
    (Hash (EO True 3) $ QRel False ["zaba"] [(QLeaf $ Word "left")
                                      ,(QLeaf $ Word "right")] )
    == (Hash (EO True 3) $ QRel False ["zaba",""] [(QLeaf $ Word "left")
                                            ,(QLeaf $ Word "right")
                                            ,(QLeaf $ Word "na")] )

  assertBool "3"
    $ leftConcat ("new") (HashNonRel $ QLeaf $ Word "new")
      (Hash (EO True 4) $ QRel False ["j"] [(QLeaf $ Word "left")
                                     ,(QLeaf $ Word "right")] )
    == (Hash (EO True 4) $ QRel False ["new","j"]
                           [(QLeaf $ Word "new")
                           ,(QLeaf $ Word "left")
                           ,(QLeaf $ Word "right")] )

  assertBool "longParse" $ longParse == Right bigQNode
  assertBool "smallerParse" $ smallerParse == Right smallerQNode

smallerParse = parse expr ""
  "you #(give or misplace) bones that reek nasty #to dogs"
smallerQNode = QRel False ["give or misplace", "to"] 
                    [ QLeaf (Word "you")
                     , QLeaf $ Word "bones that reek nasty"
                     , QLeaf (Word "dogs")]

longParse = parse expr ""
  "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
bigQNode = QRel False ["becase"] 
             [ QRel False ["like"] 
               [ QLeaf (Word "dogs")
               , QLeaf (Word "you")]
             , QRel False ["sometimes"] 
               [QRel False ["give or misplace", "to"] 
                 [ QLeaf (Word "you")
                 , QRel False ["that"] 
                   [ QLeaf (Word "bones")
                   , QLeaf  (Word "reek super nasty")]
                 , QLeaf (Word "dogs")]
               , Absent]]
