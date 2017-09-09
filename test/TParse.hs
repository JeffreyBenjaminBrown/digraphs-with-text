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
    hash 2 ("") (LeafX $ Word "hi") (LeafX $ Word "there")
    == RelX (EO True 2) [""] [(LeafX $ Word "hi")
                                    ,(LeafX $ Word "there")]

  assertBool "2" $
    rightConcat ("") (LeafX $ Word "na")
    (RelX (EO True 3) ["zaba"] [(LeafX $ Word "left")
                                      ,(LeafX $ Word "right")])
    == RelX (EO True 3) ["zaba",""] [(LeafX $ Word "left")
                                        ,(LeafX $ Word "right")
                                        ,(LeafX $ Word "na")]

  assertBool "3"
    $ leftConcat ("new") (LeafX $ Word "new")
      (RelX (EO True 4) ["j"] [(LeafX $ Word "left")
                                    ,(LeafX $ Word "right")])
    == RelX (EO True 4) ["new","j"]
      [(LeafX $ Word "new"),(LeafX $ Word "left"),(LeafX $ Word "right")]
  assertBool "4" $ p == Right a where
      p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = RelX 
            (EO True 3) 
            ["becase"] 
            [RelX 
              (EO True 1) 
              ["like"] 
              [LeafX (Word "dogs")
              ,LeafX (Word "you")]
            ,RelX 
              (EO True 2) 
              ["sometimes"] 
              [RelX 
                (EO True 1) 
                ["give or misplace", "to"] 
                [LeafX (Word "you")
                ,RelX 
                  (EO False 1) 
                  ["that"] 
                  [LeafX (Word "bones")
                  ,LeafX  (Word "reek super nasty")]
                ,LeafX (Word "dogs")]
              ,Absent]]
  -- WARNING: assertions added below this line sometimes fail, even though
  -- they work above it. for instance, the following:
  -- assertBool "broken" $ True
