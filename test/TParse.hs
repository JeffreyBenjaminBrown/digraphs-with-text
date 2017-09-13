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
    hash 2 ("") (InsLeaf $ Word "hi") (InsLeaf $ Word "there")
    == InsRel (EO True 2) [""] [(InsLeaf $ Word "hi")
                                    ,(InsLeaf $ Word "there")]

  assertBool "2" $
    rightConcat ("") (InsLeaf $ Word "na")
    (InsRel (EO True 3) ["zaba"] [(InsLeaf $ Word "left")
                                      ,(InsLeaf $ Word "right")])
    == InsRel (EO True 3) ["zaba",""] [(InsLeaf $ Word "left")
                                        ,(InsLeaf $ Word "right")
                                        ,(InsLeaf $ Word "na")]

  assertBool "3"
    $ leftConcat ("new") (InsLeaf $ Word "new")
      (InsRel (EO True 4) ["j"] [(InsLeaf $ Word "left")
                                    ,(InsLeaf $ Word "right")])
    == InsRel (EO True 4) ["new","j"]
      [(InsLeaf $ Word "new"),(InsLeaf $ Word "left"),(InsLeaf $ Word "right")]
  assertBool "4" $ p == Right a where
      p = parse expr "uhh ... parse error?"
        "dogs #like you ###becase you #(give or misplace) (bones #that reek super nasty) #to dogs ##sometimes"
      a = InsRel 
            (EO True 3) 
            ["becase"] 
            [InsRel 
              (EO True 1) 
              ["like"] 
              [InsLeaf (Word "dogs")
              ,InsLeaf (Word "you")]
            ,InsRel 
              (EO True 2) 
              ["sometimes"] 
              [InsRel 
                (EO True 1) 
                ["give or misplace", "to"] 
                [InsLeaf (Word "you")
                ,InsRel 
                  (EO False 1) 
                  ["that"] 
                  [InsLeaf (Word "bones")
                  ,InsLeaf  (Word "reek super nasty")]
                ,InsLeaf (Word "dogs")]
              ,Absent]]
  -- WARNING: assertions added below this line sometimes fail, even though
  -- they work above it. for instance, the following:
  -- assertBool "broken" $ True
