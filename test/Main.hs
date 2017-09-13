{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Test.HUnit

import TParse
import TGraph
import TShow
import TAdd

main = runTestTT $ TestList
  [
    TestLabel "tParse"   tParse
  , TestLabel "tAdd" tAdd
  , TestLabel "tGraph"   tGraph
  , TestLabel "tView"    tView
  ]
