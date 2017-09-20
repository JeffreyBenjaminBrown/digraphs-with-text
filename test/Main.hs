{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Test.HUnit

import TParse
import TAdd
import TGraph
import TShow
import TSearch

main = runTestTT $ TestList [
    TestLabel "tParse"   tParse
  , TestLabel "tAdd"     tAdd
  , TestLabel "tGraph"   tGraph
  , TestLabel "tShow"    tShow
  , TestLabel "tSearch"  tSearch
  ]
