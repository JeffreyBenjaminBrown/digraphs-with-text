{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.HUnit

import Dwt
import TData
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
