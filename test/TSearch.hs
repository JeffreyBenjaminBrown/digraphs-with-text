{-# LANGUAGE OverloadedStrings #-}
module TSearch (tSearch) where

import Data.Graph.Inductive
import Dwt
import TData
import Data.Map as M
import Test.HUnit

tSearch = TestList [ TestLabel "tQPlaysRoleIn" tQPlaysRoleIn
                   , TestLabel "tMatchQRelSpecNodes" tMatchQRelSpecNodes
                   ]

tQPlaysRoleIn = TestCase $ do
  assertBool "1" $ qPlaysRoleIn g1 needsFor TpltRole
    == qGet g1 dogNeedsWaterForBrandy
  assertBool "2" $ qPlaysRoleIn g1 dogWantsBrandy (Mbr 1)
    == qGet g1 dogWantsBrandyIsDubious

tMatchQRelSpecNodes = TestCase $ do
  assertBool "find dogNeedsWater" $ qGet g1 dogNeedsWater == Right [6]
  assertBool "1" $ matchQRelSpecNodes g1 anyNeedsWater
    == qGet g1 dogNeedsWater
  assertBool "2" $ matchQRelSpecNodes g1 tpltForRelsWithDogInPos1
    == Right [5,6,8]

-- tStar = TestCase $ do
-- runStateT (qPutSt $ QRel ["needs","for"] [QLeaf $ Word "dog", QLeaf $ Word "water", QLeaf $ Word "chocolate"]) g1
