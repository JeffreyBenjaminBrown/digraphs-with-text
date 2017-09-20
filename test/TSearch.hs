{-# LANGUAGE OverloadedStrings #-}
module TSearch (tSearch) where

import Data.Graph.Inductive
import Dwt
import TData
import Data.Map as M
import Test.HUnit

tSearch = TestList [ TestLabel "tQPlaysRoleIn" tQPlaysRoleIn]

tQPlaysRoleIn = TestCase $ do
  assertBool "1" $ qPlaysRoleIn g1 needsFor TpltRole
    == qGet g1 dogNeedsWaterForBrandy
  assertBool "2" $ qPlaysRoleIn g1 dogWantsBrandy (Mbr 1)
    == qGet g1 dogWantsBrandyIsDubious
