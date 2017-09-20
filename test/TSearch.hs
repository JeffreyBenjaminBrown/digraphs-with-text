{-# LANGUAGE OverloadedStrings #-}
module TSearch (tSearch) where

import Data.Graph.Inductive
import Dwt
import TData
import Data.Map as M
import Test.HUnit

tSearch = TestList [ TestLabel "tQPlaysRoleIn" tQPlaysRoleIn]

tQPlaysRoleIn = TestCase $ do
  let qr = M.fromList
        [(TpltRole, QNodeSpec $ QLeaf $ mkTplt "_ needs _ for ")
        ,(Mbr 1, QVarSpec Any)
        ,(Mbr 2, QVarSpec From)
        ,(Mbr 3, QVarSpec To)]
      needsFor = QLeaf $ mkTplt "_ needs _ for _"
      dog = QLeaf $ Word "dog"
      water = QLeaf $ Word "water"
      brandy = QLeaf $ Word "brandy"
      dogNeedsWaterForBrandy = At 8
      dogWantsBrandy = QRel ["wants"] [dog,brandy]
      dogWantsBrandyIsDubious = At 11
  assertBool "1" $ qPlaysRoleIn g1 needsFor TpltRole
    == qGet g1 dogNeedsWaterForBrandy
  assertBool "2" $ qPlaysRoleIn g1 dogWantsBrandy (Mbr 1)
    == qGet g1 dogWantsBrandyIsDubious
