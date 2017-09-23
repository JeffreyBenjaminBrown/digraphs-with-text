{-# LANGUAGE OverloadedStrings #-}
module TSearch (tSearch) where

import Data.Graph.Inductive
import Dwt
import TData
import Data.Map as M
import Test.HUnit
import Control.Applicative (liftA2)
import qualified Data.Set as S
import Control.Monad.Trans.State (runStateT)

tSearch = TestList [ TestLabel "tQPlaysRoleIn" tQPlaysRoleIn
                   , TestLabel "tMatchQRelspecNodes" tMatchQRelspecNodes
                   , TestLabel "tStar" tStar
                   , TestLabel "tMatchRoleMap" tMatchRoleMap
                   ]

tQPlaysRoleIn = TestCase $ do
  assertBool "1" $ qPlaysRoleIn g1 TpltRole needsFor
    == qGet g1 dogNeedsWaterForBrandy
  assertBool "2" $ qPlaysRoleIn g1 (Mbr 1) dogWantsBrandy
    == qGet g1 dogWantsBrandyIsDubious

tMatchQRelspecNodes = TestCase $ do
  assertBool "find dogNeedsWater" $ qGet g1 dogNeedsWater == Right [6]
  assertBool "1" $ matchQRelspecNodes g1 anyNeedsWater
    == qGet g1 dogNeedsWater
  assertBool "2" $ matchQRelspecNodes g1 tpltForRelsWithDogInPos1
    == Right [5,6,8]

tMatchRoleMap = TestCase $ do
  assertBool "find a rolemap" $ matchRoleMap g1 anyNeedsFromForToRM
    == Right [8]
  assertBool "2" $ matchRoleMap g1 dogNeedsFromForToRM
    == Right [8]
  assertBool "3" $ matchRoleMap g1 brandyNeedsFromForToRM
    == Right []

-- >>>
--tNestedRelspec = TestCase $ do
  --qGet g1 anyNeedsAnyIsAny

tStar = TestCase $ do
  let Right (dogWaterChoco,g2)
        = flip runStateT g1 ( qPutSt $ QRel ["needs","for"]
                                       [ QLeaf $ Word "Laura"
                                       , QLeaf $ Word "water"
                                       , QLeaf $ Word "chocolate"] )
  assertBool "star treats It the same as Any" $
    (fmap S.fromList $ star g2 (QLeaf $ Word "water") anyNeedsFromForTo)
    == (fmap S.fromList $ star g2 (QLeaf $ Word "water") itNeedsFromForTo)
  assertBool "any matches Laura and dog" $
    (fmap S.fromList $ star g2 (QLeaf $ Word "water") anyNeedsFromForTo)
    == (fmap S.fromList $ liftA2 (++)
                          (qGet g2 $ QLeaf $ Word "brandy")
                          (qGet g2 $ QLeaf $ Word "chocolate") )
  assertBool "but dog only matches dog" $
    (star g2 (QLeaf $ Word "water") dogNeedsFromForTo)
    == (qGet g2 $ QLeaf $ Word "brandy")

