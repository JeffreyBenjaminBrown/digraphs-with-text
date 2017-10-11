{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module TSearch (
  -- | = main routine
  tSearch

  -- | = testing qGet and matchRoleMap by hand
  , t2matchRoleMap, t2_qGet
  ) where

import Data.Graph.Inductive
import Dwt
import TData
import Data.Either (isLeft)
import Data.Map as Map
import Data.Maybe as Mb
import Test.HUnit hiding (Node)
import Control.Applicative (liftA2)
import qualified Data.Set as S
import Control.Monad.Trans.State (runStateT)
import Text.Megaparsec (parse)

tSearch = TestList [ TestLabel "tSubExpr" tSubExpr
                   , TestLabel "tPathsToIts" tPathsToIts
                   , TestLabel "tIts" tIts
                   , TestLabel "tQPlaysRoleIn" tQPlaysRoleIn
                   , TestLabel "tMatchQRelspecNodes" tMatchQRelspecNodes
                   , TestLabel "tStar" tStar
                   , TestLabel "tMatchRoleMap" tMatchRoleMap
                   , TestLabel "tQGetBool" tQGetBool
                   , TestLabel "tNestedQRelsWithVars" tNestedQRelsWithVars
                   ]

tPathsToIts = TestCase $ do
  let Right q = parse expr "" "(/it ##is good #for ed ##when ed #is /it)"
  assertBool "1" $ pathsToIts q == Right (S.fromList [[Mbr 1],[Mbr 3,Mbr 2]])

tSubExpr = TestCase $ do
  let Right dubious = qGet1 g1 dogWantsBrandyIsDubious
      Right wants = qGet1 g1 dogWantsBrandy
      Right brandyNode = qGet1 g1 brandy
  assertBool "1" $ subExpr g1 dubious [Mbr 1] == Right wants
  assertBool "2" $ subExpr g1 dubious [Mbr 1, Mbr 2] == Right brandyNode
  assertBool "3" $ isLeft $ subExpr g1 dubious [Mbr 1, Mbr 2, Mbr 1]

tIts = TestCase $ do
  let Right q = parse expr "" "/it #is /any"
      Right is = qGet1 g1 dogWantsBrandyIsDubious
      Right wants = qGet g1 dogWantsBrandy
  assertBool "1" $ its g1 q is == Right wants

tQPlaysRoleIn = TestCase $ do
  assertBool "1" $ qPlaysRoleIn g1 TpltRole needsFor
    == qGet g1 dogNeedsWaterForBrandy
  assertBool "2" $ qPlaysRoleIn g1 (Mbr 1) dogWantsBrandy
    == qGet g1 dogWantsBrandyIsDubious

tMatchQRelspecNodes = TestCase $ do
  assertBool "find dogNeedsWater" $ qGet g1 dogNeedsWater == Right [6]
  assertBool "1" $ matchRoleMap g1 anyNeedsWaterRM
    == qGet g1 dogNeedsWater
  assertBool "2" $ matchRoleMap g1 tpltForRelsWithDogInPos1RM
    == Right [5,6,8]

tMatchRoleMap = TestCase $ do
  assertBool "find a rolemap" $ matchRoleMap g1 anyNeedsFromForToRM
    == Right [8]
  assertBool "2" $ matchRoleMap g1 dogNeedsFromForToRM
    == Right [8]
  assertBool "3" $ matchRoleMap g1 brandyNeedsFromForToRM
    == Right []

-- testing qGet and matchRoleMap by hand
t2matchRoleMap :: RSLT -> RoleMap -> Either DwtErr [(RelRole, Maybe [Node])]
t2matchRoleMap g m = prependCaller "matchRoleMap: " $ do
  let maybeFind :: (RelRole, QNode) -> Either DwtErr (RelRole, Maybe [Node])
      maybeFind (r, QVar _) = Right (r, Nothing)
      maybeFind (r, q) = (\ns -> (r, Just ns)) <$> qPlaysRoleIn g r q
  mapM maybeFind $ Map.toList m

t2_qGet :: RSLT -> QNode
  -> Either DwtErr (RoleMap, [(RelRole, Maybe [Dwt.Node])])
t2_qGet g q@(QRel False _ qms) = do
  t <- extractTplt q
  let m = mkRoleMap (QLeaf t) qms
  (m,) <$> t2matchRoleMap g m

tQGetBool = TestCase $ do
  assertBool "1" $ qGet g1 (QAnd [dogNeedsAny,anyNeedsWater])
                == qGet g1 dogNeedsWater
  assertBool "1" $
    (S.fromList <$> (qGet g1 $ QOr [dogNeedsWater,dogWantsBrandy] ) )
    == (S.fromList <$> liftA2 (++) (qGet g1 dogNeedsWater)
                                   (qGet g1 dogWantsBrandy) )

tNestedQRelsWithVars = TestCase $ do
  assertBool "no dubious needs" $ qGet g1 anyNeedsAnyIsAny == Right []
  assertBool "a dubious want" $ qGet g1 anyWantsAnyIsAny == Right [11]

tStar = TestCase $ do
  let Right (dogWaterChoco,g2)
        = flip runStateT g1 ( qPutSt $ QRel False ["needs","for"]
                                       [ QLeaf $ Word "Laura"
                                       , QLeaf $ Word "water"
                                       , QLeaf $ Word "chocolate"] )
  assertBool "star treats It the same as Any" $
    (fmap S.fromList $ star g2 anyNeedsFromForToRM $ QLeaf $ Word "water")
    == (fmap S.fromList $ star g2 itNeedsFromForToRM $ QLeaf $ Word "water")
  assertBool "any matches Laura and dog" $
    (fmap S.fromList $ star g2 anyNeedsFromForToRM $ QLeaf $ Word "water")
    == (fmap S.fromList $ liftA2 (++)
                          (qGet g2 $ QLeaf $ Word "brandy")
                          (qGet g2 $ QLeaf $ Word "chocolate") )
  assertBool "but dog only matches dog" $
    (star g2 dogNeedsFromForToRM $ QLeaf $ Word "water")
    == (qGet g2 $ QLeaf $ Word "brandy")
