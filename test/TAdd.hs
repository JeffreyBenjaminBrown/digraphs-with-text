module TAdd where

import Dwt
import Test.HUnit hiding (Node)
import Text.Megaparsec (parse)
import Control.Monad.Trans.State (execStateT)


tAdd = TestList [ TestLabel "tAddLabeled" tAddLabeled
                , TestLabel "tAddUnlabeled" tAddUnlabeled
                ]

tAddLabeled = TestCase $ do
  let Right g = execStateT f empty
      f = mapM (addExpr . fr . parse exprSum "" ) exprs
      exprs = ["a #x", "#x a", "a #x b", "##x b #x"]
      qa = QLeaf $ Word "a"
      qb = QLeaf $ Word "b"
      qab = QRel [Joint "x"] [qa,qb]
  assertBool "1" $ either (const False) (const True) $ qGet1 g qa
  assertBool "2" $ do either (const False) (const True) $ qGet1 g qb
  assertBool "3" $ do either (const False) (const True) $ qGet1 g qab

tAddUnlabeled = TestCase $ do
  let Right g = execStateT f empty
      f = mapM (addExpr . fr . parse exprSum "" ) exprs
      exprs = ["a #", "# a", "a # b", "## b #"]
        -- TODO: unlabeled rels (a #) and (# a) are visually indistinguishable
      qa = QLeaf $ Word "a"
      qb = QLeaf $ Word "b"
      qab = QRel [Joint ""] [qa,qb]
  assertBool "1" $ either (const False) (const True) $ qGet1 g qa
  assertBool "2" $ do either (const False) (const True) $ qGet1 g qb
  assertBool "3" $ do either (const False) (const True) $ qGet1 g qab
