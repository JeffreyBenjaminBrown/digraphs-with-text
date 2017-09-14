module TAdd where

import Dwt hiding (fromRight)
import Dwt.Types (EO(..), blankEo)
import Data.Graph.Inductive
import Test.HUnit hiding (Node)
import Text.Megaparsec (parse)
import Control.Monad.Trans.State (runStateT, execStateT)

tAdd = TestList [ TestLabel "tAddLabeled" tAddLabeled
                , TestLabel "tAddUnlabeled" tAddUnlabeled
                ]

tAddLabeled = TestCase $ do
  let Right g = execStateT f empty
      f = mapM (addExpr . fr . parse expr "" ) exprs
      exprs = ["a #x", "#x a", "a #x b", "##x b #x"]
      qa = InsLeaf $ Word "a"
      qb = InsLeaf $ Word "b"
      qab = InsRel blankEo (mkJoints "_ x _") [qa,qb]
  assertBool "1" $ either (const False) (const True) $ qGet1XX g qa
  assertBool "2" $ do either (const False) (const True) $ qGet1XX g qb
  assertBool "3" $ do either (const False) (const True) $ qGet1XX g qab

tAddUnlabeled = TestCase $ do
  let Right g = execStateT f empty
      f = mapM (addExpr . fr . parse expr "" ) exprs
      exprs = ["a #", "# a", "a # b", "## b #"]
        -- TODO: unlabeled rels (a #) and (# a) are visually indistinguishable
      qa = InsLeaf $ Word "a"
      qb = InsLeaf $ Word "b"
      qab = InsRel blankEo (mkJoints "_ _") [qa,qb]
  assertBool "1" $ either (const False) (const True) $ qGet1XX g qa
  assertBool "2" $ do either (const False) (const True) $ qGet1XX g qb
  assertBool "3" $ do either (const False) (const True) $ qGet1XX g qab
