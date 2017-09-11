module TAdd where

import Dwt hiding (fromRight)
import Data.Graph.Inductive
import Test.HUnit hiding (Node)
import Text.Megaparsec (parse)
import Control.Monad.Trans.State (runStateT, execStateT)

tAdd = TestCase $ do
  let Right g = execStateT f empty
      f = mapM (addExprLongErr . fr . parse expr "" ) exprs
      exprs = ["a #", "# a", "a # b", "## b #"]
        -- TODO: (a #) and (# a) are indistinguishable
      qa = QLeaf $ Word "a"
      qb = QLeaf $ Word "b"
      qab = QRel (QLeaf $ mkTplt "_ # _") [qa,qb]
  -- v g $ nodes g -- visual test
  assertBool "1" $ either (const False) (const True)
    $ qGet1LongErr g (QLeaf $ Word "a")
  assertBool "1" $ do
    either (const False) (const True) $ qGet1LongErr g (QLeaf $ Word "a")
