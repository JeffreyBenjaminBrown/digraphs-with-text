module TSearch (tSearch) where

import Dwt
import Data.Graph.Inductive
import TData
import Test.HUnit hiding (Node)
import qualified Data.Set as S
import Control.Lens ((.~))

import Data.Either

tSearch = TestList [ 
                     TestLabel "tQPut" tQPut
                   , TestLabel "tQGet" tQGet
                   , TestLabel "tQPutRel" tQPutRel
                   ]

-- some of these are redundant but eh
gExtraDog = insLeaf (Word "dog") g1
dogWantsBrandyWords =  QRel (QLeaf $ mkTplt "_ wants _")
  [QLeaf $ Word "dog", QLeaf $ Word "brandy"]
dogBrandyDubious = QRel (QLeaf $ mkTplt "_ is _")
  [dogWantsBrandyWords, QLeaf $ Word "dubious"]
dubiousBackwards = QRel (QLeaf $ mkTplt "_ is _")
  [QLeaf $ Word "dubious", dogWantsBrandyWords]

qDog = QLeaf $ Word "dog"
qCat = QLeaf $ Word "cat"
qBrandy = QLeaf $ Word "brandy"
qWants = QLeaf $ mkTplt "_ wants _"
qDogWantsBrandy =  QRel qWants [qDog, qBrandy]

tQPut = TestCase $ do
  assertBool "1" $ qPut empty qDog == Right (mkGraph [(0,Word "dog")] [], 0)
  assertBool "2" $ let Right (_, n) = qPut g1 qCat in n == 14
  assertBool "3" $ let Right (_, n) = qPut g1 qDogWantsBrandy in n==5
  assertBool "4" $ let Right (g,n) = qPut empty qDogWantsBrandy in n > (-1)
--  assertBool "4" $ qPutStrErr empty qDogWantsBrandy
--    == Right (mkGraph [(0, Word "dog"), (1, word "brandy"), (2, mkTplt "_ wants _"), (3, Rel)] [], 3)

tQGet = TestCase $ do
  assertBool "1" $ qGet g1 (QAt 1) == Right [1]
  assertBool "1.1" $ qGet1De g1 (QAt 1) == Right 1
  assertBool "2" $ qGet g1 (QAt $ -1) == Right []
  assertBool "2.1" $ let q = (QAt $ -1) in qGet1De g1 q
    == Left (FoundNo, mQNode .~ Just q $ noErrOpts, "qGet1De: .")
  assertBool "3" $ qGet g1 (QLeaf $ Word "dog") == Right [0]
  assertBool "3.5" $ qGet gExtraDog (QLeaf $ Word "dog") == Right [0,14]
  assertBool "3.5.1" $ let q = QLeaf $ Word "dog" in qGet1De gExtraDog q
    == Left (FoundMany, mQNode .~ Just q $ noErrOpts, "qGet1De: .")
  assertBool "4" $ qGet g1 (QRel (QAt 1) [QAt 0, QAt 4]) == Right [5]
  assertBool "4.5" $ qGet g1 dogWantsBrandyWords == Right [5]
  assertBool "5" $ qGet g1 dogBrandyDubious == Right [11]
  assertBool "5" $ qGet g1 dubiousBackwards == Right []

tQPutRel = let
  qRedundant = QRel (QLeaf $ mkTplt "_ wants _")
               [QLeaf $ Word "dog", QLeaf $ Word "brandy"]
  qNestedRedundant = QRel (QLeaf $ mkTplt "_ is _")
                     [qRedundant, QAt 10] -- 10 = dubious
  qNovel = QRel (QLeaf $ mkTplt "_ wants _")
           [QLeaf $ Word "dog", QLeaf $ Word "dog"]
  (g2,_) = fr $ qPutStrErr g1 qNovel
  in TestCase $ do
  assertBool "1" $ qGet g1 qRedundant == Right [5]
  assertBool "2" $ qGet g1 qNestedRedundant == Right [11]
  assertBool "3" $ qGet g1 qNovel == Right []
  assertBool "4" $ qGet g2 qNovel == Right [14]
