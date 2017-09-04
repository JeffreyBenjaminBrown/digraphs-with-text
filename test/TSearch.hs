module TSearch (tSearch) where

import Dwt
import Data.Graph.Inductive
import TData
import Test.HUnit hiding (Node)
import Data.Set as S hiding (foldl)
import Control.Lens ((.~))

import Data.Either

tSearch = TestList [ TestLabel "tQGet" tQGet
                   , TestLabel "tQGetDe" tQGetDe
                   , TestLabel "tQPutRel" tQPutRel
                   ]

gExtraDog = insLeaf (Word "dog") g1
dogWantsBrandyWords =  QRel (QLeaf $ mkTplt "_ wants _")
  [QLeaf $ Word "dog", QLeaf $ Word "brandy"]
dogBrandyDubious = QRel (QLeaf $ mkTplt "_ is _")
  [dogWantsBrandyWords, QLeaf $ Word "dubious"]
dubiousBackwards = QRel (QLeaf $ mkTplt "_ is _")
  [QLeaf $ Word "dubious", dogWantsBrandyWords]

tQGetDe = TestCase $ do
  assertBool "1" $ qGetDe g1 (QAt 1) == Right [1]
  assertBool "1.1" $ qGet1De g1 (QAt 1) == Right 1
  assertBool "2" $ qGetDe g1 (QAt $ -1) == Right []
  assertBool "2.1" $ let q = (QAt $ -1) in qGet1De g1 q
    == Left (FoundNo, mQNode .~ Just q $ noErrOpts, "qGet1De: .")
  assertBool "3" $ qGetDe g1 (QLeaf $ Word "dog") == Right [0]
  assertBool "3.5" $ qGetDe gExtraDog (QLeaf $ Word "dog") == Right [0,14]
  assertBool "3.5.1" $ let q = QLeaf $ Word "dog" in qGet1De gExtraDog q
    == Left (FoundMany, mQNode .~ Just q $ noErrOpts, "qGet1De: .")
  assertBool "4" $ qGetDe g1 (QRel (QAt 1) [QAt 0, QAt 4]) == Right [5]
  assertBool "4.5" $ qGetDe g1 dogWantsBrandyWords == Right [5]
  assertBool "5" $ qGetDe g1 dogBrandyDubious == Right [11]
  assertBool "5" $ qGetDe g1 dubiousBackwards == Right []

tQGet = let
  qDog = QLeaf $ Word "dog"
  extraDog = insLeaf (Word "dog") g1
  qDogNeedsWater = QRel (QLeaf $ mkTplt "_ needs _")
    [QLeaf $ Word "dog", QLeaf $ Word "water"]
  (g2,_) = fr $ qPut g1 qDog
  
  in TestCase $ do
    assertBool "1" $ qGet g1 qDog == Right [0]
    assertBool "2" $ (S.fromList <$> (qGet g2 qDog))
                  == (S.fromList <$> Right [0]) -- no extra dog!
    assertBool "3" $ (S.fromList <$> (qGet extraDog qDog))
                  == (S.fromList <$> Right [0,14])
    assertBool "4" $ qGet g1 qDogNeedsWater == Right [6]

-- qPut :: RSLT -> QNode -> Either String (RSLT, Node)
tQPutRel = let
  qRedundant = QRel (QLeaf $ mkTplt "_ wants _")
               [QLeaf $ Word "dog", QLeaf $ Word "brandy"]
  qNestedRedundant = QRel (QLeaf $ mkTplt "_ is _")
                     [qRedundant, QAt 10] -- 10 = dubious
  qNovel = QRel (QLeaf $ mkTplt "_ wants _")
           [QLeaf $ Word "dog", QLeaf $ Word "dog"]
  (g2,_) = fr $ qPut g1 qNovel
  in TestCase $ do
  assertBool "1" $ qGet g1 qRedundant == Right [5]
  assertBool "2" $ qGet g1 qNestedRedundant == Right [11]
  assertBool "3" $ qGet g1 qNovel == Right []
  assertBool "4" $ qGet g2 qNovel == Right [14]
