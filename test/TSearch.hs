module TSearch where

import Dwt
import Data.Graph.Inductive
import TData
import Test.HUnit hiding (Node)
import Data.Set as S hiding (foldl)

import Data.Either

tSearch = TestList [ TestLabel "tQGet" tQGet
                   , TestLabel "tQPutRel" tQPutRel
                   ]

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
