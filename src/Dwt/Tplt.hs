{-# LANGUAGE ViewPatterns #-}

module Dwt.Tplt where

import Text.Megaparsec
import Dwt.ParseUtils (Parser, anyWord, lexeme, parens, phrase, word, sc)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))
import Text.Megaparsec.Char (satisfy, string, char)

import Data.Graph.Inductive (Node)
import Dwt.Types
import Dwt.Leaf (mkTplt)

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)

type TpltExplicit = [Maybe String]
type PTplt = [PTpltUnit]
data PTpltUnit = Blank | Phrase String deriving Show

mkTpltExplicit :: PTplt -> TpltExplicit
mkTpltExplicit = map f where f Blank = Nothing
                             f (Phrase p) = Just p

dropNonExtremeBlanks :: PTplt -> PTplt
dropNonExtremeBlanks us = map snd $ filter f zs
  where zs = zip [1..] us
        f (_, Phrase _) = True
        f (1,Blank) = True
        f ((== length us) -> True, Blank) = True
        f (_,Blank) = False

pTplt :: Parser  PTplt
pTplt = many (pBlank <|> pPhrase) where
  pBlank, pPhrase :: Parser PTpltUnit
  pBlank = const Blank <$> lexeme (char '_')
  pPhrase = Phrase <$> phrase


