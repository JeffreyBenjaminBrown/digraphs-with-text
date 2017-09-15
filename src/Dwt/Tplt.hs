{-# LANGUAGE ViewPatterns #-}

module Dwt.Tplt (
  _splitStringForTpltMB
  , pTplt
  )where

import Dwt.Types
import Dwt.Util (fr)

import Text.Megaparsec
import Dwt.ParseUtils (Parser, anyWord, lexeme, parens, phrase, word, sc)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))
import Text.Megaparsec.Char (satisfy, string, char)


data PTpltUnit = Blank | Phrase String deriving Show

_splitStringForTpltMB :: String -> [Maybe String]
_splitStringForTpltMB = fr . parse pTplt ""

pTplt :: Parser [Maybe String]
pTplt = map toMaybeString . dropNonExtremeBlanks
        <$> some (pBlank <|> pPhrase)
  where pBlank, pPhrase :: Parser PTpltUnit
        pBlank = const Blank <$> lexeme (char '_')
        pPhrase = Phrase <$> phrase

dropNonExtremeBlanks :: [PTpltUnit] -> [PTpltUnit]
dropNonExtremeBlanks us = map snd $ filter f zs
  where zs = zip [1..] us
        f (_, Phrase _) = True
        f (1,Blank) = True
        f ((== length us) -> True, Blank) = True
        f (_,Blank) = False

toMaybeString :: PTpltUnit -> Maybe String
toMaybeString Blank = Nothing
toMaybeString (Phrase p) = Just p
