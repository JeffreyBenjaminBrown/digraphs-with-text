{-# LANGUAGE ViewPatterns #-}

module Dwt.Tplt (
  -- Tplt, involve no RSLT
  _splitStringForTplt
  , mkTplt
  , subInTplt
  , padTpltStrings
  , subInTpltWithHashes

  , _splitStringForTpltMB
  , pTplt
  ) where

import Dwt.Types
import Dwt.Util (fr)
import Data.Text (pack, unpack, strip, splitOn)
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

_splitStringForTplt :: String -> [String]
_splitStringForTplt = map (maybe "" id) . _splitStringForTpltMB

mkTplt :: String -> Expr
mkTplt = Tplt
  . map (unpack . strip . pack)
  . _splitStringForTplt

subInTpltWithHashes :: Expr      -- must be a Tplt
                     -> [String] -- members for the Tplt
                     -> Int      -- relationship level = number of #s
                     -> String
  -- todo ? test length (should match arity), use Either
  -- todo ? test each tplt-string; if has space, wrap in parens
subInTpltWithHashes (Tplt ts) ss prefixCount =
  let ts' = padTpltStrings (Tplt ts)
          $ replicate (2^prefixCount) '#'
      pairList = zip ts' $ ss ++ [""]
       -- append "" because there are n+1 segments in an n-ary Tplt; 
         -- zipper ends early otherwise
  in foldl (\s (a,b) -> s++a++b) "" pairList
subInTpltWithHashes _ _ _ = error "subInTplt: not a Tplt" -- todo ? omit

subInTplt :: Expr -> [String] -> String
subInTplt (Tplt ts) ss = subInTpltWithHashes (Tplt ts) ss 0

padTpltStrings :: Expr -> String -> [String]
padTpltStrings (Tplt ss) prefix =
  let a = head ss
      z = last ss
      middle = reverse $ tail $ reverse $ tail ss
      f s = if elem ' ' s then '(' : (s ++ ")") else s
      doToMiddle s = " " ++ prefix ++ f s ++ " "
      doToFirst s = case s of "" -> ""
                              _ -> prefix ++ f s ++ " "
      doToLast  s = case s of "" -> ""
                              _ -> " " ++ prefix ++ f s
  in [doToFirst a] ++ map doToMiddle middle ++ [doToLast z]
