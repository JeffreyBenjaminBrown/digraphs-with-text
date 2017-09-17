{-# LANGUAGE ViewPatterns #-}
module Dwt.Hash.ParseEo () where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Dwt.ParseUtils (Parser, lexeme, parens, phrase
                      , anyWord, word, wordChar, sc)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))
import Text.Megaparsec.Char (satisfy, string, char)

import Data.Graph.Inductive (Node)
import Dwt.Types
import Dwt.Leaf (mkTplt)

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)

type Qeo = (QNode, EO)

-- == Things used when parsing Word and Rel values
-- QNode expresses how to add (nested) data to the RSLT
isInsRel :: Qeo -> Bool
isInsRel (QRel _ _ _, _) = True
isInsRel _ = False

startRel :: Level -> Joint -> Qeo -> Qeo -> Qeo
startRel l j a b = (QRel disregardedEo [j] [fst a, fst b], EO True l)


-- | PITFALL: In "a # b # c # d", you might imagine evaluating the middle #
-- after the others. In that case both sides would be a QRel, and you would
-- want to modify both, rather than make one a member of the other. These
-- concat functions skip that possibility; one of the two QNode arguments is
-- always incorporated into the other. I believe that is safe, because 
-- expressions in serial on the same level will always be parsed left to
-- right, not outside to inside.
rightConcat :: Joint -> Qeo -> Qeo -> Qeo
  -- TODO: if|when need speed, use a two-sided list of pairs
rightConcat j m (QRel _ joints mbrs, eo)
  = (QRel disregardedEo (joints ++ [j]) (mbrs ++ [fst m]), eo)
rightConcat _ _ _ = error "can only rightConcat into a QRel"

leftConcat :: Joint -> Qeo -> Qeo -> Qeo
leftConcat j m (QRel _ joints mbrs, eo)
  = (QRel disregardedEo (j : joints) (fst m : mbrs), eo)
leftConcat _ _ _ = error "can only leftConcat into a QRel"

close :: Qeo -> Qeo
close (QLeaf x, _)         = (QLeaf x, disregardedEo)
close (QRel _ b c, EO _ a) = (QRel disregardedEo b c, EO False a)

