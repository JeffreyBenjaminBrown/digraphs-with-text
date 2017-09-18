{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Dwt.Hash.Parse (
  expr

  -- for tests, not interface
  , hash, rightConcat, leftConcat
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char (satisfy, string, char)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))

import Data.Graph.Inductive (Node)
import Dwt.ParseUtils (Parser, lexeme, parens, sc
                      , integer
                      , anyWord, word, wordChar, phrase)
import Dwt.Types
import Dwt.Tplt (mkTplt)

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)

type Qeo = (QNode, EO)

-- == Things used when parsing Word and Rel values
-- QNode expresses how to add (nested) data to the RSLT
isInsRel :: Qeo -> Bool
isInsRel (QRel _ _, _) = True
isInsRel _ = False

startRel :: Level -> Joint -> Qeo -> Qeo -> Qeo
startRel l j a b = (QRel [j] [fst a, fst b], EO True l)


-- | PITFALL: In "a # b # c # d", you might imagine evaluating the middle #
-- after the others. In that case both sides would be a QRel, and you would
-- want to modify both, rather than make one a member of the other. These
-- concat functions skip that possibility; one of the two QNode arguments is
-- always incorporated into the other. I believe that is safe, because 
-- expressions in serial on the same level will always be parsed left to
-- right, not outside to inside.
rightConcat :: Joint -> Qeo -> Qeo -> Qeo
  -- TODO: if|when need speed, use a two-sided list of pairs
rightConcat j m (QRel joints mbrs, eo)
  = (QRel (joints ++ [j]) (mbrs ++ [fst m]), eo)
rightConcat _ _ _ = error "can only rightConcat into a QRel"

leftConcat :: Joint -> Qeo -> Qeo -> Qeo
leftConcat j m (QRel joints mbrs, eo)
  = (QRel (j : joints) (fst m : mbrs), eo)
leftConcat _ _ _ = error "can only leftConcat into a QRel"

close :: Qeo -> Qeo
close (QLeaf x, _)         = (QLeaf x, disregardedEo)
close (QRel b c, EO _ a) = (QRel b c, EO False a)


hash :: Level -> Joint -> Qeo -> Qeo -> Qeo
hash l j a@(isInsRel -> False) b@(isInsRel -> False)       = startRel l j a b
hash l j a@(isInsRel -> False) b@(QRel _ _, EO False _) = startRel l j a b
hash l j a@(QRel _ _, EO False _) b@(isInsRel -> False) = startRel l j a b
hash l j a@(isInsRel -> False) b@(QRel _ _, EO True l')
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = leftConcat j a b -- I suspect this won't happen either
  | l > l' = startRel l j a b
hash l j a@(QRel _ _, EO True l') b@(isInsRel -> False)
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = rightConcat j b a -- but this will
  | l > l' = startRel l j a b
hash l j a@(QRel _ _, ea) b@(QRel _ _, eb) =
  let e = EO True l
      msg = unlines [ "Joint should have been evaluated earlier."
                    , "level: " ++ show l
                    , "joint: " ++ show j
                    , "left: " ++ show a
                    , "right: " ++ show b ]
  in if e <= eb then error msg
     else if e > ea then startRel l j a b
     else if e == ea then rightConcat j b a
     else error msg


-- == the QNode parser
expr :: Parser QNode
expr = fst <$> expr'

expr' :: Parser Qeo
expr' = makeExprParser term
  [ [InfixL $ try $ pHash n] | n <- [1..8] ]

term :: Parser Qeo
term = (, disregardedEo) . QLeaf <$> leaf
       <|> (, disregardedEo) <$> at
       <|> close <$> parens expr'
       <|> absent where
  absent :: Parser Qeo
  absent = (, disregardedEo) . const Absent <$> f
    <?> "Intended to \"find\" nothing."
  f = lookAhead $ const () <$> satisfy (== '#') <|> eof
    -- the Absent parser should look for #, but not ), because
    -- parentheses get consumed in pairs in an outer (earlier) context

pHashUnlabeled :: Int -> Parser ()
pHashUnlabeled n = const () <$> f
  where f = string (replicate n '#') <* notFollowedBy (char '#')

pHash :: Int -> Parser (Qeo -> Qeo -> Qeo)
pHash n = lexeme $ do
  pHashUnlabeled n
  label <- option "" $ anyWord <|> parens phrase
  return $ hash n $ Joint label

leaf :: Parser Expr
leaf = do
  let blank = lexeme $ string "_" <* notFollowedBy (wordChar <|> char '_')
      f = concat . intersperse " " 
  p <- some $ blank <|> anyWord
  return $ case elem "_" p of True ->  mkTplt . f $ p
                              False -> Word   . f $ p

at :: Parser QNode
at = At <$> (char '@' >> integer)

-- | unused
hasBlanks :: Parser Bool
hasBlanks = (>0) . length . concat <$> (sc *> (many $ blank <|> other))
  where blank, other :: Parser String  -- order across the <|> matters
        blank = try $ word "_"
        other = const "" <$> anyWord
