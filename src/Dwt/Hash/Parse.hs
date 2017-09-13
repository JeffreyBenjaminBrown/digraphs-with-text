-- From an expression like "I #like turtles ##when the sun #is big", creates a set of instructions for adding expressions to a RSLT.
-- discussion: https://www.reddit.com/r/haskell/comments/6v9b13/can_this_problem_be_approached_from_the_bottomup/
{-# LANGUAGE ViewPatterns #-}

module Dwt.Hash.Parse where

import Data.Graph.Inductive (Node)
import Dwt.Types
import Dwt.Leaf (mkTplt)

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


-- == Things used when parsing Tplt values
hasBlanks :: String -> Either  (ParseError (Token String) Void) Bool
hasBlanks = parse p "not a file"
  where p :: Parser Bool
        p = (>0) . length . concat
            <$> (sc *> (many $ blank <|> other)) -- order across <|> matters
        blank :: Parser String
        blank = try $ word "_"
        other :: Parser String
        other = const "" <$> anyWord


-- == Things used when parsing Word and Rel values
-- Insertion expresses how to add (nested) data to the RSLT
isInsRel :: Insertion -> Bool
isInsRel (InsRel _ _ _) = True
isInsRel _ = False

startRel :: Level -> JointX -> Insertion -> Insertion -> Insertion
startRel l j a b = InsRel (EO True l) [j] [a,b]

-- | PITFALL: In "a # b # c # d", you might imagine evaluating the middle #
-- after the others. In that case both sides would be a InsRel, and you would
-- want to modify both, rather than make one a member of the other. These
-- concat functions skip that possibility; one of the two Insertion arguments is
-- always incorporated into the other. I believe that is safe, because 
-- expressions in serial on the same level will always be parsed left to
-- right, not outside to inside.
rightConcat :: JointX -> Insertion -> Insertion -> Insertion
  -- TODO: if|when need speed, use a two-sided list of pairs
rightConcat j m (InsRel eo joints mbrs)
  = InsRel eo (joints ++ [j]) (mbrs ++ [m])
rightConcat _ _ _ = error "can only rightConcat into a InsRel"

leftConcat :: JointX -> Insertion -> Insertion -> Insertion
leftConcat j m (InsRel eo joints mbrs)
  = InsRel eo (j : joints) (m : mbrs)
leftConcat _ _ _ = error "can only leftConcat into a InsRel"

close :: Insertion -> Insertion
close (InsLeaf x) = InsLeaf x
close (InsRel (EO _     a) b c)
     = InsRel (EO False a) b c

hash :: Level -> JointX -> Insertion -> Insertion -> Insertion
hash l j a@(isInsRel -> False) b@(isInsRel -> False)       = startRel l j a b
hash l j a@(isInsRel -> False) b@(InsRel (EO False _) _ _) = startRel l j a b
hash l j a@(InsRel (EO False _) _ _) b@(isInsRel -> False) = startRel l j a b
hash l j a@(isInsRel -> False) b@(InsRel (EO True l') _ _)
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = leftConcat j a b -- I suspect this won't happen either
  | l > l' = startRel l j a b
hash l j a@(InsRel (EO True l') _ _) b@(isInsRel -> False)
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = rightConcat j b a -- but this will
  | l > l' = startRel l j a b
hash l j a@(InsRel ea _ _) b@(InsRel eb _ _) =
  let e = EO True l
      msg = unlines [ "JointX should have been evaluated earlier."
                    , "level: " ++ show l
                    , "joint: " ++ show j
                    , "left: " ++ show a
                    , "right: " ++ show b ]
  in if e <= eb then error msg
     else if e > ea then startRel l j a b
     else if e == ea then rightConcat j b a
     else error msg

-- == the Insertion parser
expr :: Parser Insertion
expr = makeExprParser term [ [InfixL $ try $ pHash n] | n <- [1..8] ]

term :: Parser Insertion
term = InsLeaf <$> leaf
       <|> close <$> parens expr
       <|> absent where
  absent :: Parser Insertion
  absent = const Absent <$> f <?> "Intended to \"find\" nothing."
  f = lookAhead $ const () <$> C.satisfy (== '#') <|> eof
    -- the Absent parser should look for #, but not ), because
    -- parentheses get consumed in pairs in an outer (earlier) context

pHashUnlabeled :: Int -> Parser ()
pHashUnlabeled n = const () <$> f
  where f = C.string (replicate n '#') <* notFollowedBy (C.char '#')

pHash :: Int -> Parser (Insertion -> Insertion -> Insertion)
pHash n = lexeme $ do
  pHashUnlabeled n
  label <- option "" $ anyWord <|> parens phrase
  return $ hash n $ JointX label


-- == little things
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

wordChar :: Parser Char
wordChar = C.alphaNumChar <|> C.char '_' <|> C.char '-'

word :: String -> Parser String -- | could fail half-in, so requires "try"
word w = lexeme $ C.string w <* notFollowedBy wordChar

anyWord :: Parser String
anyWord = lexeme $ some wordChar

phrase :: Parser String -- | accepts the empty string, because it uses "many"
phrase = concat . intersperse " " <$> many anyWord

leaf :: Parser Expr
leaf = do p <- some anyWord
          return $ case elem "_" p of True ->  mkTplt . f $ p
                                      False -> Word   . f $ p
  where f = concat . intersperse " " 

symbol :: String -> Parser String -- | is already a lexeme
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
