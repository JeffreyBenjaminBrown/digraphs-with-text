-- From an expression like "I #like turtles ##when the sun #is big", creates a set of instructions for adding expressions to a RSLT.
-- discussion: https://www.reddit.com/r/haskell/comments/6v9b13/can_this_problem_be_approached_from_the_bottomup/
{-# LANGUAGE ViewPatterns #-}

module Dwt.Parse where

import Data.Graph.Inductive (Node)
import Dwt.Types
import Dwt.Graph (mkTplt)

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
-- AddX expresses how to add (nested) data to the RSLT
data AddX = At Node -- for when you know the expression's node. TODO: parse
          | Absent
          | LeafX Expr
          | RelX EO [JointX] [AddX]
  -- Every rel has at least one jointX, and potentially members on either side
  -- If there are more, the list of pairs stores them.
          deriving (Show, Eq)
type Level = Int -- in "cats like you because you like them", the "because"
  -- relationship is level 2, and the "like" relationships are level 1
data JointX = JointX String deriving (Show, Eq)
  -- in "you #like peaches #at noon", "like" and "at" are jointXs
data EO = EO     -- EO = "expression orderer"
  { open :: Bool -- open = "more expressions can be concatentated into it"
                 -- In b@(RelX (EO x _) _ _), x is true until
                 -- b has been surrounded by parentheses.
  , inLevel :: Level } deriving (Eq)

instance Show EO where
  show (EO x y) = "(EO " ++ show x ++ " " ++ show y ++ ")"
instance Ord EO where -- Open > closed. If those are equal, ## > #, etc.
  EO a b <= EO c d = a <= c && b <= d

isRelX :: AddX -> Bool
isRelX (RelX _ _ _) = True
isRelX _ = False

startRel :: Level -> JointX -> AddX -> AddX -> AddX
startRel l j a b = RelX (EO True l) [j] [a,b]

-- | PITFALL: In "a # b # c # d", you might imagine evaluating the middle #
-- after the others. In that case both sides would be a RelX, and you would
-- want to modify both, rather than make one a member of the other. These
-- concat functions skip that possibility; one of the two AddX arguments is
-- always incorporated into the other. I believe that is safe, because 
-- expressions in serial on the same level will always be parsed left to
-- right, not outside to inside.
rightConcat :: JointX -> AddX -> AddX -> AddX
  -- TODO: if|when need speed, use a two-sided list of pairs
rightConcat j m (RelX eo joints mbrs)
  = RelX eo (joints ++ [j]) (mbrs ++ [m])
rightConcat _ _ _ = error "can only rightConcat into a RelX"

leftConcat :: JointX -> AddX -> AddX -> AddX
leftConcat j m (RelX eo joints mbrs)
  = RelX eo (j : joints) (m : mbrs)
leftConcat _ _ _ = error "can only leftConcat into a RelX"

close :: AddX -> AddX
close (LeafX x) = LeafX x
close (RelX (EO _     a) b c)
     = RelX (EO False a) b c

hash :: Level -> JointX -> AddX -> AddX -> AddX
hash l j a@(isRelX -> False) b@(isRelX -> False)       = startRel l j a b
hash l j a@(isRelX -> False) b@(RelX (EO False _) _ _) = startRel l j a b
hash l j a@(RelX (EO False _) _ _) b@(isRelX -> False) = startRel l j a b
hash l j a@(isRelX -> False) b@(RelX (EO True l') _ _)
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = leftConcat j a b -- I suspect this won't happen either
  | l > l' = startRel l j a b
hash l j a@(RelX (EO True l') _ _) b@(isRelX -> False)
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = rightConcat j b a -- but this will
  | l > l' = startRel l j a b
hash l j a@(RelX ea _ _) b@(RelX eb _ _) =
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

-- == the AddX parser
expr :: Parser AddX
expr = makeExprParser term [ [InfixL $ try $ pHash n] | n <- [1..8] ]

term :: Parser AddX
term = LeafX <$> leaf
       <|> close <$> parens expr
       <|> absent where
  absent :: Parser AddX
  absent = const Absent <$> f <?> "Intended to \"find\" nothing."
  f = lookAhead $ const () <$> C.satisfy (`elem` ")#") <|> eof

pHashUnlabeled :: Int -> Parser ()
pHashUnlabeled n = const () <$> f
  where f = C.string (replicate n '#') <* notFollowedBy (C.char '#')

pHash :: Int -> Parser (AddX -> AddX -> AddX)
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
