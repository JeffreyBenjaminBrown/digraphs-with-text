module Dwt.Parse where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Data.List (intersperse)

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

hasBlanks :: String -> Either  (ParseError (Token String) Void) Bool
hasBlanks = parse p "not a file"
  where p :: Parser Bool
        p = (>0) . length . concat
            <$> (sc *> (many $ blank <|> other)) -- order across <|> matters
        blank :: Parser String
        blank = try $ word "_"
        other :: Parser String
        other = const "" <$> anyWord

-- Solution?
data AddX = Leaf String -- expresses how to add (nested) data to the RSLT
             | BothX EO AddX Joint [(AddX,Joint)] AddX
             deriving (Show, Eq)
type Level = Int
data Joint = Joint String deriving (Show, Eq)
data EO = EO { inParens :: Bool -- "expression orderer"
             , inLevel :: Level } deriving (Show, Eq)
instance Ord EO where
  EO a b <= EO c d
    | a /= c = c <= a
    | otherwise = b <= d

--rightConcat :: AddX -> AddX -> AddX
--rightConcat new big@(BothX eo

joint :: Level -> Joint -> AddX -> AddX -> AddX
joint l j@(Joint _) a@(Leaf _) b@(Leaf _)
  = BothX (EO False l) a j [] b
joint l j@(Joint _) a@(Leaf _) b@(BothX _ _ _ _ _)
  = BothX (EO False l) a j [] b -- I *think* level doesn't matter here.
joint l j@(Joint _) a@(BothX _ _ _ _ _) b@(Leaf _)
  = BothX (EO False l) a j [] b -- I *think* level doesn't matter here.

-- a ## b ## c
-- a # b ## c = (a,(b,c))
-- b@(BothX (EO inp l') x1 j'@(Joint _) ps x2)

-- discussion: https://www.reddit.com/r/haskell/comments/6v9b13/can_this_problem_be_approached_from_the_bottomup/

-- later: more cases
-- data AddExpr = ...
          -- | Absent
          -- | LeftX  EO AddExpr Joint [(AddExpr,Joint)]
          -- | RightX EO         Joint [(AddExpr,Joint)] AddExpr
--joint j@(Joint k _) a@(Leaf _) b@(Leaf _)
--  = BothX  (EO False k) a j [] b
--joint j@(Joint k _) Absent b@(Leaf _)
--  = RightX (EO False k)   j [] b
--joint j@(Joint k _) a@(Leaf _) Absent
--  = LeftX  (EO False k) a j []


--   a ## b ## c
-- = (RelExpr (EO False 2) (Leaf a) (Joint 2 "") (Leaf b))  ##  c
-- = RelExpr (EO False 2) (Leaf a) (Joint 2 "") 

-- == Option: Use "many" to parse a bunch of the same level

-- == Option: use an Int field in the RelExpr and Join constructors
-- data Expr = Leaf String
--          | RelExpr Mex Join [(Expr,Join)] Mex
-- type Join = Join Int Add
-- type Mex = Maybe Expr

-- == Option: Use Megaparsec.Expr. Close!
  -- (In this I was calling Expr "Mad", for "Maybe Graph Add instruction".)
-- expr :: Parser Mad
-- expr = foldl addPrecLevel term []
-- 
-- addPrecLevel' :: MonadParsec e s m
--               => m Mad -> [Operator m Mad] -> m Mad
-- addPrecLevel' term ops = -- based on Text.Megaparsec.Expr
--   term' >>= \x -> choice [las' x, return x] <?> "operator"
--   where (_, las, _, prefix, _) = foldr splitOp ([],[],[],[],[]) ops
--         term' = option Nothing
--           $ Just <$> pTerm (choice prefix) term (return id)
--         las'  = pInfixL (choice las) term'
-- 
-- pInfixL' :: MonadParsec e s m
--   => m (Mad -> Mad -> Mad) -> m Mad -> Mad -> m Mad
-- pInfixL' op p x = do
--   f <- op
--   y <- p
--   let r = f x y
--   pInfixL op p r <|> return r
-- 
-- term :: Parser Mad
-- term = parens expr
--        <|> Just . Leaf . concat . intersperse " " <$> many anyWord

hashes :: Int -> Parser ()
hashes n = C.string prefix *> notFollowedBy (C.char '#')
  where prefix = take n $ repeat '#' :: String

-- == "#" can abut a word or a parenthesized string of words
-- TODO: let it adjoin an entire nested expression
hashed :: Int -> Parser String
hashed n = C.string prefix *> notFollowedBy (C.char '#')
           *> option "" something
  where prefix = take n $ repeat '#' :: String
        something = concat . intersperse " " <$> parens (many anyWord)
                    <|> anyWord

-- == Binary Nesting Hash Expressions
data BinaryHashExpr = Var String | Pair BinaryHashExpr BinaryHashExpr deriving (Show)

binHashExpr :: Parser BinaryHashExpr
binHashExpr = makeExprParser aTerm aOperators

aTerm :: Parser BinaryHashExpr
aTerm = parens binHashExpr   <|>   Var <$> identifier

aOperators :: [[Operator Parser BinaryHashExpr]]
  -- each list is a set of operators of equal precedence
aOperators = [ [ InfixL $ op "#" *> pure (Pair) ]
             , [ InfixL $ op "##" *> pure (Pair) ]
             ] where
  op :: String -> Parser String
  op n = lexeme . try $ C.string n <* notFollowedBy (C.char '#')

testBinHashExpr :: IO ()
testBinHashExpr = mapM_ (putStrLn . show) $ map (parseMaybe binHashExpr)
  [ "a # b"               -- # and ## do the same thing
  , "a ## b"
  , "a # b # c"           -- both bind from the left
  , "a  #  b  ## c #  d"  -- ## binds after #
  , "(a ## b) # (c ## d)"
  ]

-- little things
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

wordChar :: Parser Char
wordChar = C.alphaNumChar <|> C.char '_' <|> C.char '-'

word :: String -> Parser String -- could fail half-in, so requires "try"
word w = lexeme $ C.string w <* notFollowedBy wordChar

anyWord :: Parser String
anyWord = lexeme $ some wordChar

symbol :: String -> Parser String -- is a lexeme; consumes trailing space
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar
