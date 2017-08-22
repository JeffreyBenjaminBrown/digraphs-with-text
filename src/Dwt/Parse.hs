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

-- == an instruction type for adding an expression to the graph
type End = Maybe GraphAdd
type Join = String
data GraphAdd = Leaf String
              | GraphAdd End Join [(GraphAdd,Join)] End
  -- TODO: The Tplt constructor for RSLT itself should be like this.

expr :: Parser String
expr = foldl addPrecLevel term []

addPrecLevel' :: MonadParsec e s m => m a -> [Operator m a] -> m a
addPrecLevel' term ops = -- based on Text.Megaparsec.Expr
  term' >>= \x -> choice [las' x, return x] <?> "operator"
  where (_, las, _, prefix, _) = foldr splitOp ([],[],[],[],[]) ops
        term' = pTerm (choice prefix) term (return id)
        las'  = pInfixL (choice las) term'

term :: Parser String
term = parens expr
       <|> concat . intersperse " " <$> many anyWord

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
