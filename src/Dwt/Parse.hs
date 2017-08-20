module Dwt.Parse where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

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

hasBlanks :: String -> Either  (ParseError (Token String) Void) Bool
hasBlanks = parse p "not a file"
  where p :: Parser Bool
        p = (>0) . length . concat
            <$> (sc *> (many $ blank <|> other)) -- order across <|> matters
        blank :: Parser String
        blank = try $ word "_"
        other :: Parser String
        other = const "" <$> anyWord

-- ========== Megaparsec.Expr
symbol :: String -> Parser String -- ? word v. symbol
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

data AExpr = Var String | Pair AExpr AExpr deriving (Show)

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

aTerm :: Parser AExpr
aTerm = parens aExpr   <|>   Var <$> identifier

aOperators :: [[Operator Parser AExpr]]
  -- each list is a set of operators of equal precedence
aOperators = [ [ InfixL $ op "#" *> pure (Pair) ]
             , [ InfixL $ op "##" *> pure (Pair) ]
             ] where
  op :: String -> Parser String
  op n = lexeme . try $ C.string n <* notFollowedBy (C.char '#')

testMegaparsecExpr = mapM_ (putStrLn . show) $ map (parseMaybe aExpr)
  [ "a # b"               -- # and ## do the same thing
  , "a ## b"
  , "a # b # c"           -- both bind from the left
  , "a  #  b  ## c #  d"  -- ## binds after #
  , "(a ## b) # (c ## d)"
  ]
