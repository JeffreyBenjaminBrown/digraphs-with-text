module Dwt.ParseUtils (
  Parser
  , sc, lexeme, parens -- grouping

  , wordChar, symbol, word, anyWord, phrase -- words
  ) where

import Data.Graph.Inductive (Node)

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

wordChar :: Parser Char
wordChar = C.alphaNumChar <|> C.char '_' <|> C.char '-'

symbol :: String -> Parser String -- | is already a lexeme
symbol = L.symbol sc

-- | The notFollowedBy makes word different from symbol.
-- For instance, word "(" will fail where symbol "(" would not.
word :: String -> Parser String -- | could fail half-in, so requires "try"
word w = lexeme $ C.string w <* notFollowedBy wordChar

anyWord :: Parser String
anyWord = lexeme $ some wordChar

phrase :: Parser String -- | accepts the empty string, because it uses "many"
phrase = concat . intersperse " " <$> many anyWord
