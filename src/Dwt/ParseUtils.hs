module Dwt.ParseUtils (
  Parser
  , sc, lexeme, parens -- grouping

  , wordChar, symbol, word, anyWord, phrase -- words

  , integer
  ) where

import Data.Graph.Inductive (Node)

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String -- | is already a lexeme
symbol = L.symbol sc

-- TODO: Mystery: I want wordChar to include '_', but it makes tests fail.
wordChar :: Parser Char
wordChar = C.alphaNumChar <|> C.char '-'

-- | The notFollowedBy makes word different from symbol.
-- For instance, word "(" will fail where symbol "(" would not.
word :: String -> Parser String -- | could fail half-in, so requires "try"
word w = lexeme $ C.string w <* notFollowedBy wordChar

anyWord :: Parser String
anyWord = lexeme $ some wordChar  <* notFollowedBy wordChar

phrase :: Parser String -- | does not accept the empty string
phrase = concat . intersperse " " <$> some anyWord

integer :: Integral a => Parser a
integer = lexeme L.decimal