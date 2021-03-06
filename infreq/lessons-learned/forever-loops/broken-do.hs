module Experim where

import Control.Applicative (empty)
import Data.Void (Void)
import Data.List (intersperse)
import Text.Megaparsec (Parsec, (<|>), between, many, some, option)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

main = parseTest broken ""

broken :: Parser String
broken = do level <- length <$> many (C.char '#')
            label <- option "" $ parens phrase <|> anyWord
            return $ show level ++ " " ++ label


-- little things
sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

wordChar :: Parser Char
wordChar = C.alphaNumChar <|> C.char '_' <|> C.char '-'

anyWord :: Parser String
anyWord = lexeme $ some wordChar

phrase :: Parser String
phrase = concat . intersperse " " <$> many anyWord

symbol :: String -> Parser String -- is a lexeme; consumes trailing space
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
