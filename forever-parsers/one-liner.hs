module Experim where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseTest, many)
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void String

main = parseTest broken ""

broken :: Parser [String]
broken = many $ many $ C.char '#'
