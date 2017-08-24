module Experim where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseTest, many)
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void String

main = parseTest broken ""

obviouslyBroken :: Parser [String]
obviouslyBroken = many $ many $ C.char '#'

butWhyIsThisBroken :: Parser [String]
butWhyIsThisBroken = many $ (many $ C.char '#')
                     <* (notFollowedBy $ C.char '#')
