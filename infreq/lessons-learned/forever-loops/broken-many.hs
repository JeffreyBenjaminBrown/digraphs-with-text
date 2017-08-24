module Experim where

import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), parseTest, many, notFollowedBy)
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void String

obviouslyBroken :: Parser [String]
obviouslyBroken = many $ many $ C.char '#'

butWhyIsThisBroken :: Parser [String]
butWhyIsThisBroken = many $ (many $ C.char '#')
                     <* (notFollowedBy $ C.char '#')
