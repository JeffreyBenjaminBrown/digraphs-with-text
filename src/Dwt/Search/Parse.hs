module Dwt.Search.Parse where

import Dwt.Types
import Dwt.ParseUtils (Parser, integer)

import Text.Megaparsec.Char (satisfy, string, char)

qAt :: Parser Insertion
qAt = At <$> integer

