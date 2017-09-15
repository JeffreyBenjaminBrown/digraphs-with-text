module Dwt.Search.Parse where

import Data.Graph.Inductive (Node)
import Dwt.Types
import Dwt.ParseUtils (Parser, integer)
import Text.Megaparsec.Char (satisfy, string, char)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

pAt :: Parser Insertion
pAt = At <$> integer

pModel :: Parser (Reader RSLT Node)
pModel = const (return 1) <$> char 'c'
