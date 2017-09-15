module Dwt.Search.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Types
import Dwt.ParseUtils (Parser, integer, symbol)
import Text.Megaparsec.Char (satisfy, string, char)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

pAt :: Parser Insertion
pAt = At <$> integer

pUsers :: Parser (Reader RSLT [Node])
pUsers = f <$> (symbol "u" *> integer) where
  f insertion = do g <- ask
                   return $ pre g insertion

pAll :: Parser (Reader RSLT [Node])
pAll = const (asks nodes) <$> symbol "all"
