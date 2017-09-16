module Dwt.Search.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Types
import Dwt.ParseUtils (Parser, integer, symbol)
import Text.Megaparsec.Char (satisfy, string, char)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

data Command = ViewGraph ReadNodes | ShowQueries | ShowInsertions

type ReadNodes = Reader RSLT [Node]

pAt :: Parser Insertion
pAt = At <$> integer

pUsers :: Parser ReadNodes
pUsers = f <$> (symbol "u" *> integer) where
  f insertion = do g <- ask
                   return $ pre g insertion

pAllNodes :: Parser ReadNodes
pAllNodes = const (asks nodes) <$> symbol "all"

pShowQueries :: Parser Command
pShowQueries = const ShowQueries <$> symbol "qs"

pShowInsertions :: Parser Command
pShowInsertions = const ShowInsertions <$> symbol "is"
