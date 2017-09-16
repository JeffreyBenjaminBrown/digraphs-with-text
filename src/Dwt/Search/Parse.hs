module Dwt.Search.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Types
import Dwt.ParseUtils (Parser, integer, symbol)
import Text.Megaparsec.Char (satisfy, string, char)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

data Command = ViewGraph ReadNodes | ShowQueries | ShowInsertions

type ReadNodes = Reader RSLT [Node]

pCommand :: Parser Command
pCommand = foldl1 (<|>) [pUsers, pAllNodes, pShowQueries, pShowInsertions]

pUsers :: Parser Command
pUsers = ViewGraph . f <$> (symbol "u" *> integer) where
  f insertion = do g <- ask
                   return $ pre g insertion

pAllNodes :: Parser Command
pAllNodes = const (ViewGraph $ asks nodes) <$> symbol "all"

pShowQueries :: Parser Command
pShowQueries = const ShowQueries <$> symbol "qs"

pShowInsertions :: Parser Command
pShowInsertions = const ShowInsertions <$> symbol "is"


-- ==== unused
pAt :: Parser Insertion
pAt = At <$> integer
