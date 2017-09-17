module Dwt.Search.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Types
import Dwt.Hash.Parse (leaf)
import Dwt.ParseUtils (Parser, integer, symbol)
import Text.Megaparsec.Char (satisfy, string, char)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

data Command = ViewGraph ReadNodes | ShowQueries | ShowQNodes

type ReadNodes = Reader RSLT [Node]

pCommand :: Parser Command
pCommand = foldl1 (<|>) [pUsers, pAllNodes, pShowQueries]
  --TODO: add pShowQNodes]

pUsers :: Parser Command
pUsers = ViewGraph . f <$> (symbol "u" *> integer) where
  f qNode = do g <- ask
               return $ pre g qNode

pAllNodes :: Parser Command
pAllNodes = const (ViewGraph $ asks nodes) <$> symbol "all"

pShowQueries :: Parser Command
pShowQueries = const ShowQueries <$> symbol "qs"

pShowQNodes :: Parser Command
pShowQNodes = const ShowQNodes <$> symbol "is"


-- ==== Parse a QNode
pAt :: Parser QNode
pAt = At <$> integer

pLeaf :: Parser QNode
pLeaf = QLeaf <$> leaf

