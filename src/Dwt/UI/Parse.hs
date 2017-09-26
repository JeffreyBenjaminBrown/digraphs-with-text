module Dwt.UI.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Types
import Dwt.ParseUtils (Parser, integer, symbol, word)
import Text.Megaparsec (try)
import Dwt.Hash.Parse (expr)
import Dwt.Search.QNode (qGet)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader


data Command = ViewGraph ReadNodes | ShowQueries
type ReadNodes = Reader RSLT (Either DwtErr [Node])

pCommand :: Parser Command
pCommand = foldl1 (<|>) $ map try [pUsers, pAllNodes, pShowQueries, pQNode]

pQNode :: Parser Command
pQNode = ViewGraph . f <$> expr where
  f qnode = do g <- ask
               return $ qGet g qnode

pUsers :: Parser Command
pUsers = ViewGraph . f <$> (symbol "/u" *> integer) where
  f node = do g <- ask
              return $ Right $ pre g node
  -- TODO: what if qNode is not present? use QNode, not Node
  -- or better: TODO: expand QNode to include stars

pAllNodes :: Parser Command
pAllNodes = const f <$> symbol "/a" where
   f = ViewGraph $ do g <- ask
                      return $ Right $ nodes g

pShowQueries :: Parser Command
pShowQueries = const ShowQueries <$> symbol "/q"
