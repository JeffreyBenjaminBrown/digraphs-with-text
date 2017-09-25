module Dwt.Search.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Types
import Dwt.ParseUtils (Parser, integer, symbol, word)
import Dwt.Hash.Parse (exprSum)
import Dwt.Search.QNode (qGet)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader


data Command = ViewGraph ReadNodes | ShowQueries | ShowQNodes
type ReadNodes = Reader RSLT (Either DwtErr [Node])

pCommand :: Parser Command
pCommand = foldl1 (<|>) [pUsers, pAllNodes, pShowQueries, pQNode]
  --TODO: add pShowQNodes]

pQNode :: Parser Command
pQNode = ViewGraph . f <$> (word "qn" >> exprSum) where
  f qnode = do g <- ask
               return $ qGet g qnode

pUsers :: Parser Command
pUsers = ViewGraph . f <$> (symbol "u" *> integer) where
  f node = do g <- ask
              return $ Right $ pre g node
  -- TODO: what if qNode is not present? use QNode, not Node

pAllNodes :: Parser Command
pAllNodes = const f <$> symbol "all" where
   f = ViewGraph $ do g <- ask
                      return $ Right $ nodes g

pShowQueries :: Parser Command
pShowQueries = const ShowQueries <$> symbol "qs"

pShowQNodes :: Parser Command
pShowQNodes = const ShowQNodes <$> symbol "is"
