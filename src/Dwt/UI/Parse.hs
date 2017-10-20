module Dwt.UI.Parse where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader
import Data.Graph.Inductive (Node, pre, nodes)
import Text.Megaparsec (try)

import Dwt.Initial.Types
import Dwt.Initial.ParseUtils (Parser, integer, word)
import Dwt.Second.QNode (qNodeTop)
import Dwt.Hash.Parse (expr)
import Dwt.Query.QNode (qGet)


commandToReadNodes :: Command -> ReadNodes
commandToReadNodes (CommandQNode q) = do g <- ask
                                         return $ qGet g $ qNodeTop q
commandToReadNodes (CommandUsers n) = do g <- ask; return $ Right $ pre g n
commandToReadNodes CommandAllNodes = do g <- ask; return $ Right $ nodes g
commandToReadNodes c@CommandShowQueries = return $ Left
  (ConstructorMistmatch, [ErrCommand c], "commandToReadNodes.")

pCommand :: Parser Command
pCommand = foldl1 (<|>) $ map try [pUsers
                                  , pAllNodes
                                  , pShowQueries
                                  , pQNodeCommand
                                  ]

pUsers :: Parser Command
pUsers = CommandUsers <$> (word "/users" *> integer)

pAllNodes :: Parser Command
pAllNodes = const CommandAllNodes <$> word "/all"

pShowQueries :: Parser Command
pShowQueries = const CommandShowQueries <$> word "/queries"

pQNodeCommand :: Parser Command
pQNodeCommand = CommandQNode <$> expr
