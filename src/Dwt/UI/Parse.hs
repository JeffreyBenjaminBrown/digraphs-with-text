module Dwt.UI.Parse where

import Data.Graph.Inductive (Node, pre, nodes)
import Dwt.Initial.Types
import Dwt.Initial.ParseUtils (Parser, integer, word)
import Text.Megaparsec (try)
import Dwt.Hash.Parse (expr)
import Dwt.Query.QNode (qGet)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader


data Command = ViewGraph ReadNodes | ShowQueries

commandXXToReadNodes :: CommandXX -> ReadNodes
commandXXToReadNodes (CommandQNode q) = do g <- ask; return $ qGet g q
commandXXToReadNodes (CommandUsers n) = do g <- ask; return $ Right $ pre g n
commandXXToReadNodes CommandAllNodes = do g <- ask; return $ Right $ nodes g
commandXXToReadNodes c@CommandShowQueries = return $ Left
  (ConstructorMistmatch, [ErrCommandXX c], "commandXXToReadNodes.")

pCommand :: Parser Command
pCommand = foldl1 (<|>) $ map try [pUsers
                                  , pAllNodes
                                  , pShowQueries
                                  , pQNodeCommand
                                  ]

pUsers :: Parser Command
pUsers = ViewGraph . f <$> (word "/users" *> integer) where
  f node = do g <- ask
              return $ Right $ pre g node
  -- TODO: what if qNode is not present? use QNode, not Node
  -- or better: TODO: expand QNode to include stars

pAllNodes :: Parser Command
pAllNodes = const f <$> word "/all" where
   f = ViewGraph $ do g <- ask
                      return $ Right $ nodes g

pShowQueries :: Parser Command
pShowQueries = const ShowQueries <$> word "/queries"

pQNodeCommand :: Parser Command
pQNodeCommand = ViewGraph . f <$> expr where
  f qnode = do g <- ask
               return $ qGet g qnode
