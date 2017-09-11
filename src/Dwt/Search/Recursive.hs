module Dwt.Search.Recursive where

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Search.Local

import Dwt.Util (listIntersect, prependCaller)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

usersInRoleQ :: RSLT -> QNode -> RelRole -> Either DwtErr [Node]
usersInRoleQ g (QAt n) r = prependCaller "usersInRole: " $ usersInRole g n r
usersInRoleQ g q r = qGet1 g q >>= \n -> usersInRole g n r

matchRelSpecNodesQ :: RSLT -> RelSpecQ -> Either DwtErr [Node]
matchRelSpecNodesQ g spec = prependCaller "matchRelSpecNodes: " $ do
  let qNodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpecQ _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVarQ)]
  nodeListList <- mapM (\(r,NodeSpecQ n) -> usersInRoleQ g n r) qNodeSpecs
  return $ listIntersect nodeListList

-- ifdo speed: this searches for nodes, then searches again for labels
matchRelSpecNodesLabQ :: RSLT -> RelSpecQ -> Either DwtErr [LNode Expr]
matchRelSpecNodesLabQ g spec = prependCaller "matchRelSpecNodesLab: " $ do
  ns <- matchRelSpecNodesQ g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe because matchRelSpecNodesQ only returns Nodes in g
