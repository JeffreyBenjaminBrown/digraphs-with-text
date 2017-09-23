-- Unused, except in some tests, but potentially handy.

module Dwt.Search.Misfits (
  getRelNodeSpec -- RSLT -> Node(RelspecExpr) -> Either DwtErr RelNodeSpec
  , getRelspec -- RSLT -> QNode -> Either DwtErr Relspec
  , partitionRelspec -- returns two relspecs
  , insRelspec -- add a relspec to a graph. unused.
  ) where

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Util (gelemM, prependCaller)
import Dwt.Search.QNode (qGet1)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

getRelNodeSpec :: RSLT -> Node -> Either DwtErr RelNodeSpec
getRelNodeSpec g n = prependCaller "getRelNodeSpec: " $ do
  gelemM g n
  case lab g n of
    Just (RelspecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
            f _ = error "getRelNodeSpec: something was not a RelEdge"
    Just _ -> Left (NotRelspecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

getRelspec :: RSLT -> QNode -> Either DwtErr Relspec
  -- is nearly inverse to partitionRelspec
getRelspec g q = prependCaller "getRelspec: " $ do
  n <- qGet1 g q
  case (fromJust $ lab g n) of
    RelspecExpr rvs -> do
      rnsl <- Map.toList <$> getRelNodeSpec g n
      let rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'
    x -> Left (ConstructorMistmatch, [ErrExpr x, ErrQNode $ At n], ".")

partitionRelspec :: RSLT -> QRelspec
  -> Either DwtErr (RelVarSpec, RelNodeSpec)
partitionRelspec g rSpec = let f (QVarSpec _) = True
                               f (QNodeSpec _) = False
                               (vs,qs) = Map.partition f rSpec
  in do ns <- mapM (\(QNodeSpec q) -> qGet1 g q)  qs
        return (Map.map  (\(QVarSpec  v) -> v)  vs, ns)

insRelspec :: QRelspec -> RSLT -> Either DwtErr RSLT -- ^ unused
insRelspec rSpec g = do
  (varMap, nodeMap) <- partitionRelspec g rSpec
  let newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelspecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemM g) $ Map.elems nodeMap
  let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                $ Map.toList nodeMap
        -- these edges specify the addressed nodes
  return $ insEdges newLEdges $ insNode newLNode g
