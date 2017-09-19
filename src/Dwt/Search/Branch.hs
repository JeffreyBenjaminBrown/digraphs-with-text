module Dwt.Search.Branch (
  otherDir -- Mbrship -> Either DwtErr Mbrship
  , partitionRelSpec -- returns two relspecs
  , insRelSpec -- add a relspec to a graph
  , getRelSpec -- unused.  RSLT -> QNode -> Either DwtErr RelSpec
  , has1Dir -- Mbrship(the dir it has 1 of) -> QRelSpec -> Bool
  , fork1Dir -- RSLT -> QNode -> (Mbrship,QRelSpec) -> Either DwtErr [Node]
    -- TODO: rewrite Mbrship as To,From,It,Any. Then use QRelSpec, not a pair.
  , subNodeForVars --QNode(sub this) -> Mbrship(for this) -> QRelSpec(in this)
    -- -> ReaderT RSLT (Either DwtErr) QRelSpec
  , dwtDfs -- RSLT -> (Mbrship,QRelSpec) -> [Node] -> Either DwtErr [Node]
  , dwtBfs -- same
) where

import Data.Graph.Inductive (Node, insNode, insEdges, newNodes, lab)
import Dwt.Types
import Dwt.Search.Base (getRelNodeSpec, selectRelElts)
import Dwt.Search.QNode (qGet1, playsRoleIn, matchQRelSpecNodes)

import Dwt.Util (listIntersect, prependCaller, gelemM)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (nub)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph (hoist)


-- | swap Up and Down, or err
otherDir :: Mbrship -> Either DwtErr Mbrship
otherDir Up = Right Down
otherDir Down = Right Up
otherDir mv = Left (ConstructorMistmatch, [ErrMbrship mv]
                   , "otherDir: Only accepts Up or Down.")

partitionRelSpec :: RSLT -> QRelSpec
  -> Either DwtErr (RelVarSpec, RelNodeSpec)
partitionRelSpec g rSpec = let f (QVarSpec _) = True
                               f (QNodeSpec _) = False
                               (vs,qs) = Map.partition f rSpec
  in do ns <- mapM (\(QNodeSpec q) -> qGet1 g q)  qs
        return (Map.map  (\(QVarSpec  v) -> v)  vs, ns)

insRelSpec :: QRelSpec -> RSLT -> Either DwtErr RSLT
insRelSpec rSpec g = do
  (varMap, nodeMap) <- partitionRelSpec g rSpec
  let newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelSpecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemM g) $ Map.elems nodeMap
  let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                $ Map.toList nodeMap
        -- these edges specify the addressed nodes
  return $ insEdges newLEdges $ insNode newLNode g

getRelSpec :: RSLT -> QNode -> Either DwtErr RelSpec
  -- is nearly inverse to partitionRelSpec
getRelSpec g q = prependCaller "getRelSpec: " $ do
  n <- qGet1 g q
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      rnsl <- Map.toList <$> getRelNodeSpec g n
      let rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'
    x -> Left (ConstructorMistmatch, [ErrExpr x, ErrQNode $ At n], ".")

has1Dir :: Mbrship -> QRelSpec -> Bool
has1Dir mv rc = 1 == length (Map.toList $ Map.filter f rc)
  where f (QVarSpec y) = y == mv
        f _ = False

fork1Dir :: RSLT -> QNode -> (Mbrship,QRelSpec) -> Either DwtErr [Node]
fork1Dir g qFrom (forward,axis) = do -- returns one generation, neighbors
  fromDir <- otherDir forward
  if has1Dir fromDir axis then return ()
     else Left (Invalid, [ErrRelSpec axis]
               , "fork1Dir: should have only one " ++ show fromDir)
  let forwardRoles = Map.keys $ Map.filter (== QVarSpec forward) axis
  axis' <- runReaderT (subNodeForVars qFrom fromDir axis) g
  rels <- matchQRelSpecNodes g axis'
  concat <$> mapM (\rel -> selectRelElts g rel forwardRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
      -- EXCEPT: that might hurt the dfs, bfs functions below

-- | in r, changes each QVarSpec v to QNodeSpec n
subNodeForVars :: QNode -> Mbrship -> QRelSpec
  -> ReaderT RSLT (Either DwtErr) QRelSpec
subNodeForVars q v r = hoist (prependCaller "subNodeForVars") $ do
  g <- ask
  n <- lift $ qGet1 g q
  let f (QVarSpec v') = if v == v' then QNodeSpec (At n) else QVarSpec v'
      f x = x -- f needs the v,v' distinction; otherwise v gets masked
  lift $ Right $ Map.map f r

_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- ^ order determines dfs or bfs
  -> RSLT -> (Mbrship, QRelSpec)
  -> [Node] -- ^ pending accumulation
  -> [Node] -- ^ the accumulator
  -> Either DwtErr [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g qdir (n:ns) acc = do
  newNodes <- fork1Dir g (At n) qdir -- todo speed ? calls has1Dir too much
  _bfsOrDfs collector g qdir (nub $ collector newNodes ns) (n:acc)
    -- todo speed ? discard visited nodes from graph.  (might backfire?)

bfsConcat = _bfsOrDfs (\new old -> old ++ new)
dfsConcat = _bfsOrDfs (\new old -> new ++ old)

dwtDfs :: RSLT -> (Mbrship,QRelSpec) -> [Node] -> Either DwtErr [Node]
dwtDfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> dfsConcat g dir starts []

dwtBfs :: RSLT -> (Mbrship, QRelSpec) -> [Node] -> Either DwtErr [Node]
dwtBfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> bfsConcat g dir starts []
