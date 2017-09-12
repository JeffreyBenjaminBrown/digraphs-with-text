module Dwt.Search.Recursive (
  partitionRelSpecQ
  , insRelSpec
  , relSpec
  , usersInRole
  , matchRelSpecNodes
  , matchRelSpecNodesLab
  , has1Dir
  , fork1Dir
  , subNodeForVars
  , dwtDfs
  , dwtBfs
) where

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Search.Local

import Dwt.Util (listIntersect, prependCaller, gelemM, otherDir)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (nub)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Class

partitionRelSpecQ :: RSLT -> RelSpec
  -> Either DwtErr (RelVarSpec, RelNodeSpec)
partitionRelSpecQ g rSpec = let f (VarSpec _) = True
                                f (NodeSpec _) = False
                                (vs,qs) = Map.partition f rSpec
  in do ns <- mapM (\(NodeSpec q) -> qGet1 g q)  qs
        return (Map.map  (\(VarSpec  v) -> v)  vs, ns)

insRelSpec :: RelSpec -> RSLT -> Either DwtErr RSLT
insRelSpec rSpec g = do
  (varMap, nodeMap) <- partitionRelSpecQ g rSpec
  let newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelSpecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemM g) $ Map.elems nodeMap
  let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                $ Map.toList nodeMap
        -- these edges specify the addressed nodes
  return $ insEdges newLEdges $ insNode newLNode g

relSpec :: RSLT -> QNode -> Either DwtErr RelSpecConcrete
  -- name ? getRelSpecDe
  -- is nearly inverse to partitionRelSpec
relSpec g q = prependCaller "relSpec: " $ do
  n <- qGet1 g q
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      rnsl <- Map.toList <$> relNodeSpec g n
      let rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpecC  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpecC node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'
    x -> Left (ConstructorMistmatch, [ErrExpr x, ErrQNode $ QAt n]
              , "relSpec.")

usersInRole :: RSLT -> QNode -> RelRole -> Either DwtErr [Node]
usersInRole g (QAt n) r = prependCaller "usersInRole: " $ _usersInRole g n r
usersInRole g q r = qGet1 g q >>= \n -> _usersInRole g n r

matchRelSpecNodes :: RSLT -> RelSpec -> Either DwtErr [Node]
matchRelSpecNodes g spec = prependCaller "matchRelSpecNodes: " $ do
  let qNodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRole g n r) qNodeSpecs
  return $ listIntersect nodeListList

-- ifdo speed: this searches for nodes, then searches again for labels
matchRelSpecNodesLab :: RSLT -> RelSpec -> Either DwtErr [LNode Expr]
matchRelSpecNodesLab g spec = prependCaller "matchRelSpecNodesLab: " $ do
  ns <- matchRelSpecNodes g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe because matchRelSpecNodes only returns Nodes in g

has1Dir :: Mbrship -> RelSpec -> Bool
has1Dir mv rc = 1 == length (Map.toList $ Map.filter f rc)
  where f (VarSpec y) = y == mv
        f _ = False

fork1Dir :: RSLT -> QNode -> (Mbrship,RelSpec) -> Either DwtErr [Node]
fork1Dir g qFrom (dir,axis) = do -- returns one generation, neighbors
  fromDir <- otherDir dir
  if has1Dir fromDir axis then return ()
     else Left (Invalid, [ErrRelSpecQ axis]
               , "fork1Dir: should have only one " ++ show fromDir)
  let dirRoles = Map.keys $ Map.filter (== VarSpec dir) axis
  axis' <- runReaderT (subNodeForVars qFrom fromDir axis) g
  rels <- matchRelSpecNodes g axis'
  concat <$> mapM (\rel -> relElts g rel dirRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
      -- EXCEPT: that might hurt the dfs, bfs functions below

--TODO: fork1DirsQ
--fork1DirsQ :: RSLT -> QNode -> [(Mbrship,RelSpec)] -> Either DwtErr [Node]
--fork1DirsQ g q rs = concat <$> mapM (fork1Dir g n) rs

subNodeForVars :: QNode -> Mbrship -> RelSpec
  -> ReaderT RSLT (Either DwtErr) RelSpec
subNodeForVars q v r = do -- TODO: use prependCaller
  g <- ask
  n <- lift $ qGet1 g q
  let f (VarSpec v') = if v == v' then NodeSpec (QAt n) else VarSpec v'
      f x = x -- the v,v' distinction is needed; otherwise v gets masked
  lift $ Right $ Map.map f r -- ^ change each VarSpec v to NodeSpec n

_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- | determines dfs|bfs
  -> RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> Either DwtErr [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g qdir pending@(n:ns) acc = do
  newNodes <- fork1Dir g (QAt n) qdir
    --ifdo speed: calls has1Dir redundantly
  _bfsOrDfs collector g qdir (nub $ collector newNodes ns) (n:acc)
    -- ifdo speed: discard visited nodes from graph

_dwtBfs = _bfsOrDfs (\new old -> old ++ new)
_dwtDfs = _bfsOrDfs (\new old -> new ++ old)

dwtDfs :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either DwtErr [Node]
dwtDfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> _dwtDfs g dir starts []

dwtBfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either DwtErr [Node]
dwtBfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> _dwtBfs g dir starts []
