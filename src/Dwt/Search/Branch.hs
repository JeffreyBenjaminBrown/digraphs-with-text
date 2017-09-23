module Dwt.Search.Branch (
  otherDir -- Mbrship -> Either DwtErr Mbrship
  , partitionRelspec -- returns two relspecs
  , insRelspec -- add a relspec to a graph
  , getRelspec -- unused.  RSLT -> QNode -> Either DwtErr Relspec
  , has1Dir -- Mbrship(the dir it has 1 of) -> QRelspec -> Bool
  , star -- RSLT -> QNode -> QRelspec -> Either DwtErr [Node]
    -- TODO: rewrite Mbrship as To,From,It,Any. Then use QRelspec, not a pair.
  , subQNodeForVars --QNode(sub this) -> Mbrship(for this) -> QRelspec(in this)
    -- -> ReaderT RSLT (Either DwtErr) QRelspec
  , dwtDfs -- RSLT -> QRelspec -> [Node] -> Either DwtErr [Node]
  , dwtBfs -- same
) where

import Data.Graph.Inductive (Node, insNode, insEdges, newNodes, lab)
import Dwt.Types
import Dwt.Search.Base (getRelNodeSpec, selectRelElts)
import Dwt.Search.QNode (qGet1, matchQRelspecNodes)

import Dwt.Util (listIntersect, prependCaller, gelemM)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (nub)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph (hoist)


-- | swap Up and Down, or err
otherDir :: Mbrship -> Either DwtErr Mbrship
otherDir From = Right To
otherDir To = Right From
otherDir Any = Right Any
otherDir It = Left (ConstructorMistmatch, [ErrMbrship It], "otherDir: Accepts To, From or Any.") -- todo ? just return Right It

partitionRelspec :: RSLT -> QRelspec
  -> Either DwtErr (RelVarSpec, RelNodeSpec)
partitionRelspec g rSpec = let f (QVarSpec _) = True
                               f (QNodeSpec _) = False
                               (vs,qs) = Map.partition f rSpec
  in do ns <- mapM (\(QNodeSpec q) -> qGet1 g q)  qs
        return (Map.map  (\(QVarSpec  v) -> v)  vs, ns)

insRelspec :: QRelspec -> RSLT -> Either DwtErr RSLT
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

has1Dir :: Mbrship -> QRelspec -> Bool
has1Dir mv rc = 1 == length (Map.toList $ Map.filter f rc)
  where f (QVarSpec y) = y == mv
        f _ = False

-- | warning ? treats It like Any
star :: RSLT -> QNode -> QRelspec -> Either DwtErr [Node]
star g qFrom axis = do -- returns one generation, neighbors
  if has1Dir From axis then return ()
     else Left (Invalid, [ErrQRelspec axis]
               , "star: should have only one " ++ show From)
  let forwardRoles = Map.keys $ Map.filter (== QVarSpec To) axis
  axis' <- runReaderT (subQNodeForVars qFrom From axis) g
  rels <- matchQRelspecNodes g axis'
  concat <$> mapM (\rel -> selectRelElts g rel forwardRoles) rels

-- | in r, changes each QVarSpec v to QNodeSpec n
subQNodeForVars :: QNode -> Mbrship -> QRelspec
  -> ReaderT RSLT (Either DwtErr) QRelspec
subQNodeForVars q v r = hoist (prependCaller "subQNodeForVars") $ do
  g <- ask
  n <- lift $ qGet1 g q
  let f (QVarSpec v') = if v == v' then QNodeSpec (At n) else QVarSpec v'
      f x = x -- f needs the v,v' distinction; otherwise v gets masked
  lift $ Right $ Map.map f r

_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- ^ order determines dfs or bfs
  -> RSLT -> QRelspec
  -> [Node] -- ^ pending accumulation
  -> [Node] -- ^ the accumulator
  -> Either DwtErr [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g qdir (n:ns) acc = do
  newNodes <- star g (At n) qdir -- todo speed ? calls has1Dir too much
  _bfsOrDfs collector g qdir (nub $ collector newNodes ns) (n:acc)
    -- todo speed ? discard visited nodes from graph.  (might backfire?)

bfsConcat = _bfsOrDfs (\new old -> old ++ new)
dfsConcat = _bfsOrDfs (\new old -> new ++ old)

dwtDfs :: RSLT -> QRelspec -> [Node] -> Either DwtErr [Node]
dwtDfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> dfsConcat g dir starts []

dwtBfs :: RSLT -> QRelspec -> [Node] -> Either DwtErr [Node]
dwtBfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> bfsConcat g dir starts []
