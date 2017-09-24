module Dwt.Search.Branch (
  otherDir -- SearchVar -> Either DwtErr SearchVar
  , has1Dir -- SearchVar(the dir it has 1 of) -> RoleMap -> Bool
  , star -- RSLT -> QNode -> RoleMap -> Either DwtErr [Node]
  , subQNodeForVars --QNode(sub this) ->SearchVar(for this) ->RoleMap(in this)
    -- -> ReaderT RSLT (Either DwtErr) RoleMap
  , dwtDfs -- RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
  , dwtBfs -- same
) where

import Data.Graph.Inductive (Node, insNode, insEdges, newNodes, lab)
import Dwt.Types
import Dwt.Search.Base (selectRelElts)
import Dwt.Search.QNode (qGet1, matchRoleMap)

import Dwt.Util (listIntersect, prependCaller, gelemM)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (nub)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Morph (hoist)


-- | swap Up and Down, or err
otherDir :: SearchVar -> Either DwtErr SearchVar
otherDir From = Right To
otherDir To = Right From
otherDir Any = Right Any
otherDir It = Left (ConstructorMistmatch, [ErrSearchVar It], "otherDir: Accepts To, From or Any.") -- todo ? just return Right It

has1Dir :: SearchVar -> RoleMap -> Bool
has1Dir mv rc = 1 == length (Map.toList $ Map.filter f rc)
  where f (QVar y) = y == mv
        f _ = False

-- | warning ? treats It like Any
star :: RSLT -> QNode -> RoleMap -> Either DwtErr [Node]
star g qFrom axis = do -- returns one generation of some kind of neighbor
  if has1Dir From axis then return ()
     else Left (Invalid, [ErrRoleMap axis]
               , "star: should have only one " ++ show From)
  let forwardRoles = Map.keys $ Map.filter (== QVar To) axis
  axis' <- runReaderT (subQNodeForVars qFrom From axis) g
  rels <- matchRoleMap g axis'
  concat <$> mapM (\rel -> selectRelElts g rel forwardRoles) rels

-- | in r, changes each QVarSpec v to QNodeSpec n
subQNodeForVars :: QNode -> SearchVar -> RoleMap
  -> ReaderT RSLT (Either DwtErr) RoleMap
subQNodeForVars q v r = hoist (prependCaller "subQNodeForVars") $ do
  g <- ask
  n <- lift $ qGet1 g q
  let f (QVar v') = if v == v' then At n else QVar v'
      f x = x -- f needs the v,v' distinction; otherwise v gets masked
  lift $ Right $ Map.map f r

_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- ^ order determines dfs or bfs
  -> RSLT -> RoleMap
  -> [Node] -- ^ searching from these (it gets added to and depleted)
  -> [Node] -- ^ the accumulator (it only gets added to)
  -> Either DwtErr [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g qdir (n:ns) acc = do
  newNodes <- star g (At n) qdir -- todo speed ? calls has1Dir too much
  _bfsOrDfs collector g qdir (nub $ collector newNodes ns) (n:acc)
    -- todo speed ? discard visited nodes from graph.  (might backfire?)

bfsConcat = _bfsOrDfs (\new old -> old ++ new)
dfsConcat = _bfsOrDfs (\new old -> new ++ old)

dwtDfs :: RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
dwtDfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> dfsConcat g dir starts []

dwtBfs :: RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
dwtBfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> bfsConcat g dir starts []
