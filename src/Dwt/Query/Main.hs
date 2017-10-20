{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Query.Main (
  pathsToIts -- QNode -> Either DwtErr (S.Set PathInExpr)
  , subExpr -- RSLT -> Node -> PathInExpr -> Either DwtErr Node
  , its -- RSLT -> QNode -> Node -> Either DwtErr [Node]

  , playsRoleIn -- RSLT -> RelRole -> Node -> Either DwtErr [Node]
  , qPlaysRoleIn -- RSLT -> RelRole -> QNode -> Either DwtErr [Node]
  , matchRoleMap -- RSLT -> RoleMap -> Either DwtErr [Node]

  , qGet -- RSLT -> QNode -> Either DwtErr [Node]
  , qGet1 -- RSLT -> QNode -> Either DwtErr Node
  , qPutSt -- QNode -> StateT RSLT (Either DwtErr) Node
  , qRegexWord -- RSLT -> String -> [Node]

  , otherDir -- SearchVar -> Either DwtErr SearchVar
  , has1Dir -- SearchVar(the dir it has 1 of) -> RoleMap -> Bool
  , star -- RSLT -> RoleMap -> QNode -> Either DwtErr [Node]
  , subQNodeForVars --QNode(sub this) ->SearchVar(for this) ->RoleMap(in this)
    -- -> ReaderT RSLT (Either DwtErr) RoleMap
  , dwtDfs_unlim    -- RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
  , dwtBfs_unlim    -- RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]

  -- | = kind of silly
  , dwtDfsLab_unlim -- RSLT -> RoleMap -> [Node] -> Either DwtErr [LNode Expr]
  , dwtBfsLab_unlim -- RSLT -> RoleMap -> [Node] -> Either DwtErr [LNode Expr]
) where

import Data.Graph.Inductive (Node, LNode, Graph, labfilter, lab, nodes
  , insNode, insEdges, newNodes, labNodes, gelem, lpre, lsuc)
import Dwt.Initial.Types
import Dwt.Edit (insLeaf, insRelSt)
import Dwt.Initial.Util (maxNode, dropEdges, prependCaller, gelemM
                , listIntersect, listDiff, nodeToLNodeUsf)
import Dwt.Initial.Measure (extractTplt, isAbsent)
import Dwt.Second.QNode (qNodeIsTop, mkRoleMap)
import Dwt.Second.Graph (selectRelElts, users)

import Data.Either (isRight)
import Data.List (nub, sortOn)
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import qualified Data.Set as S
import Control.Lens
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Trans.State (StateT, get, put)
import Text.Regex (mkRegex, matchRegex)


-- The pathsToIts algorithm
  -- pair each member with an indicator of what member it is, e.g.
  --   (/it ##is good #for ed ##when ed #is /it)
  --   -> [(Mbr 1, /it), (Mbr 2, good #for ed), (Mbr 3, ed #is /it)]
  -- replace each member with pathsToIts called on that member
  --   -> [( Mbr 1, Right $ Set.fromList [[]] )
  --      ,( Mbr 2, Left (FoundNo,..))
  --      ,( Mbr 3, pathsToIts (ed #is /it)) ]
  --   =  [( Mbr 1, Right $ Set.fromList [[]] )
  --      ,( Mbr 2, Left (FoundNo,..))
  --      ,( Mbr 3, Right $ Set.fromList [[Mbr 2]]) ]
  -- discard each pair with a right side of (Left (FoundNo ...))
  -- prepend the left side of each pair to each elt in the right side
  --   -> [Right $ Set.fromList [[Mbr 1]]
  --      ,Right $ Set.fromList [[Mbr 3, Mbr 2]]]
  -- make a set that's the union of each member of that list
  --   -> Right $ Set.fromList [[Mbr 1], [Mbr 3, Mbr 2]]

pathsToIts :: QNode -> Either DwtErr (S.Set PathInExpr)
pathsToIts (QRel _ _ qs) = do
  let w = zip [Mbr i | i <- [1..]]
        $ filter (not . qNodeIsTop)
        $ filter (not . isAbsent) qs
        :: [(RelRole, QNode)]
      x,y  :: [(RelRole, Either DwtErr (S.Set PathInExpr))]
      x = map (\(a,b) -> (a, pathsToIts b)) w
      y = filter (isRight . snd) x
      z = map (\(a, Right b) -> S.map (a:) b) y
  return $ S.unions z
pathsToIts (QVar It) = Right $ S.fromList [[]]
pathsToIts _ = Left (FoundNo,[],"") -- not an error

subExpr :: RSLT -> Node -> PathInExpr -> Either DwtErr Node
subExpr _ n [] = Right n
subExpr g n (s:ss) = 
  -- todo ? if Left, return full original path, not just bad subpath
  case map fst $ filter ((== RelEdge s) . snd) $ lsuc g n of
    [n'] -> subExpr g n' ss
    [] -> Left $ error & errBase .~ ConstructorMistmatch
    _ -> Left $ error & errBase .~ FoundMany
  where error = (Invalid,[ErrPathInExpr (s:ss), ErrNode n], "subExpr.")

its :: RSLT -> QNode -> Node -> Either DwtErr [Node]
its g q superExpr = do ps <- pathsToIts q
                       mapM (subExpr g superExpr) $ S.toList ps

-- | Rels using Node n in RelRole r
playsRoleIn :: RSLT -> RelRole -> Node -> Either DwtErr [Node]
playsRoleIn g r n = prependCaller "qPlaysRoleIn: " $
  do gelemM g n -- makes f safe
     return $ f g r n
  where f :: (Graph gr) => gr a RSLTEdge -> RelRole -> Node -> [Node]
        f g r n = [m | (m,r') <- lpre g n, r' == RelEdge r]

-- TODO: convert all search functions from List to Set, like this one
-- difficulty: Set has no Traversable instance; need (fromlist . _ . tolist)
playsRoleInSetRefactor :: RSLT -> RelRole -> Node -> Either DwtErr (S.Set Node)
playsRoleInSetRefactor g r n = prependCaller "qPlaysRoleIn: " $
  do gelemM g n -- makes f safe
     return $ f g r n
  where f :: (Graph gr) => gr a RSLTEdge -> RelRole -> Node -> S.Set Node
        f g r n = S.fromList [m | (m,r') <- lpre g n, r' == RelEdge r]

qPlaysRoleIn :: RSLT -> RelRole -> QNode -> Either DwtErr [Node]
qPlaysRoleIn g r q = prependCaller "qPlaysRoleIn: "
  $ concat <$> (qGet g q
                >>= (mapM $ playsRoleIn g r))

matchRoleMap :: RSLT -> RoleMap -> Either DwtErr [Node]
matchRoleMap g m = prependCaller "matchRoleMap: " $ do
  let maybeFind :: (RelRole, QNode) -> Either DwtErr (Maybe [Node])
      maybeFind (_, QVar _) = Right Nothing
      maybeFind (r, q) = Just <$> qPlaysRoleIn g r q
  founds <- Mb.catMaybes <$> (mapM maybeFind $ Map.toList m)
  if null founds then Left (NothingSpecified, [ErrRoleMap m], ".")
                 else return ()
  return $ listIntersect founds

matchRoleMapLab :: RSLT -> RoleMap -> Either DwtErr [LNode Expr]
matchRoleMapLab g rm = prependCaller "matchRoleMapLab: " $ do
    -- TODO: slow: this looks up each node a second time to find its label
  ns <- matchRoleMap g rm
  return $ zip ns $ map (Mb.fromJust . lab g) ns
    -- fromJust is safe because matchRoleMap only returns Nodes in g

-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.whereis :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- ^ x herein is either Node or LNode Expr. TODO: use a typeclass
qGet :: RSLT -> QNode -> Either DwtErr [Node]
qGet _ Absent = Left (Impossible, [ErrQNode Absent], "qGet.")
qGet _ q@(QVar _) = Left (Impossible, [ErrQNode q], "qGet.")
qGet g q@(At n) = if gelem n g then return [n]
                  else Left (FoundNo, [ErrQNode q], "qGet.")
qGet g (QLeaf l) = return $ nodes $ labfilter (==l) $ dropEdges g
qGet g q@(QRel isTop _ qms) = prependCaller "qGet: " $ do
  t <- extractTplt q
  let m = mkRoleMap (QLeaf t) $ filter (not . (== Absent)) qms
    -- because non-interior Joints require the use of Absent
  ns <- matchRoleMap g m
  ps <- pathsToIts q
  if isTop && (not . null) ps then nub . concat <$> mapM (its g q) ns
                              else return ns
qGet g (QAnd qs) = listIntersect <$> mapM (qGet g) qs
qGet g (QOr qs) = nub . concat <$> mapM (qGet g) qs
qGet g (QDiff keep drop) = listDiff <$> qGet g keep <*> qGet g drop
qGet g (QBranch dir q) = qGet g q >>= dwtDfs_unlim g dir

qGet1 :: RSLT -> QNode -> Either DwtErr Node
qGet1 g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right _ -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = [ErrQNode q]

qPutSt :: QNode -> StateT RSLT (Either DwtErr) Node
qPutSt Absent = lift $ Left (Impossible, [ErrQNode Absent], "qPutSt.")
qPutSt i@(QRel _ _ qms) = hoist (prependCaller "qPutSt: ") $ do
  g <- get
  case qGet1 g i of
    Right n -> return n
    Left e@(FoundMany,_,_) -> lift $ Left e
    Left (FoundNo,_,_) -> do
      -- TODO ? more efficient ? return even the half-completed state
      t <- lift $ extractTplt i
      tnode <- qPutSt $ QLeaf t
      ms <- mapM qPutSt $ filter (not . isAbsent) qms
      insRelSt tnode ms
qPutSt (At n) = lift $ Right n
qPutSt q@(QLeaf x) = get >>= \g -> case qGet1 g q of
  Right n            -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
                        in put g' >> lift (Right $ maxNode g')
  Left e             -> lift $ prependCaller "qPutSt: " $ Left e


-- == Regex
qRegexWord :: RSLT -> String -> [Node]
qRegexWord g s = nodes $ labfilter f $ dropEdges g
  where r = mkRegex s
        f (Word t) = Mb.isJust $ matchRegex r t
        f _ = False


-- ====== Branches
-- ==== Utilities for branches
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

-- | in r, changes each QVarSpec v to QNodeSpec n
subQNodeForVars :: QNode -> SearchVar -> RoleMap
  -> ReaderT RSLT (Either DwtErr) RoleMap
subQNodeForVars q v r = hoist (prependCaller "subQNodeForVars") $ do
  g <- ask
  n <- lift $ qGet1 g q
  let f (QVar v') = if v == v' then At n else QVar v'
      f x = x -- f needs the v,v' distinction; otherwise v gets masked
  lift $ Right $ Map.map f r

-- ==== star, bfs, dfs
-- | warning ? treats It like Any
star :: RSLT -> RoleMap -> QNode -> Either DwtErr [Node]
star g (Map.null -> True) qFrom = do from <- qGet g qFrom
                                     concat <$> mapM (users g) from
star g axis qFrom = do -- returns one generation of some kind of neighbor
  if has1Dir From axis then return ()
     else Left (Invalid, [ErrRoleMap axis]
               , "star: should have exactly one " ++ show From)
  let forwardRoles = Map.keys $ Map.filter (== QVar To) axis
  axis' <- runReaderT (subQNodeForVars qFrom From axis) g
  rels <- matchRoleMap g axis'
  concat <$> mapM (\rel -> selectRelElts g rel forwardRoles) rels

---- == todo: dfs, bfs, depth-limited
-- Rather than [Node], return Map Node [Level], where each [Level] is (at the very end) sorted.

-- == dfs, bfs, no depth limit
_bfsOrDfs_unlim :: ([Node] -> [Node] -> [Node]) -- ^ order determines dfs or bfs
  -> RSLT -> RoleMap
  -> [Node] -- ^ searching from these (it gets added to and depleted)
  -> [Node] -- ^ the accumulator (it only gets added to)
  -> Either DwtErr [Node]
_bfsOrDfs_unlim _ _ _ [] acc = return acc
_bfsOrDfs_unlim collector g qdir (n:ns) acc = do
  newNodes <- star g qdir $ At n -- todo speed ? calls has1Dir too much
  _bfsOrDfs_unlim collector g qdir (nub $ collector newNodes ns) (n:acc)
    -- todo speed ? discard visited nodes from graph.  (might backfire?)
    -- todo speed ? run nub once, rather than each pass. except ...
      -- if I do that, the order changes

_dfsConcat_unlim = (\new old -> new ++ old)
_bfsConcat_unlim = (\new old -> old ++ new)

dwtDfs_unlim :: RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
dwtDfs_unlim g dir starts =
  do mapM_ (gelemM g) $ starts
     let f = _bfsOrDfs_unlim _dfsConcat_unlim g dir starts []
     (nub . reverse) <$> f

dwtBfs_unlim :: RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
dwtBfs_unlim g dir starts =
  do mapM_ (gelemM g) $ starts
     let f = _bfsOrDfs_unlim _bfsConcat_unlim g dir starts []
     (nub . reverse) <$> f

dwtDfsLab_unlim :: RSLT -> RoleMap -> [Node] -> Either DwtErr [LNode Expr]
dwtDfsLab_unlim g dir starts =
  do mapM_ (gelemM g) $ starts
     let f = _bfsOrDfs_unlim _dfsConcat_unlim g dir starts []
     map (nodeToLNodeUsf g) . nub . reverse <$> f

dwtBfsLab_unlim :: RSLT -> RoleMap -> [Node] -> Either DwtErr [LNode Expr]
dwtBfsLab_unlim g dir starts =
  do mapM_ (gelemM g) $ starts
     let f = _bfsOrDfs_unlim _bfsConcat_unlim g dir starts []
     map (nodeToLNodeUsf g) . nub . reverse <$> f
