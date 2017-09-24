{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search.QNode (
  playsRoleIn -- RSLT -> RelRole -> Node -> Either DwtErr [Node]
  , qPlaysRoleIn -- RSLT -> RelRole -> QNode -> Either DwtErr [Node]
  , matchRoleMap -- RSLT -> RoleMap -> Either DwtErr [Node]

  , qGet -- RSLT -> QNode -> Either DwtErr [Node]
  , qGetLab -- RSLT -> QNode -> Either DwtErr [LNode Expr]
  , qGet1 -- RSLT -> QNode -> Either DwtErr Node
  , qPutSt -- QNode -> StateT RSLT (Either DwtErr) Node
  , qRegexWord -- RSLT -> String -> [Node]

  , otherDir -- SearchVar -> Either DwtErr SearchVar
  , has1Dir -- SearchVar(the dir it has 1 of) -> RoleMap -> Bool
  , star -- RSLT -> QNode -> RoleMap -> Either DwtErr [Node]
  , subQNodeForVars --QNode(sub this) ->SearchVar(for this) ->RoleMap(in this)
    -- -> ReaderT RSLT (Either DwtErr) RoleMap
  , dwtDfs -- RSLT -> RoleMap -> [Node] -> Either DwtErr [Node]
  , dwtBfs -- same
) where

import Data.Graph.Inductive (Node, LNode, Graph, labfilter, lab, nodes
  , insNode, insEdges, newNodes, labNodes, gelem, lpre)
import Dwt.Types
import Dwt.Edit (insLeaf, insRelSt)
import Dwt.Util (maxNode, dropEdges, fromRight, prependCaller, gelemM
                , listIntersect)
import Dwt.Measure (extractTplt, isAbsent)
import Dwt.Search.Base (mkRoleMap, selectRelElts)

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.Trans.State (StateT, get, put)
import Text.Regex (mkRegex, matchRegex)


-- | Rels using Node n in RelRole r
playsRoleIn :: RSLT -> RelRole -> Node -> Either DwtErr [Node]
playsRoleIn g r n = prependCaller "qPlaysRoleIn: " $
  do gelemM g n -- makes f safe
     return $ f g r n
  where f :: (Graph gr) => gr a RSLTEdge -> RelRole -> Node -> [Node]
        f g r n = [m | (m,r') <- lpre g n, r' == RelEdge r]

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
  if founds /= [] then return ()
                  else Left (NothingSpecified, [ErrRoleMap m], ".")
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
_qGet :: Eq x => (RSLT -> Node -> x) -- ^ gets what's there; used for At.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x]) -- ^ nodes or labNodes; used for QLeaf
  -> (RSLT -> RoleMap -> Either DwtErr [x])
    -- ^ matchRoleMap or matchRoleMapLab; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGet _ _ _ _ Absent = Left (Impossible, [ErrQNode Absent], "qGet.")
_qGet f _ _ g q@(At n) = if gelem n g then return [f g n]
  else Left (FoundNo, [ErrQNode q], "_qGet.")
_qGet _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g q@(QRel _ qms) = prependCaller "_qGet: " $ do
  t <- extractTplt q
  let m = mkRoleMap (QLeaf t) $ filter (not . (== Absent)) qms
    -- because non-interior Joints require the use of Absent
  f g m
_qGet a b c g (QAnd qs) = listIntersect <$> mapM (_qGet a b c g) qs
_qGet a b c g (QOr qs) = nub . concat <$> mapM (_qGet a b c g) qs

qGet :: RSLT -> QNode -> Either DwtErr [Node]
qGet = _qGet (\_ n -> n) nodes matchRoleMap

qGetLab :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLab = _qGet f labNodes matchRoleMapLab where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1 :: RSLT -> QNode -> Either DwtErr Node
qGet1 g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right _ -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = [ErrQNode q]

qPutSt :: QNode -> StateT RSLT (Either DwtErr) Node
qPutSt Absent = lift $ Left (Impossible, [ErrQNode Absent], "qPutSt.")
qPutSt i@(QRel _ qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  -- let tag = prependCaller "qPutSt: " -- TODO: use
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


-- ==== Branches
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
