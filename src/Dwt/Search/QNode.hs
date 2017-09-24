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
) where

import Data.Graph.Inductive (Node, LNode, Graph, labfilter, lab, nodes
                            , labNodes, gelem, lpre)
import Dwt.Types
import Dwt.Edit (insLeaf, insRelSt)
import Dwt.Util (maxNode, dropEdges, fromRight, prependCaller, gelemM
                , listIntersect)
import Dwt.Measure (extractTplt, isAbsent)
import Dwt.Search.Base (mkRoleMap)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put)
import qualified Data.Maybe as Mb
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Map as Map


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
_qGet :: (RSLT -> Node -> x) -- ^ gets what's there; used for At.
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
