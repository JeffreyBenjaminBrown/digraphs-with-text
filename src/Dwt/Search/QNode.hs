{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search.QNode (
  playsRoleIn -- RSLT -> Node -> RelRole -> Either DwtErr [Node]
  , qPlaysRoleIn -- RSLT -> QNode -> RelRole -> Either DwtErr [Node]
  , matchRelSpecNodes -- RSLT -> RelSpec -> Either DwtErr [Node]
    -- critical: the intersection-finding function
  , matchQRelSpecNodes -- RSLT -> QRelSpec -> Either DwtErr [Node]
  , matchRelSpecNodesLab -- same, except LNodes
  , matchQRelSpecNodesLab -- same, except LNodes

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
import Dwt.Search.Base (mkRelSpec)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put)
import qualified Data.Maybe as Mb
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Map as Map


-- | Rels using Node n in RelRole r
playsRoleIn :: RSLT -> Node -> RelRole -> Either DwtErr [Node]
playsRoleIn g n r = prependCaller "qPlaysRoleIn: " $
  do gelemM g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

qPlaysRoleIn :: RSLT -> QNode -> RelRole -> Either DwtErr [Node]
qPlaysRoleIn g q r = prependCaller "qPlaysRoleIn: "
  $ qGet1 g q >>= \n -> playsRoleIn g n r

matchRelSpecNodes :: RSLT -> RelSpec -> Either DwtErr [Node]
matchRelSpecNodes g spec = prependCaller "matchRelSpecNodes: " $ do
  let nodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> playsRoleIn g n r) nodeSpecs
  return $ listIntersect nodeListList

matchQRelSpecNodes :: RSLT -> QRelSpec -> Either DwtErr [Node]
matchQRelSpecNodes g spec = prependCaller "matchQRelSpecNodes: " $ do
  let nodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of QNodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,QNodeOrVar)]
  nodeListList <- mapM (\(r,QNodeSpec n) -> qPlaysRoleIn g n r) nodeSpecs
  return $ listIntersect nodeListList

matchRelSpecNodesLab :: RSLT -> RelSpec -> Either DwtErr [LNode Expr]
matchRelSpecNodesLab g spec = prependCaller "matchRelSpecNodesLab: " $ do
  ns <- matchRelSpecNodes g spec
  return $ zip ns $ map (Mb.fromJust . lab g) ns
    -- TODO: slow: this looks up each node a second time to find its label
    -- fromJust is safe because matchRelSpecNodes only returns Nodes in g

matchQRelSpecNodesLab :: RSLT -> QRelSpec -> Either DwtErr [LNode Expr]
matchQRelSpecNodesLab g spec = prependCaller "matchQRelSpecNodesLab: " $ do
  ns <- matchQRelSpecNodes g spec
  return $ zip ns $ map (Mb.fromJust . lab g) ns
    -- TODO speed: this looks up each node twice
    -- fromJust is safe because matchQRelSpecNodes only returns Nodes in g


-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.whereis :: RSLT -> Expr -> [Node] -- hopefully length = 1

_qGet :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErr [x])
    -- | matchRelSpecNodes or matchRelSpecNodesLab; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGet _ _ _ _ Absent = Left (Impossible, [ErrQNode Absent], "qGet.")
_qGet f _ _ g (At n) = return $ if gelem n g then [f g n] else []
_qGet _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g q@(QRel _ qms) = prependCaller "_qGet: " $ do
  t <- extractTplt q
  tnode <- qGet1 g (QLeaf t) -- todo ? multiple qt, qms matches
  ms <- mapM (qGet1 g) qms
  let relspec = mkRelSpec tnode ms
  f g relspec

qGet :: RSLT -> QNode -> Either DwtErr [Node]
qGet = _qGet (\_ n -> n) nodes matchRelSpecNodes

qGetLab :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLab = _qGet f labNodes matchRelSpecNodesLab where
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
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutSt: " $ Left e


-- == Regex
qRegexWord :: RSLT -> String -> [Node]
qRegexWord g s = nodes $ labfilter f $ dropEdges g
  where r = mkRegex s
        f (Word t) = Mb.isJust $ matchRegex r t
        f _ = False

