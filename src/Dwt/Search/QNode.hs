{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search.QNode (
  playsRoleIn -- RSLT -> RelRole -> Node -> Either DwtErr [Node]
  , qPlaysRoleIn -- RSLT -> RelRole -> QNode -> Either DwtErr [Node]
  , matchRelspecNodes -- RSLT -> Relspec -> Either DwtErr [Node]
    -- critical: the intersection-finding function
  , matchQRelspecNodes -- RSLT -> QRelspec -> Either DwtErr [Node]
  , matchRelspecNodesLab -- same, except LNodes
  , matchQRelspecNodesLab -- same, except LNodes

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
import Dwt.Search.Base (mkRelspec, mkQRelspec)

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
  $ qGet1 g q >>= playsRoleIn g r

matchRelspecNodes :: RSLT -> Relspec -> Either DwtErr [Node]
matchRelspecNodes g spec = prependCaller "matchRelspecNodes: " $ do
  let nodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVar)]
  if nodeSpecs /= [] then return ()
    else Left (NothingSpecified, [ErrRelspec spec], "matchRelspecNodes")
  nodeListList <- mapM (\(r,NodeSpec n) -> playsRoleIn g r n) nodeSpecs
  return $ listIntersect nodeListList
 
matchQRelspecNodes :: RSLT -> QRelspec -> Either DwtErr [Node]
matchQRelspecNodes g spec = prependCaller "matchQRelspecNodes: " $ do
  let nodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of QNodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,QNodeOrVar)]
  if nodeSpecs /= [] then return ()
    else Left (NothingSpecified, [ErrQRelspec spec], "matchRelspecNodes")
  nodeListList <- mapM (\(r,QNodeSpec n) -> qPlaysRoleIn g r n) nodeSpecs
  return $ listIntersect nodeListList

matchRoleMap :: RSLT -> RoleMap -> Either DwtErr [Node]
matchRoleMap g m = prependCaller "matchRoleMap: " $ do
  let maybeFind :: QNode -> Either DwtErr (Maybe [Node])
      maybeFind (QVar _) = Right Nothing
      maybeFind q = Just <$> qGet g q
      catSndMaybes :: [(RelRole, Maybe [Node])] -> [(RelRole, [Node])]
      catSndMaybes = foldl f [] where f acc (a, Just b) = (a,b) : acc
                                      f acc (a, Nothing) = acc
  founds <- catSndMaybes . Map.toList <$> mapM maybeFind m
  if founds /= [] then return ()
    else Left (NothingSpecified, [ErrRoleMap m], "matchRoleMap.")
  return $ listIntersect $ map snd founds

matchRelspecNodesLab :: RSLT -> Relspec -> Either DwtErr [LNode Expr]
matchRelspecNodesLab g spec = prependCaller "matchRelspecNodesLab: " $ do
  ns <- matchRelspecNodes g spec
  return $ zip ns $ map (Mb.fromJust . lab g) ns
    -- TODO: slow: this looks up each node a second time to find its label
    -- fromJust is safe because matchRelspecNodes only returns Nodes in g

matchQRelspecNodesLab :: RSLT -> QRelspec -> Either DwtErr [LNode Expr]
matchQRelspecNodesLab g spec = prependCaller "matchQRelspecNodesLab: " $ do
  ns <- matchQRelspecNodes g spec
  return $ zip ns $ map (Mb.fromJust . lab g) ns
    -- TODO speed: this looks up each node twice
    -- fromJust is safe because matchQRelspecNodes only returns Nodes in g


-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.whereis :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- ^ x herein is either Node or LNode Expr. TODO: use a typeclass
_qGet :: (RSLT -> Node -> x) -- ^ gets what's there; used for At.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x]) -- ^ nodes or labNodes; used for QLeaf
  -> (RSLT -> QRelspec -> Either DwtErr [x])
    -- ^ matchQRelspecNodes or matchQRelspecNodesLab; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGet _ _ _ _ Absent = Left (Impossible, [ErrQNode Absent], "qGet.")
_qGet f _ _ g q@(At n) = if gelem n g then return [f g n]
  else Left (FoundNo, [ErrQNode q], "_qGet.")
_qGet _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g q@(QRel _ qms) = prependCaller "_qGet: " $ do
  t <- extractTplt q
  let qRelspec = mkQRelspec (QLeaf t) qms
  f g qRelspec

qGet :: RSLT -> QNode -> Either DwtErr [Node]
qGet = _qGet (\_ n -> n) nodes matchQRelspecNodes

qGetLab :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLab = _qGet f labNodes matchQRelspecNodesLab where
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

