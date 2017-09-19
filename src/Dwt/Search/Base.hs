-- | These are "basic" queries in that they mostly don't use QRelSpec or
-- QNode.

{-# LANGUAGE FlexibleContexts #-}
module Dwt.Search.Base (
  -- things involving NodeOrVar
  _matchRelSpecNodes -- RSLT -> RelSpec -> Either DwtErr [Node]
    -- critical: the intersection-finding function
  , _matchRelSpecNodesLab -- same, except LNodes
  , _usersInRole -- RSLT -> Node -> RelRole -> Either DwtErr [Node]
  , _mkRelSpec -- Node -> [Node] -> RelSpec

  -- these two might belong elsewhere; they use QRelSpec and|or QNode
  , relNodeSpec -- RSLT -> Node(QRelSpec) -> Either DwtErr RelNodeSpec
  , mkRelSpec -- unused.  QNode(Tplt) -> [QNode] -> QRelSpec

  , whereis -- RSLT -> Expr -> [Node]
  , tpltAt -- (MonadError DwtErr m) => RSLT -> Node(Tplt) -> m Expr(Tplt)
  , relElts -- RSLT -> Node(Rel) -> [RelRole] -> Either DwtErr [Node]
  , validMbrRole -- RSLT -> Node -> RelRole -> Either DwtErr ()
  , relTplt -- RSLT -> Node(Rel) -> Either DwtErr Expr(Tplt)
  , collPrinciple -- RSLT -> Node(Coll) -> Either DwtErr Expr(Principle)
  , tplts -- Gr Expr b -> [Node]
  , mbrs -- RSLT -> Node(Rel) -> [Node(Mbr)]
  , users -- Graph gr => gr a b -> Node -> Either DwtErr [Node]
  ) where

import Dwt.Types
import Dwt.Util (gelemM, listIntersect)
import Dwt.Measure (isCollM, isRelM, tpltArity)
import Dwt.Util (prependCaller)
import Data.Graph.Inductive
import qualified Data.Map as Map
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (fromJust)
import Control.Lens ((%~), (.~), _1, _2)


-- ====
-- | "Concrete" in the sense that it uses Nodes, not QNodes
_matchRelSpecNodes :: RSLT -> RelSpec -> Either DwtErr [Node]
_matchRelSpecNodes g spec = prependCaller "_matchRelSpecNodes: " $ do
  let nodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> _usersInRole g n r) nodeSpecs
  return $ listIntersect nodeListList

-- ifdo speed: this searches for nodes, then searches again for labels
_matchRelSpecNodesLab :: RSLT -> RelSpec -> Either DwtErr [LNode Expr]
_matchRelSpecNodesLab g spec = prependCaller "_matchRelSpecNodesLab: " $ do
  ns <- _matchRelSpecNodes g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- TODO: slow: this looks up each node a second time to find its label
    -- fromJust is safe because _matchRelSpecNodes only returns Nodes in g

-- | Rels using Node n in RelRole r
_usersInRole :: RSLT -> Node -> RelRole -> Either DwtErr [Node]
_usersInRole g n r = prependCaller "usersInRole: " $
  do gelemM g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

-- | Use when all the nodes the Rel involves are known.
_mkRelSpec :: Node -> [Node] -> RelSpec
_mkRelSpec t ns = Map.fromList $ [(TpltRole, NodeSpec t)] ++ mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpec ns)


-- ====
relNodeSpec :: RSLT -> Node -> Either DwtErr RelNodeSpec
relNodeSpec g n = prependCaller "relNodeSpec: " $ do
  gelemM g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
            f _ = error "relNodeSpec: something was not a RelEdge"
    Just _ -> Left (NotRelSpecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

mkRelSpec :: QNode -> [QNode] -> QRelSpec
mkRelSpec t ns = Map.fromList $ (TpltRole, QNodeSpec t) : mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap QNodeSpec ns)

whereis :: RSLT -> Expr -> [Node]
  -- TODO: dependent types. (hopefully, length = 1)
  -- move ? Search.hs
  -- name ? exprOf
whereis g x = nodes $ labfilter (== x) g

tpltAt :: (MonadError DwtErr m) => RSLT -> Node -> m Expr
tpltAt g tn = let name = "tpltAt." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, [ErrNode tn], name)
  _       -> throwError (NotTplt, [ErrNode tn], name)

-- todo: add prependCaller
relElts :: RSLT -> Node -> [RelRole] -> Either DwtErr [Node]
relElts g relNode roles = do
  isRelM g relNode
  mapM_  (validMbrRole g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

validMbrRole :: RSLT -> Node -> RelRole -> Either DwtErr ()
validMbrRole g relNode role = isRelM g relNode >> case role of
  TpltRole -> return ()
  Mbr p -> do
    if p >= 1 then return () else Left err
    t <- relTplt g relNode
    let a = tpltArity t
    if p <= a then return ()
      else Left $ _1 .~ ArityMismatch $ _2 %~ (ErrExpr t:) $ err
  where err = (Invalid, [ErrRelRole role], "validMbrRoleStrErr.")

relTplt :: RSLT -> Node -> Either DwtErr Expr
relTplt g relNode = do isRelM g relNode
                       [n] <- relElts g relNode [TpltRole]
                       return $ fromJust $ lab g n

collPrinciple :: RSLT -> Node -> Either DwtErr Expr
  -- analogous to relTplt
collPrinciple g collNode = do
  prependCaller "collPrincipleDe: " $ isCollM g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

-- ==== .. -> [Node]
tplts :: Gr Expr b -> [Node]
tplts = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False)

-- opposites: mbrs, users
  -- though they would not be if Tplts pointed to|had members of their own
mbrs :: RSLT -> Node -> [Node]
mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
  where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

-- Words and Tplts are used, but are not users. (Rels and Colls use them.)
users :: Graph gr => gr a b -> Node -> Either DwtErr [Node]
users g n = do gelemM g n
               return [m | (m,label@_) <- lpre g n]
