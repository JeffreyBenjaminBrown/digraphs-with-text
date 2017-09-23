-- | These are "basic" queries in that they mostly don't use QRelspec or
-- QNode.

{-# LANGUAGE FlexibleContexts #-}
module Dwt.Search.Base (
  mkRoleMap -- Node -> [Node] -> RoleMap
  , mkRelspec -- Node -> [Node] -> Relspec
  , mkQRelspec -- unused.  QNode(Tplt) -> [QNode] -> QRelspec
  , getRelNodeSpec -- RSLT -> Node(QRelspec) -> Either DwtErr RelNodeSpec

  , whereis -- RSLT -> Expr -> [Node]
  , tpltAt -- (MonadError DwtErr m) => RSLT -> Node(Tplt) -> m Expr(Tplt)
  , relTplt --                         RSLT -> Node(Rel) -> Either DwtErr Expr
  , selectRelElts -- RSLT -> Node(Rel) -> [RelRole] -> Either DwtErr [Node]
  , validMbrRole -- RSLT -> Node -> RelRole -> Either DwtErr ()
  , collPrinciple -- RSLT -> Node(Coll) -> Either DwtErr Expr(Principle)
  , tplts -- Gr Expr b -> [Node]
  , mbrs -- RSLT -> Node(Rel) -> [Node(Mbr)]
  , users -- Graph gr => gr a b -> Node -> Either DwtErr [Node]
  ) where

import Data.Graph.Inductive (Node, Graph, Gr, lsuc, lpre, lab, labfilter
                            , nodes)
import Dwt.Types
import Dwt.MkTplt (jointsToTplt)
import Dwt.Util (gelemM, listIntersect)
import Dwt.Measure (isCollM, isRelM, tpltArity)
import Dwt.Util (prependCaller)
import qualified Data.Map as Map
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (fromJust)
import Control.Lens ((%~), (.~), _1, _2)


toQRelspec :: QNode -> Either DwtErr QRelspec
toQRelspec (QRel js qs) = Right $ Map.fromList $ t : ms where
  t = (TpltRole, QNodeSpec $ QLeaf $ jointsToTplt js)
  ms = zip (map Mbr [1..]) (map QNodeSpec qs)
toQRelspec x = Left (ConstructorMistmatch, [ErrQNode x], "toQRelspec.")

-- | Applies only when all the nodes the Rel involves are known.
mkRoleMap :: QNode -> [QNode] -> RoleMap
mkRoleMap t ns = Map.fromList $ (TpltRole, t) : mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) ns

mkRelspec :: Node -> [Node] -> Relspec
mkRelspec t ns = Map.fromList $ [(TpltRole, NodeSpec t)] ++ mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpec ns)

mkQRelspec :: QNode -> [QNode] -> QRelspec
mkQRelspec t ns = Map.fromList $ (TpltRole, QNodeSpec t) : mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap QNodeSpec ns)

getRelNodeSpec :: RSLT -> Node -> Either DwtErr RelNodeSpec
getRelNodeSpec g n = prependCaller "getRelNodeSpec: " $ do
  gelemM g n
  case lab g n of
    Just (RelspecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
            f _ = error "getRelNodeSpec: something was not a RelEdge"
    Just _ -> Left (NotRelspecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

-- ====
whereis :: RSLT -> Expr -> [Node] -- TODO ? dependent types: length == 1
whereis g x = nodes $ labfilter (== x) g
 
tpltAt :: (MonadError DwtErr m) => RSLT -> Node -> m Expr
tpltAt g tn = let name = "tpltAt." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, [ErrNode tn], name)
  _       -> throwError (NotTplt, [ErrNode tn], name)

relTplt :: RSLT -> Node -> Either DwtErr Expr
relTplt g relNode = prependCaller "relTplt" $ do
  [n] <- selectRelElts g relNode [TpltRole]
  return $ fromJust $ lab g n

selectRelElts :: RSLT -> Node -> [RelRole] -> Either DwtErr [Node]
selectRelElts g relNode roles = prependCaller "selectRelElts" $ do
  isRelM g relNode
  mapM_  (validMbrRole g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

validMbrRole :: RSLT -> Node -> RelRole -> Either DwtErr ()
validMbrRole g relNode role = prependCaller "validMbrRole: " $ do
  isRelM g relNode >> case role of
    TpltRole -> return ()
    Mbr p -> do
      if p >= 1 then return () else Left err
      t <- relTplt g relNode
      let a = tpltArity t
      if p <= a then return ()
        else Left $ _1 .~ ArityMismatch $ _2 %~ (ErrExpr t:) $ err
  where err = (Invalid, [ErrRelRole role], ".")

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
