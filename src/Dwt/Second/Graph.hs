-- | Initial in the category theory sense of initial object:
-- every module in Dwt.Query will import this one.
{-# LANGUAGE FlexibleContexts #-}
module Dwt.Second.Graph (
  whereis -- RSLT -> Expr -> [Node]
  , tpltAt -- (MonadError DwtErr m) => RSLT -> Node(Tplt) -> m Expr(Tplt)
  , relTplt --                         RSLT -> Node(Rel) -> Either DwtErr Expr
  , selectRelElts -- RSLT -> Node(Rel) -> [RelRole] -> Either DwtErr [Node]
  , validMbrRole -- RSLT -> Node -> RelRole -> Either DwtErr ()
  , collPrinciple -- RSLT -> Node(Coll) -> Either DwtErr Expr(Principle)
  , tplts -- Gr Expr b -> [Node]
  , mbrs -- RSLT -> Node(Rel) -> [Node(Mbr)]
  , users -- Graph gr => gr a b -> Node -> Either DwtErr [Node]
  , exprDepth -- RSLT -> Node -> Level
  ) where

import Data.Graph.Inductive (Node, Graph, Gr, lsuc, lpre, lab, labfilter
                            , nodes, delNode)
import Dwt.Initial.Types
import Dwt.Initial.Util (gelemM, listIntersect, prependCaller)
import Dwt.Initial.Measure (isCollM, isRelM, tpltArity)

import qualified Data.Map as Map
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (fromJust)
import Control.Lens ((%~), (.~), _1, _2)


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

-- | opposites: mbrs, users
  -- though they would not be if Tplts pointed to|had members of their own
mbrs :: RSLT -> Node -> [Node]
mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
  where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

-- | Words and Tplts are used, but are not users. (Rels and Colls use them.)
users :: Graph gr => gr a b -> Node -> Either DwtErr [Node]
users g n = do gelemM g n
               return [m | (m,label@_) <- lpre g n]

-- | == compute the (maximum) depth of an expression
type Gen = (Level,[Node])

exprDepth :: RSLT -> Node -> Level
exprDepth g n = fst $ _exprDepth g (0,[n]) (1,[]) []

-- | When a node's scrs are evaluated, it is removed from the graph,
  -- so only the shortest path to it is evaluated.
  -- Searches BFS.
  -- Does not return a Gen -- those Nodes might be at any depth.
_exprDepth :: RSLT -> Gen -- ^ this gen
                   -> Gen -- ^ next gen
                   -> [Node] -- ^ accum every node visited
                   -> (Level,[Node]) -- ^ depth + the accum
  -- TODO speed ? I kept the 'acc' only because it seemed
  -- maybe eventually useful, but so far it is unused.
_exprDepth _ (d,[])    (_,[]) acc = (d,acc)
_exprDepth g (_,[]) ng@(d, _) acc = -- this Gen is empty, so next replaces it
  _exprDepth g ng (d+1,[]) acc
_exprDepth g (d,n:ns) (d',ns')  acc = let newNodes = mbrs g n in
  _exprDepth (delNode n g) (d,ns) (d', newNodes ++ ns') (n:acc)
  -- we only finish processing n (moving it to acc) after fetching its mbrs
