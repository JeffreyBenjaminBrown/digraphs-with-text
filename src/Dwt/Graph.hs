{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Graph (
  insRelUsf
  , insRel
  , insRelSt
  , insColl
  , mkRelSpec
  , relNodeSpec
  , chLeaf
  , chRelRole
  , whereis
  , tpltAt
  , relElts
  , validRole
  , relTplt
  , collPrinciple
  , rels, mbrs
  , users
  ) where

import Dwt.Types
import Dwt.Leaf
import Dwt.Util
import Data.Graph.Inductive hiding (lift)
import Data.Either (partitionEithers)
import Data.List (intersect, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (mapM_)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Text (pack, unpack, strip, splitOn)
import Control.Lens hiding ((&))

-- ======== build
insRel :: Node -> [Node] -> RSLT -> Either DwtErr RSLT
insRel template mbrs g =
  do mapM_ (gelemM g) $ template:mbrs
     tplt <- tpltAt g template
     mbrListMatchesTpltArity mbrs tplt
     return $ addMbrs (zip mbrs [1..tpltArity tplt]) $ addTplt g
  where newNode = head $ newNodes 1 g
        addMbrs []     g = g
        addMbrs (p:ps) g = addMbrs ps $ insEdge
          (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
        addTplt = insEdge (newNode, template, RelEdge TpltRole)
                  . insNode (newNode, Rel) :: RSLT -> RSLT

insRelSt :: Node -> [Node] -> StateT RSLT (Either DwtErr) Node
insRelSt template mbrs =
  do g <- get
     let newNode = head $ newNodes 1 g
         addMbrs []     g = g
         addMbrs (p:ps) g = addMbrs ps $ insEdge
           (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
         addTplt = insEdge (newNode, template, RelEdge TpltRole)
           . insNode (newNode, Rel) :: RSLT -> RSLT
     lift $ mapM_ (gelemM g) $ template:mbrs
     tplt <- tpltAt g template
     mbrListMatchesTpltArity mbrs tplt
     modify $ addMbrs (zip mbrs [1..tpltArity tplt]) . addTplt
     g' <- get
     return $ maxNode g'

-- | Deprecated
insColl :: (Maybe Node) -- title|principle, e.g. "alternatives"
        -> [Node] -> RSLT -> Either DwtErr RSLT
insColl mt ns g = do
  mapM_ (gelemM g) ns
  let newNode = head $ newNodes 1 g
      nameEdges = case mt of
        Nothing -> []
        Just tn -> [(newNode, tn,CollEdge CollPrinciple)]
      newEdges = nameEdges ++
        map (\n -> (newNode, n, CollEdge CollMbr)) ns
  return $ insEdges newEdges $ insNode (newNode,Coll) g

-- | Covers the case where all the nodes the Rel involves are known.
-- | For a framework involving uncertainty, see Dwt.Search.QNode
mkRelSpec :: Node -> [Node] -> RelSpec
mkRelSpec t ns = Map.fromList $ [(TpltRole, NodeSpec t)] ++ mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpec ns)

mkRelSpecQ :: QNode -> [QNode] -> RelSpecQ
mkRelSpecQ t ns = Map.fromList $ [(TpltRole, NodeSpecQ t)] ++ mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpecQ ns)

-- pitfall: does not need conversion to QNode format
relNodeSpec :: RSLT -> Node -> Either DwtErr RelNodeSpec
relNodeSpec g n = prependCaller "relNodeSpec: " $ do
  gelemM g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
    Just _ -> Left
      (NotRelSpecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

-- ======== edit (but not insert)
chLeaf :: RSLT -> Node -> Expr -> Either DwtErr RSLT
chLeaf g n e' = prependCaller "chLeaf: " $ do
  let me = lab g n
  case me of
    Just e@(isLeaf -> True) -> if areLikeExprs e e' then return ()
      else Left (ConstructorMistmatch, [], ".")
    Nothing -> Left (FoundNo, [ErrNode n], ".")
    _       -> Left (NotLeaf, [ErrNode n], ".")
  return $ _chLeafUsf g n e'

_chLeafUsf :: RSLT -> Node -> Expr -> RSLT
_chLeafUsf g n newExpr = let (Just (a,b,c,d),g') = match n g
  in (a,b,newExpr,d) & g'

chRelRole :: RSLT -> Node -> Node -> RelRole -> Either DwtErr RSLT
chRelRole g user newMbr role = do
  isRelM g user
  gelemM g newMbr
  let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      err = (Invalid, [ErrNode user, ErrRelRole role], "chRelRole.")
  case candidates of
    [] -> Left $ _1 .~ FoundNo $ err
    [a] -> return $ delLEdge (user,a,RelEdge role)
           $ insEdge (user,newMbr,RelEdge role) g
    _ -> Left $ _1 .~ FoundMany $ err

-- ======== query
-- ==== more complex ("locate"?) queries
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
  mapM_  (validRole g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

validRole :: RSLT -> Node -> RelRole -> Either DwtErr ()
validRole g relNode role = isRelM g relNode >> case role of
  TpltRole -> return ()
  Mbr p -> do
    if p >= 1 then return () else Left err
    t <- relTplt g relNode
    let a = tpltArity t
    if p <= a then return ()
      else Left $ _1 .~ ArityMismatch $ _2 %~ (ErrExpr t:) $ err
  where err = (Invalid, [ErrRelRole role], "validRoleStrErr.")

relTplt :: RSLT -> Node -> Either DwtErr Expr -- unsafe
  -- might not be called on a template
relTplt g relNode = do
  [n] <- relElts g relNode [TpltRole]
  return $ fromJust $ lab g n

collPrinciple :: RSLT -> Node -> Either DwtErr Expr
  -- analogous to relTplt
collPrinciple g collNode = do
  prependCaller "collPrincipleDe: " $ isCollM g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

-- ==== .. -> [Node]
rels :: Gr Expr b -> [Node]
rels = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False)

-- opposites: mbrs, users
  -- though they would not be if Tplts pointed to|had members of their own
mbrs :: RSLT -> Node -> [Node]
mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
  where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

-- Words and Tplts are used, but are not users. (Rels and Colls use them.)
users :: Graph gr => gr a b -> Node -> Either DwtErr [Node]
users g n = do gelemM g n
               return [m | (m,label@_) <- lpre g n]

-- ======== using directions (RelSpecs)
-- todo ? 1Dir because it should have one such direction. I forget why.
  -- clarif: if I want a generation in the Down direction of the rel "has/",
  -- the RelSpec has to have only one Up variable.
-- TODO ? check: Up|Down good, Any|It bad
  -- fork1Up uses otherDir, so it will catch those errors, but obscurely

-- ========= Deprecated
insRelUsf :: Node -> [Node] -> RSLT -> RSLT
insRelUsf t ns g = case insRel t ns g of
  Left s -> error $ show s
  Right r -> r
