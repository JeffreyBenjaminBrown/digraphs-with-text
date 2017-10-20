{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Edit (
  insLeaf -- Expr -> RSLT -> RSLT
  , insRelUsf -- Node(Tplt) -> [Node] -> RSLT -> RSLT
  , insRel -- Node(Tplt) -> [Node] -> RSLT -> Either DwtErr RSLT
  , insRelSt -- Node(Tplt) -> [Node] -> StateT RSLT (Either DwtErr) Node(Rel)
  , insColl -- (Maybe Node)(principle) -> [Node] -> RSLT -> Either DwtErr RSLT

  -- unused
  -- edit in place (as opposed to insert)
  , chLeaf -- RSLT -> Node -> Expr -> Either DwtErr RSLT
  , chRelRole --RSLT -> Node(Rel) -> Node(new Mbr) -> RelRole -> Either DwtErr RSLT
  ) where

import Dwt.Initial.Types
import Dwt.Initial.Measure
import Dwt.Initial.Util
import Dwt.Second.Graph (tpltAt)
import Data.Graph.Inductive
import Control.Monad (mapM_)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens hiding ((&))


insLeaf :: Expr -> RSLT -> RSLT
insLeaf e g = case isLeaf e of
  True -> insNode (newAddr, e) g where [newAddr] = newNodes 1 g
  False -> error $ "insLeaf: " ++ show e ++ "is not a leaf."

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
_chLeafUsf g n newExpr = let (Just (a,b,_,d),g') = match n g
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

-- ========= Deprecated
insRelUsf :: Node -> [Node] -> RSLT -> RSLT
insRelUsf t ns g = case insRel t ns g of
  Left s -> error $ show s
  Right r -> r
