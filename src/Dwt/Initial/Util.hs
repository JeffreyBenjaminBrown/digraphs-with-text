{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Initial.Util (
  -- | DwtErr
  printDwtErr

   -- | lists
  , listIntersect, listDiff

  -- | graphs & monads
  , hasLEdgeM
  , gelemM

  -- | graphs
  , maxNode, dropEdges, negateGraph, compressGraph, joinGraphs
  , replaceNode, nodeToLNodeUsf

   -- | Either
  , fr, fl
  , prependCaller
  ) where

import Data.Graph.Inductive
import Dwt.Initial.Types (RSLT, RSLTEdge
                 , DwtErr(..), ErrBase(..), ErrOpt(..), errString)
import Data.List (intersect)
import qualified Data.Map as Map
import qualified Data.Set as S
import Control.Lens  ((%~))

-- == DwtErr
printDwtErr :: DwtErr -> String
printDwtErr (base,opts,chain) = "Error: " ++ show base 
  ++ "\n  triggered by input: " ++ show opts
  ++ "\n  in call stack: " ++ show chain

-- == lists
listIntersect [] = []
listIntersect (x:xs) = foldl intersect x xs

listDiff :: Ord a => [a] -> [a] -> [a]
listDiff a b = S.toList $ S.difference (S.fromList a) (S.fromList b)

-- == graphs
maxNode :: Gr a b -> Node
maxNode = snd . nodeRange
{-# INLINABLE maxNode #-}

dropEdges :: Gr a b -> Gr a b
dropEdges = gmap (\(_,b,c,_) -> ([], b, c, []))

negateGraph :: Gr a b -> Gr a b
negateGraph m = gmap (\(a,b,c,d) -> (negAdj a, -b, c, negAdj d)) m
  where negAdj = map (\(label,n) -> (label,-n))


-- in progress
-- TODO ! fails silently. use Either.
replaceNode :: LNode a -> Gr a b -> Either DwtErr (Gr a b)
replaceNode (adr,c) (match adr -> (Just (a,b,_,d), g))
  = Right $ (a,b,c,d) & g
replaceNode (adr,_) (match adr -> (Nothing, _))
  = Left (FoundNo, [ErrNode adr], "replaceNode.")

nodeToLNodeUsf :: Gr a b -> Node -> LNode a
nodeToLNodeUsf g n = let Just a = lab g n in (n,a)

-- make the nodes the least positive integers possible
compressGraph :: DynGraph gr => gr a b -> gr a b
compressGraph g = let ns = nodes g
                      ns' = [1 .. length ns]
                      mp = Map.fromList $ zip ns ns'
                      chNode n = mp Map.! n
                      chAdj (b,n) = (b, mp Map.! n)
  in gmap (\(a,b,lab,d) -> (map chAdj a, chNode b, lab, map chAdj d)) g

joinGraphs :: DynGraph gr => gr a b -> gr a b -> gr a b
joinGraphs g h =
  let gMax = snd $ nodeRange g
      hMin = fst $ nodeRange h
      shift = 1 + gMax - hMin
      shiftAdj = map (\(elab,n) -> (elab,n+shift)) :: Adj b -> Adj b
      h' = gmap (\(ins,n,nlab,outs) ->
            (shiftAdj ins, n+shift, nlab, shiftAdj outs)) h
  in mkGraph (labNodes g ++ labNodes h') (labEdges g ++ labEdges h')

hasLEdgeM :: RSLT -> LEdge RSLTEdge -> Either DwtErr ()
hasLEdgeM g le@(a,b,lab) = if hasLEdge g le then return ()
                           else Left (FoundNo, x, "hasLEdgeM.")
  where x = [ErrEdge (a,b), ErrEdgeLab lab]

gelemM :: Graph gr => gr a b -> Node -> Either DwtErr ()
gelemM g n = if gelem n g then return () 
  else Left (FoundNo, [ErrNode n], "gelemM.")

-- == Either
fr :: Either a b -> b  -- doesn't handle the Left case (with a default)
  -- is therefore different from Data.Either.fromRight
fr (Right r) = r
fr _ = error "fromRight applied to Left"

fl :: Either a b -> a  -- doesn't handle the Left case (with a default)
  -- is therefore different from Data.Either.fromRight
fl (Left a) = a
fl _ = error "fromLeft applied to Right"

prependCaller :: String -> Either DwtErr a -> Either DwtErr a
prependCaller _ e@(Right _) = e
prependCaller name (Left e) = Left $ errString %~ (name++) $ e
