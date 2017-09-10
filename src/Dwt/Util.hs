{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Util (
  listIntersect, lengthOne -- lists

  -- graphs
  , maxNode, dropEdges, negateGraph, compressGraph, joinGraphs

  -- graphs & monads
  , hasLEdgeMStrErr, hasLEdgeM
  , gelemMStrErr, gelemM

  , fr, fromRight, prependCaller -- monads
  ) where

import Data.Graph.Inductive
import Dwt.Types
import Data.List (intersect)
import qualified Data.Map as Map
import Control.Monad.Except (MonadError, throwError)
import Control.Lens  ((.~), (%~))

-- == lists
listIntersect [] = []
listIntersect (x:xs) = foldl intersect x xs

lengthOne :: [a] -> Either String [a]
lengthOne ns = if length ns == 0 then Left "zero matches"
  else if length ns > 1 then Left "multiple matches"
  else return ns

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
replaceNode :: LNode a -> Gr a b -> Gr a b
replaceNode (adr,_) (match adr -> (Nothing, g)) = g
replaceNode (adr,dat) (match adr -> (Just (a,b,c,d), g)) = (a,b,dat,d) & g

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

hasLEdgeMStrErr :: (MonadError String m, Graph gr, Eq b, Show b) => 
  gr a b -> LEdge b -> m ()
hasLEdgeMStrErr g le = if hasLEdge g le then return ()
  else throwError $ "hasLEdgeMStrErr: LEdge " ++ show le ++ " absent."

hasLEdgeM :: RSLT -> LEdge RSLTEdge -> Either DwtErr ()
hasLEdgeM g le@(a,b,lab) = if hasLEdge g le then return ()
  else Left (FoundNo, x, "hasLEdgeM.")
  where x = mEdge .~ Just (a,b) $ mEdgeLab .~ Just lab $ noErrOpts

gelemMStrErr :: (MonadError String m, Graph gr) => gr a b -> Node -> m ()
gelemMStrErr g n = if gelem n g then return () 
  else throwError $ "gelemMStrErr: Node " ++ show n ++ " absent."

gelemM :: Graph gr => gr a b -> Node -> Either DwtErr ()
gelemM g n = if gelem n g then return () 
  else Left (FoundNo, mNode .~ Just n $ noErrOpts, "gelemM")


-- == monads
fromRight, fr :: Either a b -> b  -- TODO: doesn't handle the Left case
  -- is therefore different from Data.Either.fromRight
fr = fromRight
fromRight (Right r) = r
fromRight _ = error "fromRight applied to Left"

prependCaller :: String -> Either DwtErr a -> Either DwtErr a
prependCaller name e@(Right _) = e
prependCaller name (Left e) = Left $ errString %~ (name++) $ e


