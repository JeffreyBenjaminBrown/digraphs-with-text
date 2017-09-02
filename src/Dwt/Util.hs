{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Util (
  listIntersect, lengthOne -- lists

  -- graphs
  , maxNode, dropEdges, negateGraph, compressGraph, joinGraphs
  , hasLEdgeM, gelemM, gelemMDe -- graphs & monads

  , mapLookupMe, eitherToMe, fr, fromRight -- monads
  ) where

import Data.Graph.Inductive
import Dwt.Types
import Data.List (intersect)
import qualified Data.Map as Map
import Control.Monad.Except (MonadError, throwError)
import Control.Lens  ((.~))

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

hasLEdgeM :: (MonadError String m, Graph gr, Eq b, Show b) => 
  gr a b -> LEdge b -> m ()
hasLEdgeM g le = if hasLEdge g le then return ()
  else throwError $ "hasLEdgeM: LEdge " ++ show le ++ " absent."

gelemM :: (MonadError String m, Graph gr) => gr a b -> Node -> m ()
gelemM g n = if gelem n g then return () 
  else throwError $ "gelemM: Node " ++ show n ++ " absent."

gelemMDe :: Graph gr => gr a b -> Node -> Either DwtErr ()
gelemMDe g n = if gelem n g then return () 
  else Left (FoundNo, mNode .~ Just n $ noErrOpts, "gelemMDe")


-- == monads
-- TODO: This could use a higher-kinded function, lke eitherToMe, for Maybes
  -- should in that case take also a String to show if Nothing happens
mapLookupMe :: (Ord k, Show k, Show a, MonadError String me) => -- TODO: bad?
  k -> Map.Map k a -> me a -- Is it bad to need this function?
mapLookupMe k m = case Map.lookup k m of
  Just a -> return a
  Nothing -> throwError $ "mapLookupMe: " ++
  -- reports map; could be bad if map big
    show k ++ " not in map " ++ show m

eitherToMe :: (Show e, MonadError String me) =>
  (a -> Either e t) -> a -> me t
eitherToMe f x = case f x of Right y -> return y
                             Left e -> throwError $ show e

fromRight, fr :: Either a b -> b  -- TODO: doesn't handle the Left case
  -- is therefore different from Data.Either.fromRight
fr = fromRight
fromRight (Right r) = r
fromRight _ = error "fromRight applied to Left"

prependCallerName :: String -> Either DwtErr a -> Either DwtErr a
prependCallerName name e@(Right _) = e
prependCallerName name (Left e) = Left $ 
