    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Util (
      listIntersect, lengthOne -- for lists
      -- for graphs
      , maxNode, dropEdges, negateGraph, compressGraph, joinGraphs

      , mapLookupMe, eitherToMe, fromRight -- for monads
      ) where

    import Data.Graph.Inductive
    import Data.List (intersect)
    import qualified Data.Map as Map
    import Control.Monad.Except (MonadError, throwError)

    listIntersect [] = []
    listIntersect (x:xs) = foldl intersect x xs

    lengthOne :: [a] -> Either String [a]
    lengthOne ns = if length ns == 0 then Left "zero matches"
      else if length ns > 1 then Left "multiple matches"
      else return ns

--  for graphs
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

-- for monads
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

    fromRight :: Either a b -> b
    fromRight (Right r) = r
    fromRight _ = error "fromRight applied to Left"
