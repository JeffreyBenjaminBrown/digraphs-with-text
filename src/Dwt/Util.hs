{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Util (
  listIntersect -- lists

  -- graphs
  , maxNode, dropEdges, negateGraph, compressGraph, joinGraphs
  , replaceNode

  , otherDir -- kind of an orphan

  -- graphs & monads
  , hasLEdgeM
  , gelemM

   -- monads
  , fr, fromRight
  , prependCaller
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

-- == graphs
maxNode :: Gr a b -> Node
maxNode = snd . nodeRange
{-# INLINABLE maxNode #-}

dropEdges :: Gr a b -> Gr a b
dropEdges = gmap (\(_,b,c,_) -> ([], b, c, []))

negateGraph :: Gr a b -> Gr a b
negateGraph m = gmap (\(a,b,c,d) -> (negAdj a, -b, c, negAdj d)) m
  where negAdj = map (\(label,n) -> (label,-n))


-- | swap Up and Down, or err
otherDir :: Mbrship -> Either DwtErr Mbrship
otherDir Up = Right Down
otherDir Down = Right Up
otherDir mv = Left (ConstructorMistmatch, [ErrMbrship mv]
                   , "otherDir: Only accepts Up or Down.")

-- in progress
-- TODO ! fails silently. use Either.
replaceNode :: LNode a -> Gr a b -> Either DwtErr (Gr a b)
replaceNode (adr,dat) (match adr -> (Just (a,b,c,d), g)) = Right
  $ (a,b,dat,d) & g
replaceNode (adr,_) (match adr -> (Nothing, g))          = Left
  (FoundNo, [ErrNode adr], "replaceNode.")

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

-- == monads
fromRight, fr :: Either a b -> b  -- TODO: doesn't handle the Left case
  -- is therefore different from Data.Either.fromRight
fr = fromRight
fromRight (Right r) = r
fromRight _ = error "fromRight applied to Left"

prependCaller :: String -> Either DwtErr a -> Either DwtErr a
prependCaller name e@(Right _) = e
prependCaller name (Left e) = Left $ errString %~ (name++) $ e
