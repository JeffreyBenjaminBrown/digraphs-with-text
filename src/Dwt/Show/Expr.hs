{-# LANGUAGE FlexibleContexts #-}

module Dwt.Show.Expr where

import Dwt.Initial.Types
import Dwt.Initial.Measure (tpltArity)
import Dwt.Second.MkTplt (subInTplt, subInTpltWithHashes)
import Dwt.Second.Graph (users,mbrs,exprDepth)

import Data.Graph.Inductive
import Data.List (sortOn, intercalate)
import qualified Data.Map as Map

data Prefixer = Prefixer {
    _str :: (Node -> String) -- Word|Fl -> String
  , _tplt :: (Node -> String)
  , _rel :: (Node -> Node -> String) -- Rel -> Tplt -> String
  , _coll :: (Node -> String)
  }

type ShowFiats = Map.Map Node String -- lookup how to show some special Nodes
  -- todo ! use for shorthand like It

data ViewProg = ViewProg { vpPrefixer :: Prefixer
                         , vpShowFiats :: ShowFiats
                         }

-- | == _showExpr and things only it uses
_showExpr :: ViewProg -> RSLT -> Level -> Maybe Node -> String
_showExpr _  _ _  Nothing = "#absent_node#"
  -- TODO: return Either, in case passed a missing node
_showExpr vp g d (Just n) = 
  let show_maybe_node mn = _showExpr vp g (d+1) mn
  in case Map.lookup n (vpShowFiats vp) of

    Just s -> s
    Nothing -> case lab g n of
      Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
      Just (Word s)   -> (_str $ vpPrefixer vp) n ++ s
      Just (Fl f)   -> (_str $ vpPrefixer vp) n ++ show f
      Just t@(Tplt _) -> (_tplt $ vpPrefixer vp) n
        ++ noHashes (subInTplt t $ replicate (tpltArity t) "_")
        where noHashes = filter $ not . (== '#') :: String -> String
      Just (Rel) -> _showRel vp g d n
      Just (Coll)    -> (_coll $ vpPrefixer vp) n
        ++ (show_maybe_node . Just . head)
             [m | (m,CollEdge CollPrinciple) <- lsuc g n]
        ++ ": "
        ++ ( intercalate ", "
           $ map (show_maybe_node . Just) 
                 [m | (m,CollEdge CollMbr) <- lsuc g n] )

-- toto : resurrect when needed
-- It was using QNodeSpec, not QNodeSpec
--      Just (RelspecExpr rvs) ->
--        let rs = fromRight $ relspec g (QAt n)
--            rsl = tail $ sortOn fst $ Map.toList rs -- tail drops the tplt
--              -- e.g. rsl = [(Mbr 1,QVarSpec Down),(Mbr 2,QNodeSpec 3)]
--            tpltNode = (\(QNodeSpec n) -> n) $ fromJust $ Map.lookup TpltRole rs
--            Just tpltLab = lab g tpltNode :: Maybe Expr
--            showAddressOrVar ms = case ms of
--              QVarSpec var -> show var
--              QNodeSpec node -> show_maybe_node $ Just node
--        in ((_rel $ vpPrefixer vp) n tpltNode ++)  $  ("#QRelspec#: " ++)
--          $ subInTplt tpltLab 
--          $ map showAddressOrVar $ map snd rsl

_showRel :: ViewProg -> RSLT -> Level -> Node -> String
-- The only tricky part here is handling missing nodes.
_showRel vp g d n =
  let elts = Map.fromList $ map (\(adr,elab)->(elab,Just adr))
                          $ lsuc g n :: Map.Map RSLTEdge (Maybe Node)
        -- in a well-formed graph, any edge label emits
        -- from a given node at most once
      Just tpltAddr = -- todo ? case of missing Tplt
        elts Map.! (RelEdge TpltRole)
      Just tpltExpr = lab g tpltAddr
      memberNodes = map snd -- handle missing nodes
                    $ sortOn fst $ Map.toList $ Map.union
                    (Map.delete  (RelEdge TpltRole)  elts)
                    (nullMbrMap tpltExpr) :: [Maybe Node]
        -- todo ? ordered list bad; just pass map
      memberStrings = map (_showExpr vp g $ d-1) memberNodes
  in ((_rel $ vpPrefixer vp) n tpltAddr ++)
     $ subInTpltWithHashes tpltExpr memberStrings (d-1)
       -- TODO ? the above d-1 is a hack. Better: rewrite subInTpltWithHashes.
       -- It's just an offset; it's not due to recursion in subexpressions.

-- | returns [(RelEdge (Mbr 1), Nothing), .. (RelEdge (Mbr arity), Nothing)]
nullMbrMap :: Expr -> Map.Map RSLTEdge (Maybe Node) 
nullMbrMap t@(Tplt _) = 
  let arity = tpltArity t
      es = map (RelEdge . Mbr) [1..arity]
  in Map.fromList $ zip es $ repeat Nothing
nullMbrMap _ = error "nullMbrMap: given a non-Tplt"
