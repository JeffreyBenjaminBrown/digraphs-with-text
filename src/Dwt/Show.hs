{-# LANGUAGE FlexibleContexts #-}

module Dwt.Show
  ( module Dwt.Show
  ) where

import Dwt.Initial.Types
import Dwt.Initial.Util (fromRight)
import Dwt.Initial.Measure (tpltArity)
import Dwt.Second.MkTplt (subInTplt, subInTpltWithHashes)
import Dwt.Query.Initial (users,mbrs)

import Data.Graph.Inductive
import Data.List (sortOn, intercalate)
import qualified Data.Map as Map

type Depth = Int
type Gen = (Depth,[Node])

type ShowFiats = Map.Map Node String -- lookup how to show some special Nodes
  -- todo ! use for shorthand like It

data Prefixer = Prefixer {
    _str :: (Node -> String) -- Word|Fl -> String
  , _tplt :: (Node -> String)
  , _rel :: (Node -> Node -> String) -- Rel -> Tplt -> String
  , _coll :: (Node -> String)
  }

data ViewProg = ViewProg { vpPrefixer :: Prefixer
                         , vpShowFiats :: ShowFiats
                         }

-- | == things _showExpr uses, maybe useful elsewhere -- TODO ? export|promote x-file
exprDepth :: RSLT -> Node -> Depth -- TODO ? Use the [Node]
exprDepth g n = fst $ _exprDepth g (0,[n]) (1,[]) []

_exprDepth :: RSLT -> Gen -- this gen
                      -> Gen -- next gen
                      -> [Node] -- accum every node visited
                      -> (Depth,[Node]) -- depth + the accum
  -- when a node's scrs are evaluated, it is removed from the graph
    -- so only the shortest path to it is evaluated
  -- WARNING: does not return a Gen -- those Nodes might be at any depth
_exprDepth _ (d,[])    (_,[]) acc = (d,acc)
_exprDepth g (_,[]) ng@(d, _) acc = -- this gen empty, so next replaces it
  _exprDepth g ng (d+1,[]) acc
_exprDepth g (d,n:ns) (d',ns')  acc = let newNodes = mbrs g n in
  _exprDepth (delNode n g) (d,ns) (d', newNodes ++ ns') (n:acc)

-- | == _showExpr and things only it uses
_showExpr :: ViewProg -> RSLT -> Depth -> Maybe Node -> String
_showExpr _  _ _  Nothing = "#absent_node#"
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

      Just (Rel) -> _showRel vp g d n

_showRel :: ViewProg -> RSLT -> Depth -> Node -> String
_showRel vp g d n =
  let elts = Map.fromList $ map (\(adr,elab)->(elab,Just adr))
                          $ lsuc g n :: Map.Map RSLTEdge (Maybe Node)
        -- in a well-formed graph, any edge label emits
        -- from a given node at most once
      Just tpltAddr = -- todo ? case of missing Tplt
        elts Map.! (RelEdge TpltRole)
      Just tpltExpr = lab g tpltAddr
      memberNodes = map snd $ sortOn fst $ Map.toList $ Map.union
        (Map.delete  (RelEdge TpltRole)  elts)
        (nullMbrMap tpltExpr) :: [Maybe Node]
        -- handles missing nodes
        -- todo ? ordered list bad; just pass map
  in ((_rel $ vpPrefixer vp) n tpltAddr ++)
     $ subInTpltWithHashes tpltExpr
       (map (_showExpr vp g $ d-1) memberNodes)
       d

nullMbrMap :: Expr -> Map.Map RSLTEdge (Maybe Node) 
  -- in the result, each Maybe is Nothing, and
    -- the RSLTEdges run from (Mbr 1) to (Mbr arity)
nullMbrMap t@(Tplt _) =
  let arity = tpltArity t
      mns = replicate arity Nothing :: [Maybe Node]
      es = map (RelEdge . Mbr) [1..arity] -- RelEdge $ Mbr 1 :: DwtEDge
  in Map.fromList $ zip es mns
nullMbrMap _ = error "nullMbrMap: given a non-Tplt"

-- | == things using _showExpr
prefixer = Prefixer {_str=f, _tplt=f, _coll=f, _rel = const f}
  where f = const ""

prefixerVerbose = Prefixer { _str = colWordFunc
                    , _tplt = \n -> ":" ++ show n ++ " "
                    , _rel = \n tn -> show n ++ ":" ++ show tn ++ " "
                    , _coll = colWordFunc } where
  colWordFunc = \n -> show n ++ ": "

showExpr :: RSLT -> Node -> String
showExpr g n = _showExpr vp g d (Just n)
  where d = exprDepth g n
        vp = ViewProg { vpPrefixer = prefixer
                      , vpShowFiats = Map.empty }

showExprVerbose :: RSLT -> Node -> String
showExprVerbose g n = _showExpr vp g d (Just n)
  where d = exprDepth g n
        vp = ViewProg { vpPrefixer = prefixerVerbose
                      , vpShowFiats = Map.empty }

view :: RSLT -> [Node] -> Either DwtErr [String]
view g ns = do mapM f ns
  where f n = do x <- length <$> users g n
                 return $ show (n, x, showExpr g n)

viewVerbose :: RSLT -> [Node] -> Either DwtErr [String]
viewVerbose g ns = do mapM f ns -- ^ verbose: shows inner addresses
  where f n = do x <- length <$> users g n
                 return $ show (n, x, showExprVerbose g n)

-- | == IO
_v :: (RSLT -> [Node] -> Either DwtErr [String]) -> RSLT -> [Node] -> IO ()
_v viewer g ns = do putStrLn "(node, users, content)"
                    either (putStrLn . show) (mapM_ putStrLn) $ viewer g ns

v, vv :: RSLT -> [Node] -> IO ()
v  = _v view
vv = _v viewVerbose
va, vva :: RSLT -> IO ()
va g = v g $ nodes g
vva g = vv g $ nodes g

-- | == mostly unused
bracket :: String -> String -- unused, but a useful pair of characters
bracket s = "\171" ++ s ++ "\187" -- = «s»
