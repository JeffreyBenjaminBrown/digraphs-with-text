    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Dwt.Util

    import Data.Graph.Inductive
    import Data.List (sortOn, intercalate)
    import Data.Maybe (fromJust)
    import qualified Data.Map as Map

-- showExpr
    _showExpr :: Map.Map Node String ->      -- substitutions
                  (Node -> String) ->         -- how to prefix Strs
                  (Node -> String) ->         -- how to prefix Tplts
                  (Node -> Node -> String) -> -- how to prefix Rels
                  (Node -> String) ->         -- how to prefix Colls
                  Mindmap -> Node -> String
    _showExpr subs strPfx tpltPfx relPfx collPfx g n =
      case Map.lookup n subs of
        Just s -> s
        Nothing -> case lab g n of
          Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
          Just (Str s)   -> strPfx n ++ s
          Just (Fl f)   -> strPfx n ++ show f
          Just (Tplt ts) -> tpltPfx n ++ intercalate "_" ts
          Just (Coll)    -> collPfx n ++ "TODO: use name" ++ ": "
            ++ ( intercalate ", "
               $ map show_in_brackets [m | (m,CollEdge CollMbr) <- lsuc g n] )
          Just (RelSpecExpr rvs) ->
            let rs = fromRight $ relSpec g n
                rsl = tail $ sortOn fst $ Map.toList rs -- tail drops the tplt
                  -- e.g. rsl = [(Mbr 1,VarSpec Down),(Mbr 2,NodeSpec 3)]
                tpltNode = (\(NodeSpec n) -> n) $ fromJust $ Map.lookup RelTplt rs
                Just tpltLab = lab g tpltNode :: Maybe Expr
                showMbrSpec ms = case ms of
                  VarSpec var -> bracket $ show var
                  NodeSpec node -> show_in_brackets node
            in (relPfx n tpltNode ++) $ subInTplt tpltLab 
                 $ map showMbrSpec $ map snd rsl
          Just (Rel)     ->
            let elts = sortOn snd $ lsuc g n -- elts = Mbrs + Tplt
                (tpltNode, RelEdge RelTplt) = head elts
                  -- head because RelTplt goes before RelMbr in Ord Role
                Just tpltLab = lab g tpltNode :: Maybe Expr
                memberNodes = map fst $ tail elts :: [Node]
            in (relPfx n tpltNode ++) $ subInTplt tpltLab 
                 $ map show_in_brackets memberNodes
          where bracket s = "\171" ++ s ++ "\187"
                show_in_brackets = bracket
                  . _showExpr subs strPfx tpltPfx relPfx collPfx g

    showExpr :: Map.Map Node String -> Mindmap -> Node -> String
    showExpr subs g n = _showExpr subs strPfx tpltPfx relPfx collPfx g n where
      strPfx n = show n ++ ": "
      tpltPfx n = ":" ++ show n ++ " "
      relPfx n tn = show n ++ ":" ++ show tn ++ " "
      collPfx = strPfx

    -- show tersely, without Nodes
    showExprT :: Map.Map Node String -> Mindmap -> Node -> String
    showExprT subs g n = _showExpr subs strPfx tpltPfx relPfx collPfx g n where
      strPfx n = ""
      tpltPfx n = ""
      relPfx n tn = ""
      collPfx = strPfx

-- view
    view :: Mindmap -> [Node] -> IO ()
    view g ns = mapM_ putStrLn $ map (showExpr Map.empty g) ns

    -- view tersely, without Nodes
    viewT :: Mindmap -> [Node] -> IO ()
    viewT g ns = mapM_ putStrLn $ map (showExprT Map.empty g) ns

    -- view with substitutions
    viewS :: Map.Map Node String -> Mindmap -> [Node] -> IO ()
    viewS subs g ns = mapM_ putStrLn $ map (showExpr subs g) ns

    -- view with substitutions, tersely 
    viewST :: Map.Map Node String -> Mindmap -> [Node] -> IO ()
    viewST subs g ns = mapM_ putStrLn $ map (showExprT subs g) ns

-- convenient shorthand
    (n,j,ns) = (Nothing,Just,NodeSpec)

    vm :: Mindmap -> RelSpec -> IO () -- view match
    vm g spec = viewS (redundancySubs spec) g $ fromRight $ matchRel g spec
      -- TODO : remove fromRight

    va :: Mindmap -> Node -> IO () -- view all rels
    va g n = viewS (Map.fromList [(n,show n)]) g $ pre g n
