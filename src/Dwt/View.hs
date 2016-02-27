    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Dwt.Util

    import Data.Graph.Inductive
    import Data.List (sortOn, intercalate)
    import Data.Maybe (fromJust)
    import qualified Data.Map as Map

    data PrefixStrategy = PrefixStrategy {
        _str :: (Node -> String) -- Str|Fl -> String
      , _tplt :: (Node -> String)
      , _rel :: (Node -> Node -> String) -- Rel -> Tplt -> String
      , _coll :: (Node -> String)
      }

    _showExpr :: Map.Map Node String ->      -- substitutions
                  PrefixStrategy -> 
                  Mindmap -> Node -> String
    _showExpr subs ps g n =
      case Map.lookup n subs of
        Just s -> s
        Nothing -> case lab g n of
          Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
          Just (Str s)   -> (_str ps) n ++ s
          Just (Fl f)   -> (_str ps) n ++ show f
          Just (Tplt ts) -> (_tplt ps) n ++ intercalate "_" ts
          Just (Coll)    -> (_coll ps) n ++ "TODO: use name" ++ ": "
            ++ ( intercalate ", "
               $ map show_node_bracketed [m | (m,CollEdge CollMbr) <- lsuc g n] )
          Just (RelSpecExpr rvs) ->
            let rs = fromRight $ relSpec g n
                rsl = tail $ sortOn fst $ Map.toList rs -- tail drops the tplt
                  -- e.g. rsl = [(Mbr 1,VarSpec Down),(Mbr 2,NodeSpec 3)]
                tpltNode = (\(NodeSpec n) -> n) $ fromJust $ Map.lookup RelTplt rs
                Just tpltLab = lab g tpltNode :: Maybe Expr
                showMbrSpec ms = case ms of
                  VarSpec var -> bracket $ show var
                  NodeSpec node -> show_node_bracketed node
            in ((_rel ps) n tpltNode ++) $ subInTplt tpltLab 
                 $ map showMbrSpec $ map snd rsl
          Just (Rel)     ->
            let elts = sortOn snd $ lsuc g n -- elts = Mbrs + Tplt
                (tpltNode, RelEdge RelTplt) = head elts
                  -- head because RelTplt goes before RelMbr in Ord Role
                Just tpltLab = lab g tpltNode :: Maybe Expr
                memberNodes = map fst $ tail elts :: [Node]
            in ((_rel ps) n tpltNode ++) $ subInTplt tpltLab 
                 $ map show_node_bracketed memberNodes
          where bracket s = "\171" ++ s ++ "\187" -- = «s»
                show_node_bracketed n = bracket $
                  _showExpr subs ps g n

    showExpr :: Map.Map Node String -> Mindmap -> Node -> String
    showExpr subs g n = _showExpr subs ps g n where
      colStrFunc = \n -> show n ++ ": "
      ps = PrefixStrategy { _str = colStrFunc
                          , _tplt = \n -> ":" ++ show n ++ " "
                          , _rel = \n tn -> show n ++ ":" ++ show tn ++ " "
                          , _coll = colStrFunc }

    -- show tersely, without addresses
    showExprT :: Map.Map Node String -> Mindmap -> Node -> String
    showExprT subs g n = _showExpr subs ps g n where
      f = const ""
      ps = PrefixStrategy {_str=f, _tplt=f, _rel = \a b ->"", _coll=f}

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
    vm :: Mindmap -> RelSpec -> IO () -- view match
    vm g spec = viewS (redundancySubs spec) g $ fromRight $ matchRel g spec
      -- TODO : remove fromRight

    va :: Mindmap -> Node -> IO () -- view all rels
    va g n = viewS (Map.fromList [(n,show n)]) g $ pre g n
