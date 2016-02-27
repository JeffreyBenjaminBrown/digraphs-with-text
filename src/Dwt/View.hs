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

    bracket :: String -> String
    bracket s = "\171" ++ s ++ "\187" -- = «s»

    _showExpr :: Map.Map Node String ->      -- TODO ! use for shorthand like It
                 PrefixStrategy -> 
                 Mindmap -> Maybe Node -> String
    _showExpr subs ps g Nothing = "#absent#node#" -- TODO ! use
    _showExpr subs ps g (Just n) =
      case Map.lookup n subs of
        Just s -> s
        Nothing -> case lab g n of
          Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
          Just (Str s)   -> (_str ps) n ++ s
          Just (Fl f)   -> (_str ps) n ++ show f
          Just (Tplt ts) -> (_tplt ps) n ++ intercalate "_" ts
          Just (Coll)    -> (_coll ps) n ++ "TODO: use name" ++ ": "
            ++ ( intercalate ", "
               $ map (show_node_bracketed . Just) 
                     [m | (m,CollEdge CollMbr) <- lsuc g n] )
          Just (RelSpecExpr rvs) ->
            let rs = fromRight $ relSpec g n
                rsl = tail $ sortOn fst $ Map.toList rs -- tail drops the tplt
                  -- e.g. rsl = [(Mbr 1,VarSpec Down),(Mbr 2,NodeSpec 3)]
                tpltNode = (\(NodeSpec n) -> n) $ fromJust $ Map.lookup RelTplt rs
                Just tpltLab = lab g tpltNode :: Maybe Expr
                showMbrSpec ms = case ms of
                  VarSpec var -> bracket $ show var
                  NodeSpec node -> show_node_bracketed $ Just node
            in ((_rel ps) n tpltNode ++) $ subInTplt tpltLab 
                 $ map showMbrSpec $ map snd rsl
          Just (Rel)     ->
            let elts = Map.fromList $ map (\(a,b)->(b,Just a)) $ lsuc g n
                Just (Just tpltNode) = 
                  Map.lookup (RelEdge RelTplt) elts -- TODO: if Nothing?
                Just tpltExpr = lab g tpltNode
                memberNodes = map snd $ sortOn fst $ Map.toList $ overwriteMap
                  (Map.delete  (RelEdge RelTplt)  elts)
                  (nullMembers tpltExpr)
            in ((_rel ps) n tpltNode ++) $ subInTplt tpltExpr
                 $ map show_node_bracketed memberNodes
          where show_node_bracketed mn = bracket $
                  _showExpr subs ps g mn

    overwriteMap :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
    overwriteMap after before = Map.union after
      $ Map.filterWithKey (\k _ -> not $ Map.member k after) before

    nullMembers :: Expr -> Map.Map DwtEdge (Maybe Node)
    nullMembers t@(Tplt _) =
      let arity = tpltArity t
          mns = replicate arity Nothing :: [Maybe Node]
          es = map (RelEdge . Mbr) [1..arity] -- RelEdge $ Mbr 1 :: DwtEDge
      in Map.fromList $ zip es mns

    ps = PrefixStrategy { _str = colStrFunc
                        , _tplt = \n -> ":" ++ show n ++ " "
                        , _rel = \n tn -> show n ++ ":" ++ show tn ++ " "
                        , _coll = colStrFunc } where
      colStrFunc = \n -> show n ++ ": "

    pst = PrefixStrategy {_str=f, _tplt=f, _coll=f, _rel = \a b ->""}
      where f = const ""

    showExpr :: Mindmap -> Node -> String
    showExpr g n = _showExpr Map.empty ps g (Just n)

    showExprT :: Mindmap -> Node -> String -- terse, no addresses
    showExprT g n = _showExpr Map.empty ps g (Just n)

    v :: Mindmap -> [Node] -> IO ()
    v g ns = mapM_ putStrLn $ map (showExpr g) ns

    vt :: Mindmap -> [Node] -> IO ()
    vt g ns = mapM_ putStrLn $ map (showExprT g) ns
