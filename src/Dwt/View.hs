    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Data.List (sortOn, intercalate)

    _showExpr :: (Node -> String) ->
                 (Node -> String) ->
                 (Node -> Node -> String) ->
                 (Node -> String) ->
                 Mindmap -> Node -> String
    _showExpr strPrefix tpltPrefix relPrefix collPrefix g n = case lab g n of
      Nothing          -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
      Just (Str s)     -> strPrefix n ++ s
      Just (Tplt _ ts) -> tpltPrefix n ++ intercalate "_" ts
      Just (Coll s)    -> collPrefix n ++ s ++ ": "
        ++ ( intercalate ", " 
           $ map show_in_brackets [m | (m,CollMbr) <- lsuc g n] )
      Just (Rel _)     ->
        let elts = sortOn snd $ lsuc g n -- elts = Mbrs + Tplt
            (tpltNode,RelTplt) = head elts
              -- head because RelTplt goes before RelMbr in Ord Role
            Just tpltLab = lab g tpltNode :: Maybe Expr
            memberNodes = map fst $ tail elts :: [Node]
        in (relPrefix n tpltNode ++) $ subInTplt tpltLab 
             $ map show_in_brackets memberNodes
      where bracket s = "[" ++ s ++ "]"
            show_in_brackets = bracket
              . _showExpr strPrefix tpltPrefix relPrefix collPrefix g

    showExpr :: Mindmap -> Node -> String
    showExpr g n = _showExpr strPrefix tpltPrefix relPrefix collPrefix g n where
      strPrefix n = show n ++ ": "
      tpltPrefix n = ":" ++ show n ++ " "
      relPrefix n tn = show n ++ ":" ++ show tn ++ " "
      collPrefix = strPrefix

    showExprT :: Mindmap -> Node -> String -- show tersely, without Nodes
    showExprT g n = _showExpr strPrefix tpltPrefix relPrefix collPrefix g n where
      strPrefix n = ""
      tpltPrefix n = ""
      relPrefix n tn = ""
      collPrefix = strPrefix

    view :: Mindmap -> [Node] -> IO ()
    view g ns = mapM_ putStrLn $ map (showExpr g) ns

    viewT :: Mindmap -> [Node] -> IO () -- view tersely, without Nodes
    viewT g ns = mapM_ putStrLn $ map (showExprT g) ns
