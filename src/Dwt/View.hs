    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Data.List (sortOn, intercalate)

    _showExpr :: (Node -> String) -> (Node -> String) -> 
      (Node -> Node -> String) -> Mindmap -> Node -> String
    _showExpr strPrefix tpltPrefix relPrefix g n = case lab g n of
      Nothing          -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
      Just (Str s)     -> strPrefix n ++ s
      Just (Tplt _ ts) -> tpltPrefix n ++ intercalate "_" ts
      Just (Rel _)     ->
        let ledges = sortOn edgeLabel $ out g n
            (_,tpltNode,_) = head ledges
              -- head because Tplt sorts first, before Rel, in Ord Expr 
            Just tpltLab = lab g tpltNode :: Maybe Expr
            memberNodes = map (\(_,m,_)-> m) $ tail ledges :: [Node]
        in (relPrefix n tpltNode ++) $ subInTplt tpltLab 
             $ map (bracket . _showExpr strPrefix tpltPrefix relPrefix g) 
                   memberNodes
      where bracket s = "[" ++ s ++ "]"

    showExpr :: Mindmap -> Node -> String
    showExpr g n = _showExpr strPrefix tpltPrefix relPrefix g n where
      strPrefix n = show n ++ ": "
      tpltPrefix n = ":" ++ show n ++ " "
      relPrefix n tn = show n ++ ":" ++ show tn ++ " "

    showtExpr :: Mindmap -> Node -> String -- show tersely, without Nodes
    showtExpr g n = _showExpr strPrefix tpltPrefix relPrefix g n where
      strPrefix n = ""
      tpltPrefix n = ""
      relPrefix n tn = ""

    view :: Mindmap -> [Node] -> IO ()
    view g ns = mapM_ putStrLn $ map (showExpr g) ns

    viewt :: Mindmap -> [Node] -> IO () -- view tersely, without Nodes
    viewt g ns = mapM_ putStrLn $ map (showtExpr g) ns
