    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Data.List (sortOn, intercalate)

    showExpr :: Mindmap -> Node -> String -- TODO: Either|Maybe
      -- BEWARE ? infinite loops
        -- if the graph is recursive, this could infinite loop
          -- such cycles seem unlikely to be intende, b/c recursive statements are
          -- confusing; c.f. Godel's impossibility theorem
        -- a solution: while building, keep list of visited nodes
          -- if visiting one already listed, display it as just its Node
          -- or Node ++ "already displayed higher in this (node view?)"
    showExpr g n = case lab g n of
      Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
      Just (Str s) ->     (show n) ++ ": "       ++ s
      Just (Tplt _ ts) -> ":" ++ (show n) ++ " " ++ intercalate "_" ts
      Just (Rel _) ->
        let ledges = sortOn edgeLabel $ out g n
            (_,tpltNode,_) = head ledges
              -- head because Tplt sorts first, before Rel, in Ord Expr 
            Just tpltLab = lab g tpltNode :: Maybe Expr
            members = map (\(_,m,_)-> m) $ tail ledges :: [Node]
        in prefixRel tpltNode $ subInTplt tpltLab 
             $ map (bracket . showExpr g) members
      where prefixRel tn s = show n ++ ":" ++ show tn ++ " " ++ s
            bracket s = "[" ++ s ++ "]"

    view :: Mindmap -> [Node] -> IO ()
    view g ns = mapM_ putStrLn $ map (showExpr g) ns
