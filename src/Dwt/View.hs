-- shorthand
  -- pfx = prefix

--
    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Data.List (sortOn, intercalate)
    import qualified Data.Map as Map

-- engine
    _showExpr :: Map.Map Node String -> -- substitutions
                 (Node -> String) ->
                 (Node -> String) ->
                 (Node -> Node -> String) ->
                 (Node -> String) ->
                 Mindmap -> Node -> String
    _showExpr subs strPfx tpltPfx relPfx collPfx g n =
      case Map.lookup n subs of
        Just s -> s
        Nothing -> case lab g n of
          Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
          Just (Str s)     -> strPfx n ++ s
          Just (Tplt _ ts) -> tpltPfx n ++ intercalate "_" ts
          Just (Coll s)    -> collPfx n ++ s ++ ": "
            ++ ( intercalate ", " 
               $ map show_in_brackets [m | (m,CollMbr) <- lsuc g n] )
          Just (Rel _)     ->
            let elts = sortOn snd $ lsuc g n -- elts = Mbrs + Tplt
                (tpltNode,RelTplt) = head elts
                  -- head because RelTplt goes before RelMbr in Ord Role
                Just tpltLab = lab g tpltNode :: Maybe Expr
                memberNodes = map fst $ tail elts :: [Node]
            in (relPfx n tpltNode ++) $ subInTplt tpltLab 
                 $ map show_in_brackets memberNodes
          where bracket s = "[" ++ s ++ "]"
                show_in_brackets = bracket
                  . _showExpr subs strPfx tpltPfx relPfx collPfx g

-- things that use it
    showExpr :: Map.Map Node String -> Mindmap -> Node -> String
    showExpr subs g n = _showExpr subs strPfx tpltPfx relPfx collPfx g n where
      strPfx n = show n ++ ": "
      tpltPfx n = ":" ++ show n ++ " "
      relPfx n tn = show n ++ ":" ++ show tn ++ " "
      collPfx = strPfx

    showExprT :: Map.Map Node String -> Mindmap -> Node -> String -- show tersely, without Nodes
    showExprT subs g n = _showExpr subs strPfx tpltPfx relPfx collPfx g n where
      strPfx n = ""
      tpltPfx n = ""
      relPfx n tn = ""
      collPfx = strPfx

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
