    {-# LANGUAGE FlexibleContexts #-}

    module Dwt.View
      ( module Dwt.View
      ) where

    import Dwt.Graph
    import Dwt.Util
    import Dwt.FileIO (graphToText)

    import Data.Graph.Inductive

    import Control.Monad.Except (MonadError, throwError, catchError)
    import Data.List (sortOn, intercalate, nub)
    import Data.Maybe (fromJust)
    import qualified Data.Map as Map

    type Depth = Int
    type Gen = (Depth,[Node])

    data PrefixStrategy = PrefixStrategy {
        _str :: (Node -> String) -- Str|Fl -> String
      , _tplt :: (Node -> String)
      , _rel :: (Node -> Node -> String) -- Rel -> Tplt -> String
      , _coll :: (Node -> String)
      }

-- things _showExpr uses, maybe useful elsewhere -- TODO ? export|promote x-file
    exprDepth :: Mindmap -> Node -> Depth -- TODO ? Use the [Node]
    exprDepth g n = fst $ _exprDepth g (0,[n]) (1,[]) []

    _exprDepth :: Mindmap -> Gen -- this gen
                          -> Gen -- next gen
                          -> [Node] -- accum every node visited
                          -> (Depth,[Node]) -- depth + the accum
      -- when a node's scrs are evaluated, it is removed from the graph
        -- so only the shortest path to it is evaluated
      -- WARNING: does not return a Gen -- those Nodes might be at any depth
    _exprDepth g (d,[]) (_,[])      acc = (d,acc)
    _exprDepth g (_,[]) ng@(d,n:ns) acc = -- this gen empty, so next replaces it
      _exprDepth g ng (d+1,[]) acc
    _exprDepth g (d,n:ns) (d',ns')  acc = let newNodes = mbrs g n in
      _exprDepth (delNode n g) (d,ns) (d', newNodes ++ ns') (n:acc)

-- _showExpr and things only it uses
    _showExpr :: Depth ->
                 Map.Map Node String -> -- todo ! use for shorthand like It
                 PrefixStrategy -> 
                 Mindmap -> Maybe Node -> String
    _showExpr d subs ps g Nothing = "#absent node#"
    _showExpr d subs ps g (Just n) = 
      let show_maybe_node mn = _showExpr (d+1) subs ps g mn
          expr = Map.lookup n subs 
      in case expr of

        Just s -> s
        Nothing -> case lab g n of
          Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
          Just (Str s)   -> (_str ps) n ++ s
          Just (Fl f)   -> (_str ps) n ++ show f
          Just t@(Tplt ts) -> (_tplt ps) n ++ 
            (subInTplt t $ replicate (tpltArity t) "_")
          Just (Coll)    -> (_coll ps) n ++ "TODO: use name" ++ ": "
            ++ ( intercalate ", "
               $ map (show_maybe_node . Just) 
                     [m | (m,CollEdge CollMbr) <- lsuc g n] )

          Just (RelSpecExpr rvs) ->
            let rs = fromRight $ relSpec g n
                rsl = tail $ sortOn fst $ Map.toList rs -- tail drops the tplt
                  -- e.g. rsl = [(Mbr 1,VarSpec Down),(Mbr 2,NodeSpec 3)]
                tpltNode = (\(NodeSpec n) -> n) $ fromJust $ Map.lookup RelTplt rs
                Just tpltLab = lab g tpltNode :: Maybe Expr
                showMbrSpec ms = case ms of
                  VarSpec var -> show var
                  NodeSpec node -> show_maybe_node $ Just node
            in ((_rel ps) n tpltNode ++) $ ("#RelSpec#: " ++) $ subInTplt tpltLab 
                 $ map showMbrSpec $ map snd rsl

          Just (Rel) -> _showRel Rel d subs ps g n

    _showRel :: Expr ->
                Depth ->
                Map.Map Node String ->
                PrefixStrategy -> 
                Mindmap -> Node -> String
    _showRel Rel d subs ps g n =
      let elts = Map.fromList $ map (\(adr,elab)->(elab,Just adr)) $ lsuc g n
          Just tpltAddr = -- todo ? case of missing Tplt
            elts Map.! (RelEdge RelTplt)
          Just tpltExpr = lab g tpltAddr
          memberNodes = map snd $ sortOn fst $ Map.toList $ Map.union
            (Map.delete  (RelEdge RelTplt)  elts)
            (nullMembers tpltExpr) -- todo ? ordered list bad; just pass map
      in ((_rel ps) n tpltAddr ++) 
         $ subInTpltWithDollars tpltExpr
           (map (_showExpr (d-1) subs ps g) memberNodes)
           d

    nullMembers :: Expr -> Map.Map DwtEdge (Maybe Node) 
      -- each Maybe is Nothing
      -- the DwtEdges run from (Mbr 1) to (Mbr arity)
    nullMembers t@(Tplt _) =
      let arity = tpltArity t
          mns = replicate arity Nothing :: [Maybe Node]
          es = map (RelEdge . Mbr) [1..arity] -- RelEdge $ Mbr 1 :: DwtEDge
      in Map.fromList $ zip es mns

-- things using _showExpr
    ps = PrefixStrategy { _str = colStrFunc
                        , _tplt = \n -> ":" ++ show n ++ " "
                        , _rel = \n tn -> show n ++ ":" ++ show tn ++ " "
                        , _coll = colStrFunc } where
      colStrFunc = \n -> show n ++ ": "

    pst = PrefixStrategy {_str=f, _tplt=f, _coll=f, _rel = \a b ->""}
      where f = const ""

    showExpr :: Mindmap -> Node -> String
    showExpr g n = _showExpr d Map.empty ps g (Just n)
      where d = exprDepth g n

    showExprT :: Mindmap -> Node -> String -- terse, no addresses
    showExprT g n = _showExpr d Map.empty pst g (Just n)
      where d = exprDepth g n

    -- TODO NEXT: count clarifications

    v :: Mindmap -> [Node] -> IO ()
    v g ns = mapM_ putStrLn $ map f ns
      where f n = show $ (fromRight $ length <$> users g n -- emul: counts users
                         , showExpr g n)

    vt :: Mindmap -> [Node] -> IO () -- terse; no countUsers, no addresses
    vt g ns = mapM_ putStrLn $ map (showExprT g) ns

-- mostly unused
    bracket :: String -> String -- now entirely unused
    bracket s = "\171" ++ s ++ "\187" -- = «s»

    showRaw :: Mindmap -> IO ()
    showRaw g = putStr $ graphToText g
