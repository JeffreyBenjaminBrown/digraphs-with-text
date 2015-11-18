-- usually folded
  -- NEXT TODO ? IO, parser
    -- ? viewer
  -- TODO ? make "arity" a type
      -- its support is the integers in [1,k]
      -- RelPositions and RelExprs contain one
        -- or RelPosition contains a number that varies in [1,k]
          -- where k is the arity
  -- TODO ? use concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
    -- (I forget where but I think I was doing that by hand)
  -- types, vocab, language
    -- Node,Edge: FGL. Expr, Rel: DWT|Mindmap.
    -- how to read edges
      -- in (n,m,lab :: MmLab) :: LEdge MmLab, n is a triplet referring to m
      -- that is, predecessors refer to successors 
        -- (in that kind of relationship they do; maybe there will be others)

  -- export & import
    module Dwt
      ( -- exports:
      module Data.Graph.Inductive -- export for testing, not production
      , module Dwt -- exports everything in this file
      -- , module Dwt.Graph -- etc. Will need to import below to match.
      ) where    
    import Data.Graph.Inductive
    import Data.String (String)
    import Data.Either (partitionEithers)
    import Data.List (intersect)
    import Data.Maybe (isJust, catMaybes)
    import Control.Monad (mapM_)

-- types
    data MmExpr = StrExpr String | RelExpr Int
      deriving (Show,Eq,Ord)

    data MmEdge = RelTemplate | RelPosition Int -- hide this type from user
      deriving (Show,Eq,Ord)

    type Mindmap = Gr MmExpr MmEdge

-- build
    insStrExpr :: String -> Mindmap -> Mindmap
    insStrExpr str g = insNode (int, StrExpr str) g
      where int = head $ newNodes 1 g

    insRelExpr :: Node -> [Node] -> Mindmap -> Mindmap
    insRelExpr t ns g = f (zip ns [1..len]) g' -- t is like ns but tplt
      where len = length ns
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, RelTemplate)
               $ insNode (newNode, RelExpr len) g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelPosition $ snd p) g

-- query
    mmReferents :: Mindmap -> MmEdge -> Int -> Node -> [Node]
    mmReferents g e arity n =
      let pdrNode (m,n,label) = m
          isKAryRel m = lab g m == (Just $ RelExpr arity)
      in [m | (m,n,label) <- inn g n, label == e, isKAryRel m]

    mmRelps :: Mindmap -> [Maybe Node] -> [Node]
    mmRelps g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, Int)]
            f (Just n, 0) = mmReferents g RelTemplate     arity n
            f (Just n, k) = mmReferents g (RelPosition k) arity n
            listIntersect (x:xs) = foldl intersect x xs

-- view
    showExpr :: Mindmap -> Node -> Either String String -- WARNING
      -- if the graph is recursive this (I think) could infinite loop
    showExpr g n = case lab g n of
      Nothing -> Left $ "node " ++ (show n) ++ " not in graph"
      Just (StrExpr s) -> Right s
      Just (RelExpr n) -> Right -- broken
          $ concat $ concat --concat [String]s, then Strings
          $ partitionEithers $ map f $ out g n
        where f (n,m,label) = showExpr g m

    v :: Mindmap -> [Maybe Node] -> IO ()
    v g mns = mapM_ putStrLn $ map (eitherString . showExpr g) $ mmRelps g mns
      where eitherString (Left s) = s
            eitherString (Right s) = s
