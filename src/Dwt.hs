-- export & import
    module Dwt
      ( -- exports:
      module Data.Graph.Inductive -- export for testing, not production
      , module Dwt -- exports everything in this file
      -- , module Dwt.Graph -- etc. Will need to import below to match.
      ) where    
    import Data.Graph.Inductive
    import Data.String (String)
    import Data.List (intersect)
    import Data.Maybe (isJust)

-- REFACTORING from triplets to general n-ary relationships
    -- Node,Edge: FGL. Expr, Rel: DWT|Mindmap.
    -- ? make "arity" a type
      -- its support is the integers in [1,k]
      -- RelPositions and RelExprs contain one
        -- or RelPosition contains a number that varies in [1,k]
          -- where k is the arity

    data MmExpr = StrExpr String | RelExpr Int
      deriving (Show,Eq,Ord)

    data MmEdge = RelTemplate | RelPosition Int -- hide this type from user
      deriving (Show,Eq,Ord)

    type Mindmap = Gr MmExpr MmEdge
    -- how to read edges
      -- in (n,m,lab :: MmLab) :: LEdge MmLab, n is a triplet referring to m
      -- that is, predecessors refer to successors 
      -- (in that relationship; maybe there will be others

-- build mindmap
    insStrExpr :: String -> Mindmap -> Mindmap
    insStrExpr str g = insNode (int, StrExpr str) g
      where int = head $ newNodes 1 g

    insRelExpr :: (Node, Node, Node) -> Mindmap -> Mindmap -- obsoleting
    insRelExpr (n,r,m) g = insEdge (newNode, n, RelPosition 1)
                         $ insEdge (newNode, r, RelPosition 2)
                         $ insEdge (newNode, m, RelPosition 3)
                         $ insNode (newNode, RelExpr 3) g -- add 1 node, 3 edges
      where newNode = head $ newNodes 1 g

    insRelExpr' :: Node -> [Node] -> Mindmap -> Mindmap -- TODO: test, replace 1st
    insRelExpr' t ns g = f (zip ns [1..len]) g' -- t is like ns but tplt
      where len = length ns
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, RelTemplate)
               $ insNode (newNode, RelExpr len) g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelPosition $ snd p) g

-- query mindmap
  -- mmRelvs: to find neighbors
    relExprLab :: MmEdge -> LEdge MmEdge -> Bool
    relExprLab mmLab (m,n,lab) = lab == mmLab

    mmReferents :: Mindmap -> MmEdge -> Int -> Node -> [Node]
    mmReferents g e arity n =
      let pdrNode      (m,n,lab) = m
          hasLab mmLab (m,n,lab) = lab == mmLab
          isKAryRel m = lab g m == (Just $ RelExpr arity)
      in [m | (m,n,lab) <- inn g n, lab == e, isKAryRel m]

    mmRelvs' :: Mindmap -> [Maybe Node] -> [Node]
    mmRelvs' g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, Int)]
            f (Just n, 0) = mmReferents g RelTemplate     arity n
            f (Just n, k) = mmReferents g (RelPosition k) arity n
            listIntersect (x:xs) = foldl intersect x xs
