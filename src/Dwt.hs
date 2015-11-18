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
    mmEmpty = empty :: Mindmap

    insStrExpr :: String -> Mindmap -> Mindmap
    insStrExpr str g = insNode (int, StrExpr str) g
      where int = head $ newNodes 1 g

    insRelExpr :: (Node, Node, Node) -> Mindmap -> Mindmap
    insRelExpr (n,r,m) g = insEdge (newNode, n, RelPosition 1)
                         $ insEdge (newNode, r, RelPosition 2)
                         $ insEdge (newNode, m, RelPosition 3)
                         $ insNode (newNode, RelExpr 3) g -- add 1 node, 3 edges
      where newNode = head $ newNodes 1 g

    insRelExpr' :: Node -> [Node] -> Mindmap -> Mindmap -- TODO: test, replace 1st
    insRelExpr' t ns g = f (zip ns [1..len]) g'
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

    -- TODO: Rewrite mmRelvs for general n-ary rels
    mmRelvs :: Mindmap -> (Maybe Node, Maybe Node, Maybe Node) -> [Node]
    mmRelvs g (Just n, Nothing, Nothing) = 
      map (\(m,n,l)-> m) $ filter (relExprLab $ RelPosition 1) $ inn g n
    mmRelvs g (Nothing, Just n, Nothing) = 
      map (\(m,n,l)-> m) $ filter (relExprLab $ RelPosition 2) $ inn g n
    mmRelvs g (Nothing, Nothing, Just n) = 
      map (\(m,n,l)-> m) $ filter (relExprLab $ RelPosition 3) $ inn g n
    mmRelvs g (Just n1, Just n2, Nothing) = 
      intersect (mmRelvs g (Just n1, Nothing, Nothing))
                (mmRelvs g (Nothing, Just n2, Nothing))
    mmRelvs g (Just n1, Nothing, Just n3) = 
      intersect (mmRelvs g (Just n1, Nothing, Nothing))
                (mmRelvs g (Nothing, Nothing, Just n3))
    mmRelvs g (Nothing, Just n2 , Just n3) = 
      intersect (mmRelvs g (Nothing, Just n2, Nothing))
                (mmRelvs g (Nothing, Nothing, Just n3))
    mmRelvs g (Just n1, Just n2 , Just n3) = 
      intersect (mmRelvs g (Just n1, Just n2, Nothing))
                (mmRelvs g (Nothing, Nothing, Just n3))

    mmReferents :: Mindmap -> Node -> MmEdge -> [Node]
      -- TODO: Add an integer argument k, indicating is for k-ary relationships.
    mmReferents g n e =
      let pdrNode      (m,n,lab) = m
          hasLab mmLab (m,n,lab) = lab == mmLab
      in [m | (m,n,lab) <- inn g n, lab == e]
--      in map (\(m,n,l) -> m)  -- the first way I was doing that
--         $ filter (\(m,n,lab) -> lab == e) 
--         $ inn g n

    mmRelvs' :: Mindmap -> [Maybe Node] -> [Node]
    mmRelvs' g mns = listIntersect $ map f jns
      where jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, Int)]
            f (Just n, 0) = mmReferents g n RelTemplate
            f (Just n, k) = mmReferents g n $ RelPosition k
            listIntersect (x:xs) = foldl intersect x xs

--    mmRelvs' :: Mindmap -> Maybe Node -> [Maybe Node] -> [Node]
--    mmRelvs' g (Just n) ns =
--      map (\(m,n,l)-> m) $ filter (relExprLab $ RelPosition 1) $ inn g n

-- Sum data type
    -- intention: use the Sum type below in the Graph type 
      -- implementing eval as a recursive graph function 
    data Sum = Sum {sum1 :: Sum, sum2 :: Sum} | SumConst Int deriving Show
    a = SumConst 3
    b = Sum a a

    eval :: Sum -> Int
    eval s = case s of
      SumConst a -> a
      Sum a b -> eval a + eval b
      -- works! transcript:
        -- *Dwt> (eval a, eval b)
        -- (3,6)
