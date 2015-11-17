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

-- REFACTORING: destination types have ' appended
    data MmExpr = StrExpr String | RelExpr
      deriving (Show,Eq,Ord)
    -- data MmExpr' = StrExpr' String | RelExpr' Int
    -- deriving (Show,Eq,Ord)

    data MmLab = MmLab1 | MmLab2 | MmLab3 -- corresponding to MTrip's 3
      deriving (Show,Eq,Ord)
    -- data MmLab' = MmLab' Int -- int for which of the Tplt's _s an Expr corresps to
    --   deriving (Show,Eq,Ord)

    type Mindmap = Gr MmExpr MmLab
    -- type Mindmap' = Gr MmExpr' MmLab'

-- build mindmap
    mmEmpty = empty :: Mindmap
    -- mmEmpty' = empty :: Mindmap'

    insStrExpr str g = insNode (int, StrExpr str) g
      where int = head $ newNodes 1 g

    insRelExpr (n,r,m) g = insEdge (newNode, n, MmLab1)
                         $ insEdge (newNode, r, MmLab2)
                         $ insEdge (newNode, m, MmLab3)
                         $ insNode (newNode, RelExpr) g -- add 1 node, 3 edges
      where newNode = head $ newNodes 1 g

-- query mindmap
    -- if (n,m,lab :: MmLab) :: LEdge MmLab, then n is triplet referring to m
      -- that is, predecessors refer to successors 
        -- (in that relationship; maybe there will be others

  -- mmRelvs: to find neighbors
    relExprLab :: MmLab -> LEdge MmLab -> Bool
    relExprLab mmLab (m,n,lab) = lab == mmLab

    mmRelvs :: Mindmap -> (Maybe Node, Maybe Node, Maybe Node) -> [Node]
    mmRelvs g (Just n, Nothing, Nothing) = 
      map (\(m,n,l)-> m) $ filter (relExprLab MmLab1) $ inn g n
    mmRelvs g (Nothing, Just n, Nothing) = 
      map (\(m,n,l)-> m) $ filter (relExprLab MmLab2) $ inn g n
    mmRelvs g (Nothing, Nothing, Just n) = 
      map (\(m,n,l)-> m) $ filter (relExprLab MmLab3) $ inn g n
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

  --

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
