-- export & import
    module Dwt
  ( -- exports:
  module Data.Graph.Inductive -- export for testing, not production
  , module List
  , module Dwt -- exports everything in this file
    -- could be more selective
  -- , module Dwt.Graph -- etc. Will need to import below to match.
  ) where    
    import Data.Graph.Inductive
    import Data.String as String
    import Data.List as List

-- data
    data MmNode = MmStr String.String | MmTrip
      deriving (Show,Eq,Ord)
      -- Adj b = (Node,Node,b) but Adj String = MmTrip would be confusg

    data MmLab = MmLab1 | MmLab2 | MmLab3 -- corresponding to MTrip's 3
      deriving (Show,Eq,Ord)

    type Mindmap = Gr MmNode MmLab

-- build mindmap
    mmEmpty = empty :: Mindmap
    
    insMmStr str g = insNode (int, MmStr str) g
      where int = head $ newNodes 1 g
    
    insMmTrip (n,r,m) g = insEdge (newNode, n, MmLab1)
                        $ insEdge (newNode, r, MmLab2)
                        $ insEdge (newNode, m, MmLab3)
                        $ insNode (newNode, MmTrip) g -- add 1 node, 3 edges
      where newNode = head $ newNodes 1 g

-- query mindmap
  --  mmMatch g (Just n, Nothing, Nothing) =
    --  let outLEdges = out g n :: [LEdge MmLab]
      -- find all edges (m,n) labeled MmLab1
      -- return the list of those m values
    -- mmMatch (Nothing, Just n, Nothing)
      -- find all edges (m,n) labeled MmLab2
      -- ...
    -- mmMatch (Just n, Just m, Nothing)
      -- find all (k,n) and all (k,m)
      -- return the intersection of the two lists of k values
    -- ...

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
    
