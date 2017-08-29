-- | Simplifies (and solves) the problem in Add.hs --
-- that is, the problem of inserting a Hash expression into the graph.
-- It has weird recursion, because each Hash expression's sub-expressions
-- are themselves instructions for adding something to the graph,
-- unless that thing is already present.

-- Thanks to Reddit user WarDaf: https://www.reddit.com/r/haskell/comments/6wmrwh/how_to_process_a_tree_of_orderdependent_insertion/

import Data.List
import Data.Ratio
import Data.Void
import Control.Lens

data Op = Add | Mul

instance Show Op where
  show Add = "(+1)"
  show Mul = "(*2)"

readOp :: Op -> (Mdl -> Mdl)
readOp Add = _1 %~ (+1)
readOp Mul = _1 %~ (*2)

type Mdl = (Float,Float)

type Chg = Mdl -> Mdl
data OpNode = Leaf Op         | DoneLeaf Mdl
            | Branch [OpNode] | DoneBranch Mdl [OpNode] deriving Show

doneMdl :: OpNode -> Mdl
doneMdl (DoneLeaf m) = m
doneMdl (DoneBranch m _) = m
doneMdl _ = error "doneMdl only runs on Done OpNodes"

-- basing the mapAccum on this
f :: Mdl -> OpNode -> (Mdl, OpNode)
f m d@(DoneLeaf _) = (m, d)
f m d@(DoneBranch _ _) = (m, d)
f m (Leaf o) = let x = readOp o $ m in (x, DoneLeaf x)
f m b@(Branch bs) = (m3, DoneBranch m3 bs2) where
  (m2,bs2) = mapAccumL f m bs -- first use the children (bs)
  m3 = m2 & _2 %~ g -- then use the parent (b), with its (done) children
  g k = (+k) $ sum $ flip map bs2 $ (^. _1) . doneMdl
    -- g adds k to the sum of the first elts of the models in bs2

ops = [Leaf Add
      , Branch [Leaf Mul, Leaf Mul]
      , Leaf Add, Leaf Add, Leaf Mul]
go = mapAccumL f (0,0) ops

-- | It works:
-- *Main Control.Lens> ops
-- [ Leaf (+1)
--   ,Branch [Leaf (*2),Leaf (*2)]
--   ,Leaf (+1)
--   ,Leaf (+1)
--   ,Leaf (*2)]
-- *Main Control.Lens> go
-- ((12.0,6.0)
--   ,[DoneLeaf (1.0,0.0)
--      ,DoneBranch (4.0,6.0) [DoneLeaf (2.0,0.0),DoneLeaf (4.0,0.0)]
--      ,DoneLeaf (5.0,6.0)
--      ,DoneLeaf (6.0,6.0)
--      ,DoneLeaf (12.0,6.0)])
