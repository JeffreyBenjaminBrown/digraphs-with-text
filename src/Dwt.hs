module Dwt
  ( -- exports:
  module Data.Graph.Inductive -- export for testing, not production
  , module Dwt -- exports everything in this file
    -- could be more selective
  -- , module Dwt.Graph -- etc. Will need to import below to match.
  ) where
import Data.Graph.Inductive

myEmpty = empty :: Gr String String

insString s g = insNode (i,s) g
  where i = head $ newNodes 1 g

insStringEdge s n1 n2 g = insEdge (n1,n2,s) g

-- 2015 11 13
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

