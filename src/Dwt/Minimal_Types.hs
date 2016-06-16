-- This file is for illustration purposes only; it is not part of the codebase.
-- Once this file makes sense to you,
  -- the natural place to start reading the codebase is at Graph.hs.

import Data.Graph.Inductive

type Arity = Int -- tuples have Arity: pairs have arity 2, triples arity 3 ..
type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]

type RSLT = Gr Expr RSLTEdge -- Recursive Set of Labeled Tuples
  -- Read this as "a graph (Gr) in which
  -- each node is an Expr and each edge is an RSLTEdge"

data Expr = Str String
          | Rel  -- labeled tuple (LT), like "[cats] need [sun]"
          | Tplt [String] -- template for a labeled tuple, like "_ need _"

data RSLTEdge = RelTplt | Mbr RelPos -- !Not every possible edge is valid!
  -- Rule 1: any Expr can receive an edge (be a member of a superexpression),
    -- but only Rels emit edges (have subexpressions).
  -- Rule 2: Each k-ary Rel emits exactly one RelTplt edge and k Mbr edges
    -- with the Mbr edges labeled 1 through k
  --These rules are not reified; respecting them is the coder's responsibility.
