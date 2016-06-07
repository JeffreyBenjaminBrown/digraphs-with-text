    import Data.Graph.Inductive

    type Arity = Int -- tuples have Arity: pairs have arity 2, triples arity 3 ..
    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]

    type RSLT = Gr Expr RSLTEdge -- Recursive Set of Labeled Tuples
      -- Read this as "a graph in which which
      -- each node is an Expr and each edge is an RSLTEdge

    data Expr = Str String
              | Rel  -- labeled tuple (LT), like "[cats] need [sun]"
              | Tplt [String] -- template for a labeled tuple, like "_ did _ to _"

    data RSLTEdge = RelTplt | Mbr RelPos
      -- any Expr can receive an edge (be a member of a superexpression)
      -- only Rels emit edges (have subexpressions)
      -- each k-ary Rel emits exactly one RelTplt edge and k Mbr edges
        -- with the Mbr edges labeled 1 through k
