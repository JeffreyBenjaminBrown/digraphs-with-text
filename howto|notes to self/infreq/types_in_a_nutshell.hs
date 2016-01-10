I use the Functional Graph Library (FGL) to implement something resembling a hyperggraph, which I'm calling Mindmap, in which relationships can involve any number of things, including other relationships. (By contrast, in a graph, Edges cannot belong to other Edges; only Nodes can.)

Here are the types:

    -- Exprs (expressions) play Roles in Rels (relationships).
      -- A k-ary (Arity k) Rel consists of a k-ary template and k members.
    -- Each k-ary Rel emits k+1 Edges toward the other Exprs:
      -- one connects it to its RelTplt (relationship template)
      -- k more connect it to each of its k RelMbrs (relationship members)
    -- The two paragraphs after it will clear up any questions about the next.

    type Mindmap = Gr Expr Role
    data Role = RelTplt | RelMbr RelPos
      deriving (Show,Read,Eq,Ord)
    data Expr = Str String | Tplt Arity [String] | Rel Arity
      -- TODO ? deduce the Arity of a Tplt from its [String]
      -- TODO ? deduce from the graph the Arity of a Rel
        -- rather than carrying it redundantly in the Rel constructor
      deriving (Show,Read,Eq,Ord)
    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    type Arity = Int

The following is a Mindmap that represents the expression "dog needs water" using the subexpressions "dog" (a string), "water" (a string), and "_ wants _" (a relationship two things can have, that is a binary Rel):

    -- mkGraph :: Graph gr => [LNode a] -> [LEdge b] -> gr a b
      -- that is, mkGraph takes a list of nodes followed by a list of edges
    g1 :: Mindmap
    g1 = mkGraph
      [   (0, Str "dog"       )
        , (1, stringToTplt "_ wants _" ) -- produces a Tplt with Arity 2
        , (3, Str "water"     )
        , (4, Rel 2           )
      ] [ -- "dog wants water"
            (4,1, RelTplt)  -- Node 1 is the Template for the Rel at Node 4
          , (4,0, RelMbr 1) -- Node 0 is the 1st Rel Member of the Rel at Node 4
          , (4,3, RelMbr 2) -- Node 3 is the 2nd Rel Member of the Rel at Node 4
      ]

The next Mindmap encodes the previous statement and a second statement stating that the first is dubious:

    g2 :: Mindmap
    g2 = mkGraph
      [   (0, Str "dog"       )
        , (1, stringToTplt "_ wants _" )
        , (3, Str "water"     )
        , (4, Rel 2           )
        , (5, stringToTplt "_ is _")
        , (6, Str "dubious"   )
        , (7, Rel 2           )
      ] 
      [ -- "dog wants water" is represented just like it was in g1
          (4,1,RelTplt), (4,0, RelMbr 1), (4,3,RelMbr 2),
        -- "[dog wants water] is dubious"
          (7,5,RelTplt),
          (7,4,RelMbr 1), -- Node 4, the first Member of this Rel, is itself a Rel
          (7,6,RelMbr 2)
      ]
