    import Dwt

    -- mkGraph :: Graph gr => [LNode a] -> [LEdge b] -> gr a b
      -- that is, mkGraph takes a list of nodes followed by a list of edges
    g1, g2 :: Mindmap
    g1 = mkGraph
      [   (0, Word "dog"       )
        , (1, mkTplt "_ wants _" ) -- produces a Tplt with Arity 2
        , (3, Word "water"     )
        , (4, Rel 2           )
      ] [ -- "dog wants water"
            (4,1, TpltRole)  -- Node 1 is the Template for the Rel at Node 4
          , (4,0, RelMbr 1) -- Node 0 is the 1st Rel Member of the Rel at Node 4
          , (4,3, RelMbr 2) -- Node 3 is the 2nd Rel Member of the Rel at Node 4
      ]

    g2 = mkGraph
      [   (0, Word "dog"       )
        , (1, mkTplt "_ wants _" )
        , (3, Word "water"     )
        , (4, Rel 2           )
        , (5, mkTplt "_ is _")
        , (6, Word "dubious"   )
        , (7, Rel 2           )
      ] 
      [ -- "dog wants water" is represented just like it was in g1
          (4,1,TpltRole), (4,0, RelMbr 1), (4,3,RelMbr 2),
        -- "[dog wants water] is dubious"
          (7,5,TpltRole),
          (7,4,RelMbr 1), -- Node 4, the first Member of this Rel, is itself a Rel
          (7,6,RelMbr 2)
      ]
