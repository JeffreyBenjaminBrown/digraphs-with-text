import Dwt

minimalGraph = mkGraph
    [   (0, Str "the dog")
      , (1, stringToTplt "_ wants _") -- a template3 for relationships
      , (2, Str "water")
      , (3, Rel 2) -- the relationship "[the dog] needs [water]"
    ] 
    [ (3,1, AsTplt),  -- Node 1 serves as the template for Rel 3
      (5,0, AsPos 1), -- Node 0 appears in the first position in Rel 3
      (5,2,AsPos 2)   -- Node 2 appears in the second position in Rel 3
    ] :: Mindmap

-- Edge orientation matters
  -- Predecessor Rels are built from|refer to their successor Exprs.
  -- That is, in the LEdge (n,m,label), n is a Rel referring to m.
  -- Usually, m is a Str, but it could be a Rel or even a Tplt.
