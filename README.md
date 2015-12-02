# What is Dwt?

Dwt is an editor for hypergraphs with text on them.



# To a programmer

  ## How does Dwt use FGL?
    -- how Dwt.Exprs and Dwt.Rels are made from FGL.Nodes and FGL.Edges
      -- This 10-line file explains it best:
        -- data/minimalGraph.hs
      -- Rels instantiate Tplts
        -- For example, the relationship "fish need water to breathe"
        -- instantiates the template "_ need _ to _"
      -- how to read edges
        -- predecessor Rels are built from their successor Exprs
        -- in (n,m,lab :: MmLab) :: LEdge MmLab, n is a Rel referring to m
          -- usually, m is a Str, but it could be a Rel or even a Tplt


    -- abbreviations
      -- ch = change
      -- mbr = member
        -- in a a k-ary Rel, there are k AsPos Roles for k members to play,
        -- plus one more Role for the Tplt (which must be k-ary) to play
      -- pos = position
      -- rel = relationship
      -- tplt = (relationship) template

