
--  for graphs
    -- in progress
    -- TODO ! fails silently. use Either.
    replaceNode :: Graph Gr => LNode -> Gr a b -> Either String (Gr a b)
    replaceNode (adr,_) (match adr -> (Nothing,g)) = g
    replaceNode (adr,dat) (match adr -> (Just (a,b,c,d),g)) = 

Before I realized that I could use the ViewPatterns extension in the above, the body of one of those cases looked like this:

      let ( , -- TODO ! how is typing this so hard?
                -- maybe because: there's a name in it irrelevant to me
                    -- 
                    -- <interactive>:1:1: Not in scope: ‘match'’
                    -- *Main M Data.Either> :i MContext 
                    -- type MContext a b = Maybe (Context a b)
                    --         -- Defined in ‘Data.Graph.Inductive.Graph’
                    -- *Main M Data.Either> :i Context
                    -- type Context a b = (Adj b, Dwt.Node, a, Adj b)
                    --         -- Defined in ‘Data.Graph.Inductive.Graph’
                    -- *Main M Data.Either> 
          g') = match adr g
      in 
