    module MyGraph where

-- imports
    import qualified Data.Set as S
    import qualified Data.Map as M
    import qualified Data.Maybe as Mb

-- Node
  -- before functions
    type Glab = Int -- Graph node label
    data Node = Node { -- Graph node
      glab :: Glab -- Each node's should be unique. (TODO: enforce?)
      , prds :: S.Set Glab -- predecessors
      , sucs :: S.Set Glab -- successors
    } deriving (Show, Eq)

  -- create, compare
    diff :: Node -> Node -> Node -- for tests
    diff gn1 gn2 =
      gn1 { sucs = S.difference (sucs gn1) (sucs gn2)
          , prds = S.difference (prds gn1) (prds gn2)
          }

    isInNoEdges :: Node -> Bool
    isInNoEdges g = prds g == S.empty && sucs g == S.empty

    blank :: Glab -> Node -- make a Node without relatives
    blank n = Node { glab = n
                    , sucs = S.empty
                    , prds = S.empty
                    }

  -- add, remove relatives
    addPrd :: Node -> Node -> Node
    addPrd newPrd input = -- "input" gains "newPrd" as a predessor
      input { prds = S.insert (glab newPrd) $ prds input }

    addPrdLab :: Glab -> Node -> Node
    addPrdLab newPrdLab input = -- similar, but use a label
      input { prds = S.insert newPrdLab $ prds input }

    rmPrd :: Node -> Node -> Node
    rmPrd lostPrd input = -- "input" loses "lostPrd" as a predessor
      input { prds = S.delete (glab lostPrd) $ prds input }

    rmPrdLab :: Glab -> Node -> Node
    rmPrdLab lostPrd input = -- similar, but use a label
      input { prds = S.delete lostPrd $ prds input }

    -- the next 4 are untested but identical* to the previous 4
      -- *except I swapped every instance of prd -> suc, ignoring caps
    addSuc :: Node -> Node -> Node
    addSuc newSuc input = -- "input" gains "newSuc" as a successor
      input { sucs = S.insert (glab newSuc) $ sucs input }

    addSucLab :: Glab -> Node -> Node
    addSucLab newSucLab input = -- similar, but use a label
      input { sucs = S.insert newSucLab $ sucs input }

    rmSuc :: Node -> Node -> Node
    rmSuc lostSuc input = -- "input" loses "lostSuc" as a successor
      input { sucs = S.delete (glab lostSuc) $ sucs input }

    rmSucLab :: Glab -> Node -> Node
    rmSucLab lostSuc input = -- similar, but use a label
      input { sucs = S.delete lostSuc $ sucs input }

-- Graph
    type Graph = M.Map Glab Node

  -- nodes
    blankGraphFromList :: [Glab] -> Graph
    blankGraphFromList = foldr (\elt acc -> add (blank elt) acc) M.empty

    add :: Node -> Graph -> Graph -- can overwrite; indeed, often should
    add gn gr = M.insert (glab gn) gn gr

  -- analyze
    grDiff :: Graph -> Graph -> Graph -- for testing
    grDiff gr1 gr2 = M.differenceWith f gr1 gr2
      where f gnLeft gnRight = if isInNoEdges d then Nothing else Just d
              where d = diff gnLeft gnRight

    nextGlab :: Graph -> Glab
    nextGlab gr = (+1) $ fst $ M.findMax gr

  -- edges
    addEdge :: Node -> Node -> Graph -> Graph
    addEdge p s graph = M.insert (glab s) s' $
                        M.insert (glab p) p' graph
      where s' = addPrd p s
            p' = addSuc s p

    rmEdge :: Node -> Node -> Graph -> Graph
    rmEdge p s graph = M.insert (glab s) s' $
                       M.insert (glab p) p' graph
      where s' = rmPrd p s
            p' = rmSuc s p

    modUsingLabels :: (Node -> Node -> Graph -> Graph)
      -> Glab -> Glab -> Graph -> Graph
    modUsingLabels f pl sl graph = -- f shoulde be in {addEdge, rmEdge}
      if Mb.isJust p && Mb.isJust s
        then f (Mb.fromJust p) (Mb.fromJust s) graph
        else graph
      where p = M.lookup pl graph
            s = M.lookup sl graph

-- EOF
