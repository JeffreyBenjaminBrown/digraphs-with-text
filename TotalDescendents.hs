    module TotalDescendents where

-- imports
    import qualified Data.Set as S
    import qualified Data.Map as M
    import qualified Data.Maybe as Mb
    
    import MyGraph

-- totalDescendents
    {- Purpose
        Given a set A (for Ancestors) of nodes in graph G, I would like to know the set T (for "Totally descended from") of nodes that are descended entirely from A -- i.e. such that for every t in T, every maximal chain of predecessors starting at t intersects S.
      Algorithm:
        The TotalDescendentsData type holds four sets:
          tdSnv = totally descended from A, successors not visited
          tdSv = totally descended from A, successors visited
            This set could be called "done processing" -- once a node is in tdSv, it never leaves, and it is not further processed except to determine whether other nodes are descended from it.
          undet = total descent from A undetermined
            A node gets into undet by being descended from something in td.
            If it is then verified that all of its parents are in td, the node itself is moved to td (specifically to tdSnv).
          gr = the graph of interest
          td = shorthand for the union of tdSnv and tdSv
        To begin, tdSnv = A: The ancestors are known to be pure, and their children have not been visited yet. tdSv and undet start empty, and gr starts (and remains unchanged henceforth) the initial graph of interest.
        At each iteration, if tdSnv is nonempty, the children of everything in tdSnv are added to undet, unless they have already been accounted for. Then tdSnv is made empty, and what were in it are added to tdSv. Now that tdSnv is empty, the elements of undet are checked; if the parents of any node N in undet lie entirely in tdSv, then N has been determined pure, so it is moved to tdSnv and the cycle restarts.
      A wrinkle, for efficiency:
        To avoid repeatedly checking the same parents of a node in undet, undet is not a set of nodes; rather, it is a map from nodes to sets of nodes. As parents are determined pure, they are removed from those sets.
    -}
    type PredMap = M.Map Glab (S.Set Glab) -- maps a Node to a subset of its prds
      -- specifically, to those prds that have not yet been determined to be totally descended from the ancestral set of interest
    type TotalDescendentsData = (S.Set Glab, S.Set Glab, PredMap, Graph)
      -- = (tdSnv, tdSv, undet, graph), as explained above.

    relsIfNodeExists :: (Node -> S.Set Glab) -> Glab -> Graph -> S.Set Glab
    relsIfNodeExists f glab gr = -- f is prds or sucs. if glab in gr, returns them.
      if Mb.isJust mbNode -- if mbNode is found in gr
         then f $ Mb.fromJust mbNode
         else S.empty
      where mbNode = M.lookup glab gr

    prdsNotYetCleared :: Glab -> S.Set Glab -> Graph -> S.Set Glab
    prdsNotYetCleared n td gr = -- parents of (gr ! n) not in td
      S.difference parents td
      where parents = relsIfNodeExists prds n gr

    addToPredMap :: S.Set Glab -> PredMap -> Graph -> PredMap
      -- for each i in glabs, inserts (prds i) into pm at i
      -- "increments the predMap" by adding more nodes whose prds must be checked for purity.
    addToPredMap glabs pm graph = S.foldr f pm glabs
        where f i pm = if Mb.isJust lu
                then M.insert i (prds $ Mb.fromJust lu) pm
                else pm
                where lu = M.lookup i graph

    visitTdSnvSucs :: TotalDescendentsData -> TotalDescendentsData
    visitTdSnvSucs (tdSnv,  tdSv,  undet,  graph) =
                 (tdSnv', tdSv', undet', graph)
      where td = S.union tdSnv tdSv
            tdSnv' = S.empty
            tdSv' = S.union tdSv tdSnv
            tdSnvSucs = S.foldr -- all successors of td, relevant or not
              (\n acc -> S.union acc $ relsIfNodeExists sucs n graph)
                S.empty tdSnv
            tdSnvSucsRelevant = -- only those not yet accounted for
              -- remove any already in undet because they may have had their parent sets reduced already, to avoid repeating that work
              S.difference tdSnvSucs $ S.union td $ S.fromList $ M.keys undet
            undet' = addToPredMap tdSnvSucsRelevant undet graph

    visitUndetPrds  :: TotalDescendentsData -> TotalDescendentsData
      -- for each pair (i, parents of i) in undet, parents in td are removed
      -- if the result is a pair (i, empty),
        -- then i is removed from undet, placed in tdSnv
    visitUndetPrds  (tdSnv,  tdSv, undet,  graph) =
              (tdSnv', tdSv, undet', graph)
      where td = S.union tdSnv tdSv
            swept = M.map (\set -> S.difference set td) undet
              -- where "sweep" = "remove parents determined pure"
            (cleared, undet') = M.partition S.null swept
            tdSnv' = S.union tdSnv $ S.fromList $ M.keys cleared
              -- tdSnv should be empty, I think, but inclusion won't hurt

    totalDescendents :: S.Set Glab -> Graph -> S.Set Glab
    totalDescendents roots graph = tdSv
      where (_,tdSv,_,_) = totalDescendentsBackend 
                            (roots, S.empty, M.empty, graph)

    totalDescendentsBackend :: TotalDescendentsData -> TotalDescendentsData
    totalDescendentsBackend (a,b,c,gr) =
      if S.null a' -- fails, as does a == a'
        -- a == a' is one iteration slower but should have same effect
        then                         (a',b',c',gr')
        else totalDescendentsBackend (a',b',c',gr')
      where (a',b',c',gr') = visitUndetPrds $ visitTdSnvSucs (a,b,c,gr)

-- EOF

