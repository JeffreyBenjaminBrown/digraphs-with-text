    module Dwt.FileIO
      ( graphToText
      ) where

    import Dwt.Graph
    import Dwt.Parse
    import Data.List (intersperse, sortOn)

-- pretty-print graph for svn|git diff
    -- each line corresponds either to a node, or to the members of a Rel or Coll
      -- (and always in the same order)
    -- so that if you edit a graph, the diff is easily human-readable
    -- TRICKY: uses the fact that labEdges and labNodes sort their output
    -- TODO ? could be more general (in 2 ways)
      -- currently it groups on whether the first coordinates are equal
        -- it assumes that elts with the same first coord are contiguous
        -- in a more general func, that could be forced by sorting first (1 way)
      -- "take the first coord" could be a function argument (the 2nd way)
    _groupEdges :: [LEdge b] -> -- what's left of the input
                   [[LEdge b]] -> -- accumulator, recursive
                   [[LEdge b]]
    _groupEdges [] els = els
    _groupEdges (e:es) [] = _groupEdges es [[e]]
    _groupEdges   (( a, b, c):es )  
                 (((aa,bb,cc):es'):els') = if a == aa
      then _groupEdges es $   ((a,b,c):(aa,bb,cc):es'):els'
      else _groupEdges es $ [(a,b,c)]:((aa,bb,cc):es'):els'

    groupEdges :: Ord b => [LEdge b] -> [[LEdge b]]
    groupEdges es = map (sortOn (\(_,_,c)->c))
      $ _groupEdges es []

    showNodes g = concat 
      $ intersperse ",\n"
      $ (map show $ labNodes g)

    showEdges g = concat 
      $ intersperse ",\n"
      $ (map (concat . intersperse "," . map show) $ groupEdges $ labEdges g)

    graphToText g = "mkGraph [\n" ++ showNodes g ++ "\n][\n"
                                  ++ showEdges g ++ "\n]\n"
