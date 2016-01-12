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
    _groupEdges :: [LEdge b] -> [[LEdge b]] -> [[LEdge b]]
    _groupEdges [] els = els
    _groupEdges (e:es) [] = _groupEdges es [[e]]
    _groupEdges   (( a, b, c):es )  
                 (((aa,bb,cc):es'):els') = if a == aa
      then _groupEdges es $   ((a,b,c):(aa,bb,cc):es'):els'
      else _groupEdges es $ [(a,b,c)]:((aa,bb,cc):es'):els'

    groupEdges :: Ord b => [LEdge b] -> [[LEdge b]]
    groupEdges es = map (sortOn (\(_,_,c)->c))
      $ _groupEdges es []

    graphToText g = concat $ map (++ "\n") $ 
         (map show $ labNodes g)
      ++ (map (concat . intersperse "," . map show) $ groupEdges $ labEdges g)
