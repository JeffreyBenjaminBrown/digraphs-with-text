import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS

u = ()
ln :: Node -> LNode ()
ln x = (x,u)
le :: Edge -> LEdge ()
le (a,b) = (a,b,u)

-- | a sortable graph
g = mkGraph (map ln [4,2,3,1]) (map le [(1,2),(3,4),(1,3),(2,4)]) :: Gr () ()
-- cycles g

-- | a cyclic graph
h = mkGraph (map ln [4,2,3,1]) (map le [(1,2),(3,4),(1,3),(2,4),(4,1)]) :: Gr () ()
-- cycles h
