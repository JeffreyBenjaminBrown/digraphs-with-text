-- imports
    module Dwt.Subgraph where
    import Dwt.Graph
-- types
    data GVar = It | These | Any | GVar NodeIdx
      --It: the Stmt that view-contains|precedes this one
      --These: what the ViewCmd this NodeVar appears in will return 
      --Any: "disregard whatever appears in this position"
      --example: "It needs These for Any when (StmtIdx 3)"
        --returns all y such that there exists x such that It needs y for x when Stmt 3
    data VRel = VRel { _vRelTplt :: StmtIdx   
                     , _vRelMbrs :: [GVar] }
    data Subgraph =
      TheNodes [NodeIdx]
      | Branch { _forkRoot :: NodeIdx
               , _forkVRel :: VRel
               , _maxDepth :: Maybe Int } -- should be > 0
      | Union Subgraph Subgraph
      | Intersect Subgraph Subgraph
      | Diff Subgraph Subgraph -- the first, minus the second

