-- Francesco Ariis gave me the idea, 
  -- in the thread: Manual type-checking in graphs: Avoidable?
 
*Dwt> type SGraph = Gr String String
*Dwt> let sg = insNode (0,"zero") $ empty :: SGraph
*Dwt> sg
mkGraph [(0,"zero")] []

*Dwt> :set -XExistentialQuantification 
*Dwt> data ShowBox = forall s. Show s => SB s
*Dwt> type ExQuantGraph = Gr ShowBox Int
*Dwt> let g = insNode (0, SB 1) $ insNode (1, SB 'a') $ empty :: ExQuantGraph -- needs type sig to specify the kind (Gr) of Graph

