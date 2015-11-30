import Dwt

minimalGraph = -- This graph contains the relationship "[the dog] needs [water]" and its constituent elements.
  mkGraph
      [   (0, Str "the dog")
        , (1, stringToTplt "_ wants _")
        , (2, Str "water")
        , (3, Rel 2)
      ] 
      [(3,1, AsTplt), (5,0, AsPos 1), (5,2,AsPos 2)] :: Mindmap
