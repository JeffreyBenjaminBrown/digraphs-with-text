    module TData (
      g1, g1Alt
      , relSpec, relSpec2, relSpecNonsense
    ) where

    import Dwt.Graph
    import Data.Graph.Inductive
    import qualified Data.Map as Map

-- exports
    g1,g1Alt :: Mindmap

    g1 = let mbr = RelEdge . Mbr
             tplt = RelEdge RelTplt
      in mkGraph [
          (0, Str "dog"       )
        , (1, stringToTplt "_ wants _" )
        , (2, stringToTplt "_ needs _" )
        , (3, Str "water"     )
        , (4, Str "brandy"    )
        , (5, Rel             )
        , (6, Rel             )
        , (7, stringToTplt "_ needs _ for _")
        , (8, Rel             ) 
        , (9, stringToTplt "statement _ is _")
        , (10, Str "dubious"  )
        , (11, Rel            )
        , (12, Fl 1)
        , (13, Fl 1.3)
      ] [ (5,1, tplt), (5,0, mbr 1), (5,4,mbr 2) -- dog wants brandy
        , (6,2, tplt), (6,0, mbr 1), (6,3,mbr 2) -- dog needs water
        , (8,7, tplt), (8,0, mbr 1), (8,3,mbr 2), (8,4,mbr 3) 
          -- dog needs water for brandy
        , (11,9,tplt), (11,5,mbr 1), (11,10,mbr 2) 
          -- [dog wants brandy] is dubious
      ]

    g1Alt = insFl 1.3           $ insLeaf (Fl 1)
          $ insRelUsf 9 [5,10] 
          $ insStr"dubious"     $ insTplt"statement _ is _"
          $ insRelUsf 7 [0,3,4] $ insTplt"_ needs _ for _"
          $ insRelUsf 2 [0,3]   $ insRelUsf 1 [0,4]
          $ insStr"brandy"      $ insStr"water"
          $ insTplt"_ needs _"  $ insTplt"_ wants _"
          $ insStr"dog"         $ empty :: Mindmap

    relSpec = Map.fromList [ (RelTplt, VarSpec It)
                           , (Mbr 1,   NodeSpec 0)
                           , (Mbr 2,   VarSpec Any)
                           ]

    relSpec2 = Map.fromList [ (RelTplt, NodeSpec 1)
                           , (Mbr 1,   VarSpec Ana)
                           , (Mbr 2,   VarSpec Kata)
                           ]

    relSpecNonsense = Map.fromList [ (RelTplt, NodeSpec 0) -- "dog" Str, not Tplt
                                   , (Mbr 1,   VarSpec It)
                                   , (Mbr 2,   VarSpec Ana)
                                   ]
