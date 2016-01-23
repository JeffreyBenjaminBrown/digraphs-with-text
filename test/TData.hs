    module TData where

    import Dwt.Graph
    import qualified Data.Map as Map

    g1,g1Alt :: Mindmap

    g1 = mkGraph
      [   (0, Str "dog"       )
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
      ] [ (5,1, RelTplt), (5,0, RelMbr 1), (5,4,RelMbr 2) -- dog wants brandy
        , (6,2, RelTplt), (6,0, RelMbr 1), (6,3,RelMbr 2) -- dog needs water
        , (8,7, RelTplt), (8,0, RelMbr 1), (8,3,RelMbr 2), (8,4,RelMbr 3) 
          -- dog needs water for brandy
        , (11,9,RelTplt), (11,5,RelMbr 1), (11,10,RelMbr 2) 
          -- [dog wants brandy] is dubious
      ]

    g1Alt =   insRelUsf 9 [5,10] 
          $ insStr"dubious"     $ insTplt"statement _ is _"
          $ insRelUsf 7 [0,3,4] $ insTplt"_ needs _ for _"
          $ insRelUsf 2 [0,3]   $ insRelUsf 1 [0,4]
          $ insStr"brandy"      $ insStr"water"
          $ insTplt"_ needs _"  $ insTplt"_ wants _"
          $ insStr"dog"         $ empty :: Mindmap

    relSpec = Map.fromList [ (RelTplt, It)
                           , (RelMbr 1, MbrSpec 0)
                           , (RelMbr 2, Any)
                           ]
    relSpecNonsense = Map.fromList [ (RelTplt, MbrSpec 0) -- "dog" Str, not Tplt
                                   , (RelMbr 1, It)
                                   , (RelMbr 2, Any)
                                   ]
