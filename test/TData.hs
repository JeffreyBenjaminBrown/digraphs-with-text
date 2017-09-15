    module TData (
      g1, g1Alt
      , tRelSpec, tRelSpec2, tRelSpecNonsense
      , tRelSpecXX2, tRelSpecXXNonsense
    ) where

    import Dwt
    import Data.Graph.Inductive
    import qualified Data.Map as Map

-- exports
    g1,g1Alt :: RSLT

    g1 = let mbr = RelEdge . Mbr
             tplt = RelEdge TpltRole
      in mkGraph [
          (0, Word "dog"       )
        , (1, mkTplt "_ wants _" )
        , (2, mkTplt "_ needs _" )
        , (3, Word "water"     )
        , (4, Word "brandy"    )
        , (5, Rel             )
        , (6, Rel             )
        , (7, mkTplt "_ needs _ for _")
        , (8, Rel             ) 
        , (9, mkTplt "_ is _")
        , (10, Word "dubious"  )
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
          $ insWord"dubious"     $ insTplt"_ is _"
          $ insRelUsf 7 [0,3,4] $ insTplt"_ needs _ for _"
          $ insRelUsf 2 [0,3]   $ insRelUsf 1 [0,4]
          $ insWord"brandy"      $ insWord"water"
          $ insTplt"_ needs _"  $ insTplt"_ wants _"
          $ insWord"dog"         $ empty :: RSLT

    tRelSpec = Map.fromList [ (TpltRole, VarSpec It)
                           , (Mbr 1,   NodeSpec $ At 0)
                           , (Mbr 2,   VarSpec Any)
                           ] :: RelSpec

    tRelSpec2 = Map.fromList [ (TpltRole, NodeSpec $ At 2)
                           , (Mbr 1,   VarSpec Up)
                           , (Mbr 2,   VarSpec Down)
                           ] :: RelSpec

    tRelSpecNonsense = Map.fromList [ (TpltRole, NodeSpec $ At 0) -- "dog" Word, not Tplt
                                   , (Mbr 1,   VarSpec It)
                                   , (Mbr 2,   VarSpec Up)
                                   ] :: RelSpec

    tRelSpecXX2 = Map.fromList [ (TpltRole, NodeSpec $ At 2)
                           , (Mbr 1,   VarSpec Up)
                           , (Mbr 2,   VarSpec Down)
                           ] :: RelSpec

    tRelSpecXXNonsense = Map.fromList [ (TpltRole, NodeSpec $ At 0) -- "dog" Word, not Tplt
                                   , (Mbr 1,   VarSpec It)
                                   , (Mbr 2,   VarSpec Up)
                                   ] :: RelSpec
