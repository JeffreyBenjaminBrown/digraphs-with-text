module TData (
  g1, g1Alt
  , firstMemberOfAnyTplt, tpltForRelsWithDogInPos1
  , fromNeedsTo, dogAsTplt
  , needsFor
  , dog
  , water
  , brandy
  , dogNeedsWaterForBrandy
  , dogNeedsWater
  , anyNeedsWater
  , dogNeedsFromForTo
  , itNeedsFromForTo
  , dogWantsBrandy
  , dogWantsBrandyIsDubious
  , anyNeedsFromForTo
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

g1Alt = insLeaf (Fl 1.3)           $ insLeaf (Fl 1)
      $ insRelUsf 9 [5,10] 
      $ insLeaf (Word "dubious")     $ insLeaf (mkTplt "_ is _")
      $ insRelUsf 7 [0,3,4] $ insLeaf (mkTplt"_ needs _ for _")
      $ insRelUsf 2 [0,3]   $ insRelUsf 1 [0,4]
      $ insLeaf (Word"brandy")    $ insLeaf (Word"water")
      $ insLeaf (mkTplt"_ needs _")  $ insLeaf (mkTplt"_ wants _")
      $ insLeaf (Word"dog")         $ empty :: RSLT

needsFor = QLeaf $ mkTplt "_ needs _ for _" :: QNode
dog = QLeaf $ Word "dog" :: QNode
water = QLeaf $ Word "water" :: QNode
brandy = QLeaf $ Word "brandy" :: QNode
dogNeedsWaterForBrandy = At 8 :: QNode
dogNeedsWater = QRel ["needs"] [dog,water] :: QNode
dogWantsBrandy = QRel ["wants"] [dog,brandy] :: QNode
dogWantsBrandyIsDubious = At 11 :: QNode

anyNeedsWater :: QRelSpec
anyNeedsWater = Map.fromList
  [(TpltRole, QNodeSpec $ QLeaf $ mkTplt "_ needs _")
  ,(Mbr 1, QVarSpec Any)
  ,(Mbr 2, QNodeSpec $ QLeaf $ Word "water")]

anyNeedsFromForTo :: QRelSpec
anyNeedsFromForTo = Map.fromList
  [(TpltRole, QNodeSpec $ QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QVarSpec Any)
  ,(Mbr 2, QVarSpec From)
  ,(Mbr 3, QVarSpec To)]

dogNeedsFromForTo :: QRelSpec
dogNeedsFromForTo = Map.fromList
  [(TpltRole, QNodeSpec $ QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QNodeSpec $ QLeaf $ Word "dog")
  ,(Mbr 2, QVarSpec From)
  ,(Mbr 3, QVarSpec To)]

itNeedsFromForTo :: QRelSpec
itNeedsFromForTo = Map.fromList
  [(TpltRole, QNodeSpec $ QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QVarSpec It)
  ,(Mbr 2, QVarSpec From)
  ,(Mbr 3, QVarSpec To)]

firstMemberOfAnyTplt = Map.fromList [ (TpltRole, QVarSpec It)
  -- todo: rename
                       , (Mbr 1,   QNodeSpec $ At 0)
                       , (Mbr 2,   QVarSpec Any)
                       ] :: QRelSpec

tpltForRelsWithDogInPos1 = Map.fromList [ (TpltRole, QVarSpec It)
                                        , (Mbr 1,   QNodeSpec $ At 0)
                                        ] :: QRelSpec

fromNeedsTo =Map.fromList [ (TpltRole, QNodeSpec $ QLeaf $ mkTplt "_ needs _")
                       , (Mbr 1,   QVarSpec From)
                       , (Mbr 2,   QVarSpec To)
                       ] :: QRelSpec

dogAsTplt = Map.fromList [
  (TpltRole, QNodeSpec $ At 0) -- "dog" Word, not Tplt
  , (Mbr 1,   QVarSpec To)
  , (Mbr 2,   QVarSpec From)
  ] :: QRelSpec
