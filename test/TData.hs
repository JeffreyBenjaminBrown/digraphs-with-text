{-# LANGUAGE OverloadedStrings #-}

module TData (
  -- | = RSLT
  g1, g1Alt

  -- | = QNode
  , dogNeedsWaterForBrandy
  , qAny

  -- | == QLeaf
  , needsFor
  , dog
  , water
  , brandy

  -- | == QRel
  , dogNeedsWater
  , dogWantsBrandy
  , dogWantsBrandyIsDubious
  , anyNeedsAny
  , anyNeedsAnyIsAny

  -- | = RoleMap
  , anyNeedsWaterRM
  , anyNeedsFromForToRM
  , brandyNeedsFromForToRM
  , dogNeedsFromForToRM
  , itNeedsFromForToRM
  , firstMemberOfAnyTpltRM
  , tpltForRelsWithDogInPos1RM
  , dogAsTpltRM

  -- | = QRelspec (deprecated)
  , firstMemberOfAnyTplt_Relspec
) where

import Dwt
import Data.Graph.Inductive
import qualified Data.Map as Map


-- | = RSLT
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

-- | = QNode
dogNeedsWaterForBrandy = At 8 :: QNode
qAny = QVar Any :: QNode

-- | == QLeaf
needsFor = QLeaf $ mkTplt "_ needs _ for _" :: QNode
dog = QLeaf $ Word "dog" :: QNode
water = QLeaf $ Word "water" :: QNode
brandy = QLeaf $ Word "brandy" :: QNode

-- | == QRel
dogNeedsWater = QRel ["needs"] [dog,water] :: QNode
dogWantsBrandy = QRel ["wants"] [dog,brandy] :: QNode
dogWantsBrandyIsDubious = At 11 :: QNode
anyNeedsAny = QRel ["needs"] [qAny, qAny] :: QNode
anyNeedsAnyIsAny = QRel ["is"] [anyNeedsAny, qAny] :: QNode

-- | = RoleMap
anyNeedsWaterRM :: RoleMap
anyNeedsWaterRM = Map.fromList
  [(TpltRole, QLeaf $ mkTplt "_ needs _")
  ,(Mbr 1, QVar Any)
  ,(Mbr 2, QLeaf $ Word "water")]

anyNeedsFromForToRM :: RoleMap
anyNeedsFromForToRM = Map.fromList
  [(TpltRole, QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QVar Any)
  ,(Mbr 2, QVar From)
  ,(Mbr 3, QVar To)]

brandyNeedsFromForToRM :: RoleMap
brandyNeedsFromForToRM = Map.fromList
  [(TpltRole, QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QLeaf $ Word "brandy")
  ,(Mbr 2, QVar From)
  ,(Mbr 3, QVar To)]

dogNeedsFromForToRM :: RoleMap
dogNeedsFromForToRM = Map.fromList
  [(TpltRole, QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QLeaf $ Word "dog")
  ,(Mbr 2, QVar From)
  ,(Mbr 3, QVar To)]

itNeedsFromForToRM :: RoleMap
itNeedsFromForToRM = Map.fromList
  [(TpltRole, QLeaf $ mkTplt "_ needs _ for _")
  ,(Mbr 1, QVar It)
  ,(Mbr 2, QVar From)
  ,(Mbr 3, QVar To)]

firstMemberOfAnyTpltRM = Map.fromList [ (TpltRole, QVar It)
  -- todo: rename
                       , (Mbr 1,   At 0)
                       , (Mbr 2,   QVar Any)
                       ] :: RoleMap

tpltForRelsWithDogInPos1RM = Map.fromList [ (TpltRole, QVar It)
                                          , (Mbr 1,   At 0)
                                          ] :: RoleMap

dogAsTpltRM = Map.fromList [
  (TpltRole, At 0) -- "dog" Word, not Tplt
  , (Mbr 1,  QVar To)
  , (Mbr 2,  QVar From)
  ] :: RoleMap

-- | = QRelspec (deprecated)
firstMemberOfAnyTplt_Relspec = Map.fromList [ (TpltRole, QVarSpec It)
  -- todo: rename
                       , (Mbr 1,   QNodeSpec $ At 0)
                       , (Mbr 2,   QVarSpec Any)
                       ] :: QRelspec
