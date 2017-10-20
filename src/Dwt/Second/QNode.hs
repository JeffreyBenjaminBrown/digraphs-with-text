module Dwt.Second.QNode (
  qNodeTop -- QNode -> QNode
  , qNodeIsTop -- QNode -> QNode
  , toRoleMap -- QNode -> Either DwtErr RoleMap
  , mkRoleMap -- QNode -> [QNode] -> RoleMap
  ) where

import Dwt.Initial.Types
import Dwt.Initial.Measure (extractTplt)
import Dwt.Second.MkTplt (jointsToTplt)
import qualified Data.Map as Map

qNodeTop :: QNode -> QNode
qNodeTop (QRel _ js qs) = QRel True js qs
qNodeTop x = x

qNodeIsTop :: QNode -> Bool
qNodeIsTop (QRel True _ _) = True
qNodeIsTop _ = False

toRoleMap :: QNode -> Either DwtErr RoleMap
toRoleMap q@(QRel _ js qs) = do t <- extractTplt q
                                let tPair = (TpltRole, QLeaf t)
                                    mPairs = zip (map Mbr [1..]) qs
                                Right $ Map.fromList $ tPair : mPairs
toRoleMap x = Left (ConstructorMistmatch, [ErrQNode x], "toRoleMap.")

-- | Applies only when all the nodes the Rel involves are known.
mkRoleMap :: QNode -> [QNode] -> RoleMap
mkRoleMap t ns = Map.fromList $ (TpltRole, t) : mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) ns
