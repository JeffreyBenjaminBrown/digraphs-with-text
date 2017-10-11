module Dwt.Second.Misc (
  qNodeTop -- QNode -> QNode
  , qNodeIsTop -- QNode -> QNode
  , toRoleMap -- QNode -> Either DwtErr RoleMap
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
