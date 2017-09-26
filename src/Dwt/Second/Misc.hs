module Dwt.Second.Misc (
  toRoleMap -- QNode -> Either DwtErr RoleMap
  ) where

import Dwt.Initial.Types
import Dwt.Initial.Measure (extractTplt)
import Dwt.Second.MkTplt (jointsToTplt)
import qualified Data.Map as Map

toRoleMap :: QNode -> Either DwtErr RoleMap
toRoleMap q@(QRel js qs) = do t <- extractTplt q
                              let tPair = (TpltRole, QLeaf t)
                                  mPairs = zip (map Mbr [1..]) qs
                              Right $ Map.fromList $ tPair : mPairs
toRoleMap x = Left (ConstructorMistmatch, [ErrQNode x], "toRoleMap.")

