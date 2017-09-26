module Dwt.Second.Misc (
  toRoleMap -- QNode -> Either DwtErr RoleMap
  ) where

import Dwt.Initial.Types
import Dwt.Second.MkTplt (jointsToTplt)
import qualified Data.Map as Map

toRoleMap :: QNode -> Either DwtErr RoleMap
toRoleMap (QRel js qs) = Right $ Map.fromList $ t : ms where
  t = (TpltRole, QLeaf $ jointsToTplt js)
  ms = zip (map Mbr [1..]) qs
toRoleMap x = Left (ConstructorMistmatch, [ErrQNode x], "toRoleMap.")

