module Dwt.Second.Misc (
  qRelUntop
  , toRoleMap -- QNode -> Either DwtErr RoleMap
  ) where

import Dwt.Initial.Types
import Dwt.Initial.Measure (extractTplt)
import Dwt.Second.MkTplt (jointsToTplt)
import qualified Data.Map as Map

qRelUntop :: QNode -> QNode
qRelUntop (QRelTop js qs) = QRel js qs
qRelUntop q@(QRel _ _) = q
qRelUntop _ = error "qRelUntop: expects a QRel."

toRoleMap :: QNode -> Either DwtErr RoleMap
toRoleMap q@(QRel js qs) = do t <- extractTplt q
                              let tPair = (TpltRole, QLeaf t)
                                  mPairs = zip (map Mbr [1..]) qs
                              Right $ Map.fromList $ tPair : mPairs
toRoleMap x = Left (ConstructorMistmatch, [ErrQNode x], "toRoleMap.")

