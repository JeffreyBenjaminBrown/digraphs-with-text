dwtDfsVerboseTypes :: RSLT -> (Mbrship,RelSpecVerboseTypes) -> [Node] -> Either String [Node]
insRelSpecVerboseTypes :: RelSpecVerboseTypes -> RSLT -> Either DwtErr RSLT
type RelSpecVerboseTypes = Map.Map RelRole NodeOrVarVerboseTypes
  -- the others are flexible, but the TpltRole must map to a NodeSpecVerboseTypes
data NodeOrVarVerboseTypes = VarSpecVerboseTypes Mbrship | NodeSpecVerboseTypes Node deriving(Show,Read,Eq,Ord)
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
data Mbrship = It | Any | Up | Down
