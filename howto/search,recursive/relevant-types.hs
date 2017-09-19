dwtDfs :: RSLT -> (Mbrship,QRelSpec) -> [Node] -> Either String [Node]
insRelSpec :: QRelSpec -> RSLT -> Either DwtErr RSLT
type QRelSpec = Map.Map RelRole QNodeOrVar
  -- the others are flexible, but the TpltRole must map to a QNodeSpec
data QNodeOrVar = QVarSpec Mbrship | QNodeSpec Node deriving(Show,Read,Eq,Ord)
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
data Mbrship = It | Any | Up | Down
