dwtDfs :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either String [Node]
insRelSpec :: RelSpec -> RSLT -> Either DwtErr RSLT
type RelSpec = Map.Map RelRole AddressOrVar
  -- the others are flexible, but the TpltRole must map to a NodeSpec
data AddressOrVar = VarSpec Mbrship | NodeSpec Node deriving(Show,Read,Eq,Ord)
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
data Mbrship = It | Any | Up | Down
