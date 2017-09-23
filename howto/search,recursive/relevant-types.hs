dwtDfs :: RSLT -> (SearchVar,QRelspec) -> [Node] -> Either String [Node]
insRelspec :: QRelspec -> RSLT -> Either DwtErr RSLT
type QRelspec = Map.Map RelRole QNodeOrVar
  -- the others are flexible, but the TpltRole must map to a QNodeSpec
data QNodeOrVar = QVarSpec SearchVar | QNodeSpec Node deriving(Show,Read,Eq,Ord)
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
data SearchVar = It | Any | Up | Down
