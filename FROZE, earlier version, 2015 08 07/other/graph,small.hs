let si = StmtIdx 0
let sni = StmtNodeIdx si
let s = Stmt "_ needs _"

let ri = RelIdx 1
let ri2 = RelIdx 2
let rni = RelNodeIdx ri
let r = Rel { _template = si, _members = [rni] }

let sn = StmtNode { _stmt = s, _relIdxs = [ri] }
let rn = RelNode  { _rel  = r, _relIdxs = [ri] }

-- Lens examples
view template r
r ^. template 
rn ^? rel -- how to express not as infix?
set relIdxs [ri2] rn
relIdxs .~ [ri2] $ rn

