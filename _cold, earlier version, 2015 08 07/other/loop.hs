-- the loop
let s = "_ connects _"
let si = StmtIdx 1
let sni = StmtNodeIdx si
let r = Rel { _template = si, _members = [sni,sni] }
:t (view template r)
:t Set.fromList $ view members r
Set.insert (view template r)  $ Set.fromList $ view members r

