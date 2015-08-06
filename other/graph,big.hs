:{
let si = StmtIdx 1
    sni = StmtNodeIdx si
    s = Stmt "_ needs _"
    sn = StmtNode { _stmt = s, _relIdxs = [ri] }
    si2 = StmtIdx 2
    sni2 = StmtNodeIdx si2
    s2 = Stmt "dog"
    sn2 = StmtNode { _stmt = s2, _relIdxs = [ri] }
    si3 = StmtIdx 3
    sni3 = StmtNodeIdx si3
    s3 = Stmt "collar"
    sn3 = StmtNode { _stmt = s3, _relIdxs = [ri2] } -- ri2: false (it's ri1)
    ri = RelIdx 1
    rni = RelNodeIdx ri
    r = Rel { _tplt = si, _mbrs = [sni2, sni3] } -- dog needs collar
    rn = RelNode  { _rel  = r, _relIdxs = [] }
    ri2 = RelIdx 2
    rni2 = RelNodeIdx ri2
    r2 = Rel { _tplt = si, _mbrs = [sni2, sni2] } -- dog needs dog
    rn2 = RelNode  { _rel  = r, _relIdxs = [] }
    g = Graph { _nodeMap = Map.insert sni sn $ Map.insert sni2 sn2
                $ Map.insert sni3 sn3 $ Map.insert rni rn 
                $ Map.insert rni2 rn2 $ Map.empty
              , _maxStmtIdx = 3
              , _maxRelIdx = 2 }
:}

