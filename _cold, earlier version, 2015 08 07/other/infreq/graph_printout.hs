Graph {
  _nodeMap = fromList [
    (StmtNodeIdx (StmtIdx 1),StmtNode {_stmt = Stmt "_ needs _", 
                                       _relIdxs = [RelIdx 1]}),
    (StmtNodeIdx (StmtIdx 2),StmtNode {_stmt = Stmt "dog", 
                                       _relIdxs = [RelIdx 1]}),
    (StmtNodeIdx (StmtIdx 3),StmtNode {_stmt = Stmt "collar", 
                                       _relIdxs = [RelIdx 2]}), --false
    (RelNodeIdx (RelIdx 1),RelNode {
      _rel = Rel {
        _template = StmtIdx 1, 
        _members = [StmtNodeIdx (StmtIdx 2),StmtNodeIdx (StmtIdx 3)]},
      _relIdxs = []}),
    (RelNodeIdx (RelIdx 2),RelNode {
      _rel = Rel {
        _template = StmtIdx 1, 
        _members = [StmtNodeIdx (StmtIdx 2),StmtNodeIdx (StmtIdx 3)]}, 
      _relIdxs = []})], 
  _maxStmtIdx = 3, 
  _maxRelIdx = 2
}

