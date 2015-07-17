-- This code is very tentative.
    module Dwt.View where
    import Dwt

    data ViewVar = It | These | Something
      --It: the Stmt that view-contains|precedes this one
      --These: what the ViewCmd this NodeVar appears in will return 
      --Something: "disregard whatever appears in this position"
      --example: "It needs These for Something when (StmtIdx 3)"
        --returns all y such that It needs y for anything when Stmt 3
    data ViewStmt = ViewStmt StmtIdx | ViewStmtVar ViewVar
    --data ViewRel = ViewRel RelIdx | ViewRelVar ViewVar -- ? don't need?
    data ViewNode = ViewNode NodeIdx | ViewNodeVar ViewVar
    data View = View ViewCmd [View]
    data ViewCmd = --not Cmd, this is just Subgraph 
      OneNode NodeIdx --make it a whole list (or set)
      | Fork { forkRelTplt :: ViewStmt
             , forkRelMbrs :: [ViewNode] }
      | Branch { branchRoots :: [ViewNode]
               , branchRels :: [(ViewStmt,[ViewNode])] }
                --DO? branchRels is a list of forks; could reify
-- example
  -- regarding the following view ...
    -- View (NodeView (NodeIdx 5)) -- Node 5 = the volcano
    --     [ View FanView { fanRelTplt = RelNodeIdx 1, -- Rel 1 = "_ has _ for _"
    --         fanMembers = [It,These,Something] } -- "It has These"
    --     , View FanView { fanRelTplt = RelNodeIdx 2, -- Rel 2 ="_ needs _ to _"
    --         fanMembers = [It,NodeVar $ NodeIdx 10, These] } -- 10 = the gods
    --     ]
  -- -- ... that view should display like this:
    -- the volcano -- NodeIdx 5 goes to "the volcano"
    --   ## It has These for Something. 
    --   -- "These" refers to "rocks" and "myths".  
    --   rocks -- because: the volcano has rocks for children
    --   myths -- because: the volcano has myths for fooling dragons
    --   ## It needs the gods to These.
    --   chill out
    --   give blessings
    -- 
-- eof

