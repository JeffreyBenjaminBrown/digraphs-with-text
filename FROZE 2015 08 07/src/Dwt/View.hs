-- This code is very tentative.
-- imports
    module Dwt.View where
    import Dwt.Graph
    import Dwt.Subgraph

    -- data ViewStmt = ViewStmt StmtIdx | ViewStmtVar ViewVar
    -- data ViewRel = ViewRel RelIdx | ViewRelVar ViewVar -- ? don't need?
    -- data ViewNode = ViewNode NodeIdx | ViewNodeVar ViewVar
    data View = View Subgraph [View]
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

