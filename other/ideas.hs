-- view idea
  data StmtSpec = StmtSpec {idx :: Int} | It | These

  -- a graph
    Stmt 2 = "_ needs _"

  -- a view
    StmtSpec 1
      relvs (StmtSpec 2) [It, These]
        -- 1-indents under Stmt 1 all Stmts S s.t. Stmt 1 needs S
        StmtSpec 5 -- Stmt 1 had better need Stmt 5
          rel (StmtSpec 2) [It, These] -- 1-indents under Stmt 5 all S s.t. Stmt 5 needs S 
      relvs (StmtSpec 3) [These, 4, It] -- Ternary relationships work too

_ needs _ = Stmt 2

Stmt 1
  Fan (Stmt 2) (It, These) == Every Stmt x such that "It needs x"  
  Its children
Stmt 2
  Its parents
    Stmt 5
      Its parents
    Stmt 4
      Its parents
  Every Stmt x such that Stmt 2 needs Stmt y1
                         Stmt y1 needs Stmt y2
                         ...                y3
                         ...
                         Stnt yn needs Stmt x
  == Wedge (origin = Stmt 2) (rel = (Stmt 2, (This, Next) ) )



<Stmt 1>
  "Fan (Stmt 2) (It, These)"
  <Stmt 5>
  <Stmt 6>
  "... It ..."
  <...>
  <...>


-- eof

