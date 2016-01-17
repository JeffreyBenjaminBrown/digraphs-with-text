-- TODO: load, save, parse together; futz elsewhere

-- load
  gf <- readFile "data/agent.dwt"
  let g = read gf :: Mindmap

-- futz
  -- some important nodes
    view g $ pre g 765 -- lets see every Rel involving the root
    view g $ nodes $ labfilter (== stringToTplt "_ .mm/ _") g -- Node 32
    view g $ nodes $ labfilter (== stringToTplt "_ then read-> _") g -- Node 23

  -- how I found that (Nodes obsolete, method still valid)
    length $ nodes g
    matchRel g [Just 32,Nothing,Just 400]
    showExpr g 1583
    matchRel g [Just 32,Nothing,Just 584]
    showExpr g 1600
    matchRel g [Just 32,Nothing,Just 274]
    showExpr g 1753
    -- now I know the root Node is 763
    view g $ pre g 763 -- lets see every Rel involving the root

  -- important nodes
    -- root: 33
    -- git/agent: 763
    -- .mm rels: 31
      -- 16:23 [31: .mm rels] instance/ [:30 _ .mm/ _]
      -- 17:23 [31: .mm rels] instance/ [:29 _ .mm~ _]
    -- system: 32
    -- rels: 24
      -- problem ? does not include 29 (.mm/) and 30 (.mm~).

    matchRel g [Just 22,Nothing,Nothing] -- number of Exprs with nonstandard font

  -- save
  writeFile "data/agent.dwt" $ graphToText g
