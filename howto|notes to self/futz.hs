Table of Contents (eventually this format is Haskell)
    view
    dfs
    relVarSpec
    Find all nodes using a Tplt
    Find all gold nodes:
    matchRelUsf

-- 2016 02 13: bfs, dfs
    *Main> view g $ 8:[45..(length $ nodes g)-1] -- show relevant portion of graph
    :8 _ isa\ _ -- node 8 is the template for "x isa\ y" relationships
      -- you can tell it's a template and not a string 
      -- because a string would start "8:" instead of ":8"
    45:8 «Down» isa\ «Up» -- node 45 describes node 8
      -- it says that if [x isa\ y], one can usefully think of
      -- x as lying in the Down direction, and y as Up
    46: animal -- just a string
    47: mammal
    48: fish
    49:8 «47: mammal» isa\ «46: animal» -- a relationship
    50:8 «48: fish» isa\ «46: animal»
    51: dolphin
    52:8 «51: dolphin» isa\ «47: mammal»
    53:8 «51: dolphin» isa\ «46: animal»
    :54 _ implies/ _ -- the template for the (top) relationship at node 55
    55:54 «52:8 «51: dolphin» isa\ «47: mammal»» implies/ «53:8 «51: dolphin» isa\ «46: animal»» -- a meta-statement
    *Main> dwtDfs g (fr $ relSpec g 45) [46]
    Right [46,47,51,48]
    *Main> dwtBfs g (fr $ relSpec g 45) [46]
    Right [46,47,48,51]
    *Main> 

-- 2016 02 12
  view g $ concatMap (pre g) (rels g) -- every use of every rel
  let rs = M.fromList [(RelTplt,NodeSpec 8), (Mbr 1,VarSpec Down), (Mbr 2,VarSpec Up)] :: RelSpec
  let rvs = M.fromList [(Mbr 1, Down), (Mbr 2, Up)] :: RelVarSpec

-- 2016 01 16
  Find all nodes using a Tplt
    > labfilter (== mkTplt "_ uses font-> _") g
      mkGraph [(24,Tplt 2 [""," uses font-> ",""])] []
    > view g $ pre g 24
      [The links here to Default Style could be deleted.]

  Find all gold nodes:
    In Freeplane, I see the node with this text
        = already big enough to sort within
    is gold.
    > labfilter (== Str "= already big enough to sort within") g
      1182
    > view g $ pre g 1182
      1526:24 [1182: = already big enough to sort within] uses font-> [7: AutomaticLayout.level,2]
        ..
    > let gold = 7 :: Dwt.Node
    > view g $ pre g gold

-- earlier
  -- some important nodes
    view g $ pre g 771 -- lets see every Rel involving the root
    view g $ nodes $ labfilter (== mkTplt "_ .mm/ _") g -- Node 32
    view g $ nodes $ labfilter (== mkTplt "_ then read-> _") g -- Node 23

  -- how I found that (Nodes obsolete, method still valid)
    length $ nodes g
    matchRelUsf g [Just 32,Nothing,Just 400]
    showExpr g 1583
    matchRelUsf g [Just 32,Nothing,Just 584]
    showExpr g 1600
    matchRelUsf g [Just 32,Nothing,Just 274]
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

    matchRelUsf g [j 22,n,n] -- Exprs with nonstandard font
