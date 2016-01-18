-- 2016 01 16
do ? sep punct from RelTplt [String]
  currently : stringToTplt "_ uses font-> _"
    mixes punctuation, a display choice, with description of the relationship

How to find all nodes using a Tplt
    > labfilter (== stringToTplt "_ uses font-> _") g
      mkGraph [(24,Tplt 2 [""," uses font-> ",""])] []
    > view g $ pre g 24
      [The links here to Default Style could be deleted.]

How to find all gold nodes:
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
  
Compress (depth) keeping only gold

-- earlier
  -- some important nodes
    view g $ pre g 771 -- lets see every Rel involving the root
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
