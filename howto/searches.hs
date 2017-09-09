Table of Contents (eventually this format is Haskell)
    pre: Find all nodes using a leaf (even a Tplt)
    matchRelDe: find all relationships using some Tplt with certain members
    ?? adding a directed RelSpec to a graph
    ?? dfs, bfs
    RelSpec v. RelVarSpec
    Find all gold nodes (from Freeplane)

-- Find all nodes using a leaf, even a Tplt
    > labfilter (== mkTplt "_ uses font-> _") g
      mkGraph [(24,Tplt 2 [""," uses font-> ",""])] []
    > putStr $ view g $ pre g 24
      [The links here to Default Style could be deleted.]
    > putStrLn $ view g [7,8]
    (7,"_ #can _")
    (8,"turtles ##can swim")

--  matchRelDe: find all relationships using some Tplt with certain members
    > let s = M.fromList [(TpltRole,NodeSpec 7),(Mbr 1,VarSpec Any),(Mbr 2,VarSpec Any)]
    >  matchRelDe g s
    Right [8]

-- adding a directed RelSpec to a graph
    I saw this template:
      ":15 _ #kind/ _"
    So I made a direction:
      *Main M> let r = M.fromList [(TpltRole,NodeSpec 15),
        (Mbr 1,VarSpec Up),
        (Mbr 2,VarSpec Down)]
    And added it:
      g <- p $ fr $ (insRelSpec r g :: Either String RSLT)

-- 2016 02 13: bfs, dfs
    *Main> putStr $ view g $ 8:[45..(length $ nodes g)-1] -- show relevant portion of graph
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

-- RelSpec v. RelVarSpec
  putStr $ view g $ concatMap (pre g) (rels g) -- every use of every rel
  let rs = M.fromList [(TpltRole,NodeSpec 8), (Mbr 1,VarSpec Down), (Mbr 2,VarSpec Up)] :: RelSpec
  let rvs = M.fromList [(Mbr 1, Down), (Mbr 2, Up)] :: RelVarSpec

-- Find all gold nodes (from Freeplane)
    In Freeplane, I see the node with this text
        = already big enough to sort within
    is gold.
    > labfilter (== Word "= already big enough to sort within") g
      1182
    > putStr $ view g $ pre g 1182
      1526:24 [1182: = already big enough to sort within] uses font-> [7: AutomaticLayout.level,2]
        ..
    > let gold = 7 :: Dwt.Node
    > putStr $ view g $ pre g gold
