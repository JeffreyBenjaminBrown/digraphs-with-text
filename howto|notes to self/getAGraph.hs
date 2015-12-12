-- run "stack ghci" and load the test suite
:l test/TGraph.hs 

-- start with a .mm file with no html text
  -- -- the file must have no hypertext tags
mls <- mmToMlTags "data/agent.mm"

-- this slightly-longer-than-necessary process turns it into a Mindmap called g
let spec = fromRight $ dwtSpec $ fromRight mls
let fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
let fWithNodes = fromRight $ loadNodes (spec, fromRight fr)
let g = compressGraph $ fromRight $ loadEdges spec fWithNodes

-- explore it:
labfilter (== stringToTplt "_ .mm/ _") g
length $ nodes g
matchRel g [Just 30,Nothing,Just 400]
showExpr g 1583
matchRel g [Just 30,Nothing,Just 584]
showExpr g 1600
matchRel g [Just 30,Nothing,Just 274]
showExpr g 1753
-- now I know the root Node is 763
view g $ pre g 763 -- lets see every Rel involving the root

-- important nodes
-- .mm rels: 31
  -- 16:23 [31: .mm rels] instance/ [:30 _ .mm/ _]
  -- 17:23 [31: .mm rels] instance/ [:29 _ .mm~ _]
-- system: 32
-- rels: 24

matchRel g [Just 22,Nothing,Nothing] -- number of nodes with nonstandard font
