
-- parse
  -- run "stack ghci" and load the test suite
    :l test/TGraph.hs 

  -- start with a .mm file with no html text
      -- -- the file must have no hypertext tags
    mls <- mmToMlTags "data/agent.mm"
    let mls2 = stripRichTags $ fromRight mls
    let mls3 = collapseRich $ fromRight mls

  -- this slightly-longer-than-necessary process turns it into a Mindmap called g
    let spec = fromRight $ dwtSpec mls3
    let fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
    let fWithNodes = fromRight $ loadNodes (spec, fromRight fr)
    let g = compressGraph $ fromRight $ loadEdges spec fWithNodes
