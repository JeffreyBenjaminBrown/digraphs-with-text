
-- parse
  -- run "stack ghci" and load the test suite
    :l test/TGraph.hs 

  -- start with a .mm file with no html text
    mls <- mmToMlTags "untracked/data/agent.mm"
    let mls2 = collapseRich $ stripRichTags $ fromRight mls

  -- this slightly-longer-than-necessary process turns it into a Mindmap called g
    let spec = fromRight $ dwtSpec mls2
    let fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
    let fWithNodes = fromRight $ loadNodes (spec, fromRight fr)
    let g = compressGraph $ fromRight $ loadEdges spec fWithNodes
