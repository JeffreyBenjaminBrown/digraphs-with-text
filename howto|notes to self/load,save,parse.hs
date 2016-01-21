-- load
  gf <- readFile "data/agent.dwt"
  let g = read gf :: Mindmap
  let g' = read gf :: Mindmap'

-- save
  writeFile "data/agent.dwt" $ graphToText g

-- parse
  -- start with a .mm file with no html text
    mls <- mmToMlTags "data/agent.mm"
    let mls2 = collapseRich $ stripRichTags $ fromRight mls
    let spec = fromRight $ dwtSpec mls2

  -- with redundant Arities
    let fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
    let fWithNodes = fromRight $ loadNodes (spec, fromRight fr)
    let g = compressGraph $ fromRight $ loadEdges spec fWithNodes :: Mindmap

  -- with redundant Arities
    let fr' = frame' $ frameOrphanStyles' spec :: Either String DwtFrame'
    let fWithNodes' = fromRight $ loadNodes' (spec, fromRight fr')
    let g' = compressGraph $ fromRight $ loadEdges' spec fWithNodes' :: Mindmap'
