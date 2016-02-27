-- cycling
  let filename =  "/home/jeff/dwt_git_hask/data/agent.dwt"
  let filename =  "/home/jeff/work/mm/git/share.dwt"
  let p = pure
  let fr = fromRight
  type EM = Either String Mindmap
  let show' g = putStrLn $ graphToText g

  gf <- readFile filename
  let g = read gf :: Mindmap

--
  g <- p $ insStr _ g
  g <- p $ fr $ (insRel 2 [4,3] g :: EM)
  putStrLn $ graphToText g

  writeFile filename $ graphToText g

-- the above is just what I currently need from the below

-- load
  gf <- readFile "data/agent.dwt"
  let g = read gf :: Mindmap

-- save
  writeFile filename $ graphToText g

-- parse
  -- start with a .mm file with no html text
    mls <- mmToMlTags "data/agent.mm"
    let mls2 = collapseRich $ stripRichTags $ fromRight mls
    let spec = fromRight $ dwtSpec mls2

  -- with redundant Arities
    let fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
    let fWithNodes = fromRight $ loadNodes (spec, fromRight fr)
    let g = compressGraph $ fromRight $ loadEdges spec fWithNodes :: Mindmap
