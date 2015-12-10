mls <- tMmFile "data/agent.mm"
let spec = fromRight $ dwtSpec $ fromRight mls
let fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
let fWithNodes = fromRight $ loadNodes (spec, fromRight fr)
let g = compressGraph $ fromRight $ loadEdges spec fWithNodes
