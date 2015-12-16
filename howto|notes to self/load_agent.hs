:l test/TGraph.hs 

-- load
x <- readFile "data/agent.dwt" -- dwt = txt
let g = read x :: Mindmap

-- save
writeFile "data/agent.dwt" $ show g

-- If I ever want to prune for speed, I can prune every .mm/ and .mm~ rels which is not the only arity-2 rel involving its endpoints.
