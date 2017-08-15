-- -- the commands alone
let g = empty :: RSLT

g <- pure $ foldl (flip insWord) g ["the cat","water","living organism"]
view g $ nodes g
g <- pure $ foldl (flip insTplt) g ["_ needs _", "_ is a _","_ because _"]
view g $ nodes g
g <- pure $ fromRight $ insRel 3 [0,1] g
g <- pure $ fromRight $ insRel 4 [0,2] g
view g $ nodes g
g <- pure $ fromRight $ insRel 5 [6,7] g
view g $ nodes g

 
-- -- with output and comments
-- make an empty graph
> let g = empty :: RSLT

-- add three words to it
> g <- pure $ foldl (flip insWord) g ["the cat","water","living organism"]

-- add three relationship templates.
  -- Templates resemble edge labels, but with arbitrary arity and nesting.
> g <- pure $ foldl (flip insTplt) g ["_ needs _", "_ is a _","_ because _"]

-- view the graph so far
> view g $ nodes g
(0,"the cat") -- Expressions 0, 1 and 2 are "Words"
(1,"water")
(2,"living organism")
(3,"_ #needs _") -- Expressions 3, 4 and 5 are "Tplts" (relationship templates)
(4,"_ #(is a) _")
(5,"_ #because _")

-- use the relationship templates to create two relationships
> g <- pure $ fromRight $ insRel 3 [0,1] g
> g <- pure $ fromRight $ insRel 4 [0,2] g

> view g $ nodes g
(0,"the cat")
(1,"water")
(2,"living organism")
(3,"_ #needs _")
(4,"_ #(is a) _")
(5,"_ #because _")
(6,"the cat ##needs water") -- nodes 6 and 7 are the new relationships
(7,"the cat ##(is a) living organism")

-- nesting! create a "because" relationship between relationships 6 and 7
> g <- pure $ fromRight $ insRel 5 [6,7] g

> view g $ nodes g
(0,"the cat")
(1,"water")
(2,"living organism")
(3,"_ #needs _")
(4,"_ #(is a) _")
(5,"_ #because _")
(6,"the cat ##needs water")
(7,"the cat ##(is a) living organism")
(8,"the cat ##needs water ####because the cat ##(is a) living organism")
> 
