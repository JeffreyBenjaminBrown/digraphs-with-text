-- This is Haskell code, designed to be evaluated in a Haskell interpreter such as GHCI.

-- Data Set 1:
--     Mildred is a person.
--     Mildred has stable angina.
--     Stable angina is a coronary artery disease.
--     Every person with coronary artery disease needs mustard.

-- create new RSLT
let g = empty :: RSLT

-- add relationship Templates. "every _" is a unary one; the others are all binary.
g <- pure $ foldl (flip insTplt) g ["every _","_ is a _", "_ has _","_ with _", "_ needs _"]

-- show every expression in g, which is no longer empty
putStr $ view g $ nodes g
-- Each expression appears alongside its address. For instance, the unary "every _" Template resides at address 0, and the binary "_ needs _" Template is at address 4.

-- add the Words we will use. (for these purposes "stable angina" and "coronary artery disease" are one word. We could subdivide them if we needed to.)
g <- pure $ foldl (flip insWord) g ["person","Mildred","stable angina","coronary artery disease","mustard"]

-- now the RSLT has some Words after the Templates
putStr $ view g $ nodes g

-- Now that we have both Templates and Words, we can start adding Relationships. First let's encode that Mildred is a person and stable angina is a coronary artery disease:
g <- pure $ fromRight $ insRel 1 [6,5] g
g <- pure $ fromRight $ insRel 1 [7,8] g
-- The first of those two commands says "insert into g a Relationship that uses the Template at address 1 ("_ is a _"), where the Members are the expressions at address 6 ("Mildred") and 5 ("person")". The second command is similar.

-- Rather than showing all of g, let's just look at the Relationships in it:
putStr $ view g $ nodes $ labfilter isRel g
-- In expressions 10 and 11, the ## symbol identifies the Relationship label "is a". The two relationship Members appear to either side of the label.

-- Last, let us encode the idea that every person with coronary artery disease needs mustard:
g <- pure $ fromRight $ insRel 3 [5,8] g
      -- creates node 12: "person with coronary artery disease"
g <- pure $ fromRight $ insRel 0 [12] g
      -- creates node 13: "every person with coronary artery disease"
g <- pure $ fromRight $ insRel 4 [13,9] g
      -- creates node 14: "every person with coronary artery disease needs mustard"

-- Here is the final RSLT:
putStr $ view g $ nodes g
