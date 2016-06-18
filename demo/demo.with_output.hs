Data Set 1: Mildred is a person.
            Mildred has stable angina.
            Stable angina is a coronary artery disease.
            Every person with coronary artery disease needs mustard.

First, let's create an empty RSLT named "g":

    > let g = empty :: RSLT

Next, let's insert into g the five Templates we will need:

    > g <- pure $ foldl (flip insTplt) g
        ["every _"
	,"_ is a _"
	, "_ has _"
	,"_ with _"
	, "_ needs _"]

Now let's look at g:

    > view g $ nodes g
    (0,"#every _")
    (1,"_ #(is a) _")
    (2,"_ #has _")
    (3,"_ #with _")
    (4,"_ #needs _")

Each expression appears alongside its address. For instance, the unary "every _" Template resides at address 0, and the binary "_ needs _" Template is at address 4.

Next, lets put our first order expressions into the RSLT: the Words "person", "Mildred", "stable angina", "coronary artery disease", and "mustard". (For these purposes "coronary artery disease" and "stable angina" are single Words.)

    > g <- pure $ foldl (flip insStr) g
         ["person",  "Mildred",  "stable angina"
	 ,  "coronary artery disease",  "mustard"]

The new Words are inserted after the Templates:

    > view g $ nodes g
    (0,"#every _")
    (1,"_ #(is a) _")
    (2,"_ #has _")
    (3,"_ #with _")
    (4,"_ #needs _")
    (5,"person")
    (6,"Mildred")
    (7,"stable angina")
    (8,"coronary artery disease")
    (9,"mustard")

Now that we have both Templates and Words, we can start adding Relationships. First let's encode that Mildred is a person and stable angina is a coronary artery disease:

    > g <- pure $ fromRight $ insRel 1 [6,5] g
      -- creates node 10, "Mildred is a person"
    > g <- pure $ fromRight $ insRel 1 [7,8] g
      -- creates node 11, "stable angina is a coronary artery disease"

The first of those two commands says "insert into g a Relationship that uses the Template at address 1 ("_ is a _"), where the Members are the expressions at address 6 ("Mildred") and 5 ("person")". The second command is similar.

Let's view the RSLT again:

    > view g $ nodes g
    (0,"#every _")
    (1,"_ #(is a) _")
    (2,"_ #has _")
    (3,"_ #with _")
    (4,"_ #needs _")
    (5,"person")
    (6,"Mildred")
    (7,"stable angina")
    (8,"coronary artery disease")
    (9,"mustard")
    (10,"Mildred ##(is a) person")
    (11,"stable angina ##(is a) coronary artery disease")

Expressions 10 and 11 are the two new Relationships. In both of them, the ## symbol identifies the Relationship label "is a", on either side of which appear the two Members.

Last, let us encode the idea that every person with coronary artery disease needs mustard:

    > g <- pure $ fromRight $ insRel 3 [5,8] g
      -- creates node 12: "person with coronary artery disease"
    > g <- pure $ fromRight $ insRel 0 [12] g
      -- creates node 13: "every person with coronary artery disease"
    > g <- pure $ fromRight $ insRel 4 [13,9] g
      -- creates node 14: "every person with coronary artery disease needs mustard"

Here is the final RSLT:

    > view g $ nodes g
    (0,"#every _")
    (1,"_ #(is a) _")
    (2,"_ #has _")
    (3,"_ #with _")
    (4,"_ #needs _")
    (5,"person")
    (6,"Mildred")
    (7,"stable angina")
    (8,"coronary artery disease")
    (9,"mustard")
    (10,"Mildred ##(is a) person")
    (11,"stable angina ##(is a) coronary artery disease")
    (12,"person ##with coronary artery disease")
    (13,"####every person ##with coronary artery disease")
    (14,"####every person ##with coronary artery disease ########needs mustard")

Recall that the number of # symbols on a Relationship label indicates the order of that Relationship. Thus in expression 14, ##with binds first, then ####every, and last ########needs. I chose to make the number of # symbols start at 2 and increase exponentially (2, 4, 8, ...) so that visually parsing them would be easy: You can see which label has more # symbols without explicitly counting them.



-- create new RSLT
let g = empty :: RSLT

-- add relationships. "every _" is a unary one; the others are all binary.
g <- pure $ foldl (flip insTplt) g ["every _","_ is a _", "_ has _","_ with _", "_ needs _"]

-- show every expression in g, which is no longer empty
view g $ nodes g

-- add the Words we will use. (for these purposes "stable angina" and "coronary artery disease" are one word. We could subdivide them if we needed to.)
g <- pure $ foldl (flip insStr) g ["person","Mildred","stable angina","coronary artery disease","mustard"]

-- now the RSLT has some Words after the Templates
view g $ nodes g

-- 
g <- pure $ fromRight $ insRel 1 [6,5] g
g <- pure $ fromRight $ insRel 1 [7,8] g
view g $ nodes g
g <- pure $ fromRight $ insRel 3 [5,8] g
g <- pure $ fromRight $ insRel 0 [12] g
g <- pure $ fromRight $ insRel 4 [13,9] g
view g $ nodes g
