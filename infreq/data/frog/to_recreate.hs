SENSITIVE to order
  Node-specifying functions here can be broken by reordering the earlier stmts
  use Insertion to fix that

let g = empty :: Mindmap -- g is now an empty mindmap.
  -- I use the letter g because Mindmaps are Graphs,
  -- a class provided by the Functional Graph Library.
g <- pure $ insWord "frog" g -- This inserts a string into g.
  -- "g <- pure $" because we use the IO monad to redefine g.
-- Now g has one thing, the word frog:
v g $ nodes g -- This views every expression in g.
  -- The 0 before the comma is a count of how many expressions refer to that expression. None do, because none yet exist.
  -- The second 0 is a Node, which one should think of as the address of the string "frog".
-- Let's add more words:
g <- pure $ insWord "moist" g
g <- pure $ insWord "springy" g
-- Now g has three words:
v g $ nodes g
-- Let's indicate that moist and springy are both qualities of frogs:
-- First, we'll create a template for relationships about qualities.
g <- pure $ insTplt "_ is a quality of/ _" g
v g $ nodes g
  -- The template has been inserted at node 3. Its address is preceded by a : to indicate that it is a template, as opposed to the string "_ is a quality of _". (That will seem more natural in the context of how relationships are displayed.)
-- Now we'll use the template at 3 to insert two relationships among the other nodes:
-- We could have added them in a single stage:
g <- pure $ fromRight $ insRelStrErr 3 [2,0] g >>= insRelStrErr 3 [1,0]
  -- insRelStrErr 3 [1,0] inserts "moist is a quality of frog".
  -- insRelStrErr 3 [2,0] inserts "springy is a quality of frog".
  -- Both relationships use the template at node 3, and in both the second member is frog, at node 0.
  -- Since the template at 3 relates 2 expressions, each list has length two. Templates can have any arity -- for instance, "_ does _ to _" has arity 3.
  -- I used the monadic bind operator (>>=) because that way I only need to escape one Either monad (using fromRight) rather than two of them, one containing the other.
-- Inserting those two relationships worked:
v g $ nodes g
  -- When showing a relationship, the node (integer) listed first, before the :, is it's address, and the node listed after the : is the address of its template.
  -- After the first two, any other node in a relationship is the address of the symbol(s) it appears just left of.
  -- Notice that now the frog is in two relationships. One of those relationships involves springy, the other involves moist.
  -- The # symbols are better explained later.
-- DWT can encode meta-statements, statements about other statements. As an example, next let's encode that maybe frogs are springy because they use rubber bands. 
-- First we should insert a new string and a couple new templates for relationships.
g <- pure $ insTplt "maybe _ because _" g
g <- pure $ insWord "rubber bands" g
g <- pure $ insTplt "_ uses/ _" g
v g $ nodes g
-- Now we can encode the subexpression that frog uses rubber bands.
g <- pure $ fr $ insRelStrErr 8 [0,7] g
v g $ nodes g
-- From those elements, we can encode that maybe frogs are springy because they use rubber bands.
g <- pure $ fr $ insRelStrErr 6 [4,9] g
v g $ nodes g
  -- At last I can explain the # symbols: They indicate how fast an expression binds. In the last, at node 10, the highest-level|slowest-binding relationship is "maybe _ because _". In the first _ of the maybe-because relationship is another relationship, one that binds faster and hence has fewer # symbols, between springy and frog. The strings "springy", "frog" and "rubber bands" can be thought of as atomic, "binding" even before the first-level relationships bind.
  -- This illustrates one of DWT's advantages over graphs. In a graph, an edge cannot be a member of another edge. By contrast, in DWT a relationship can involve other relationships.
