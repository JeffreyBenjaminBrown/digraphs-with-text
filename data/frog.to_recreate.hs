SENSITIVE to order
  Node-specifying functions here can be broken by reordering the earlier stmts
  use QNode to fix that

let g = empty :: Mindmap -- g is now an empty mindmap.
g <- pure $ insStr "frog" g -- This inserts a string into g.
  -- "g <- pure $" because we use the IO monad to redefine g.
-- Now g has one thing, the word frog:
v g $ nodes g -- This views every expression in g.
  -- The 0 before the comma is a count of how many expressions refer to that expression. None do, because none yet exist.
  -- The second 0 is a Node, which one should think of as the address of the string "frog".
-- Let's add more words:
g <- pure $ insStr "moist" g
g <- pure $ insStr "springy" g
-- Now g has three words:
v g $ nodes g
-- Let's indicate that moist and springy are both qualities of frogs:
-- First, we'll create a template for relationships about qualities.
g <- pure $ insTplt "_ is a quality of/ _" g
v g $ nodes g
  -- The template is at node 3. Its address is preceded by a : to indicate that it is a template, as opposed to the string "_ is a quality of _". (That will seem more natural in the context of how relationships are displayed.)
-- Now we'll use the template at 3 to insert two relationships among the other nodes:
g <- pure $ fromRight $ insRel 3 [2,0] g >>= insRel 3 [1,0]
  -- Both relationships use the template at node 3.
  -- In both the second member is frog, at node 0.
  -- The first members in each relationship are the two qualities that frogs have, springiness (at 2) and moistness (at 1).
  -- Since the template at 3 relates 2 expressions, each list has length two. Templates can have any arity, however -- for instance, "_ does _ to _" has arity 3.
  -- >>= because that way I only need to escape one Either monad (using fromRight) rather than two of them nested.
-- It worked:
v g $ nodes g
  -- In a relationship, the node (integer) listed first, before the :, is its address.
  -- The node listed after the : is the address of its template.

-- Let's encode that maybe frogs are springy because they use rubber bands.
-- First we need some "leaves", objects ni the graph that refer to nothing else in the graph.
g <- pure $ insTplt "maybe _ because _" g
g <- pure $ insStr "rubber bands" g
g <- pure $ insTplt "_ uses/ _" g
-- Now we can encode that frog uses rubber bands.
g <- pure $ fr $ insRel 8 [0,7] g
-- From those elements, we can encode that maybe frogs are springy because they use rubber bands.
g <- pure $ fr $ insRel 6 [4,9] g
v g $ nodes g
