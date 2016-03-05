-- SENSITIVE TO ORDER
  -- Node-specifying functions here can be broken by reordering the earlier stmts
  -- use QNode to fix that
let g = empty :: Mindmap
g <- p $ insStr "frog" g
-- Now the graph has a frog in it:
v g $ nodes g
g <- p $ insStr "moist" g
g <- p $ insStr "springy" g
-- Now it has three words:
v g $ nodes g
-- Let's indicate that moist and springy are both qualities of frogs:
g <- p $ insTplt "_ is a quality of/ _" g
g <- p $ fr $ insRel 3 [2,0] g >>= insRel 3 [1,0]
-- It worked:
v g $ nodes g
-- Let's encode that maybe frogs are springy because they use rubber bands.
g <- p $ insTplt "maybe _ because _" g
g <- p $ insStr "rubber bands" g
g <- p $ insTplt "_ uses/ _" g
g <- p $ fr $ insRel 8 [0,7] g
g <- p $ fr $ insRel 6 [4,9] g
