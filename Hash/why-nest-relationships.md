Why nest relationships?
---
Hash is agnostic about whether and where you should nest relationships; it only makes clear the tradeoff. If you want a high number of flat, high-arity relationships, you can do that. If you want to nest relationships, you can use fewer.

Consider the statement "Molly needs a bath before dinner'. 

We could encode it as a single ternary "needds-before" relationship: "Molly #needs a bath #before dinner". Taking that strategy, we could get a large number of such ternary relationships: "eats before", "creates before", "expresses during", "argues because", etc.

We could instead encode it as a compound relationship, "Molly #needs a bath ##before dinner". In this case we'll need fewer kinds of relationships. As long as we distinguish complete expressions (which can be interpreted as something a user said) to incomplete ones (which make no sense out of context), that saving comes at no information cost; nothing is lost.

My theory is that in addition to saving space, the compound relationship makes some queries easier. It becomes easy, for instance, to query the graph, "under what conditions does Molly need a bath?" The graph just needs to know which relationships are conditions.
