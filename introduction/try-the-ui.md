# Start it

First install [Stack](https://docs.haskellstack.org/en/stable/README/).

Clone the repo. Within that folder, run `stack ghci`. Now you're in GHCI, the REPL for Haskell.

Run `ui empty`. Now you're in the ui, starting with an empty graph.


# Add data to the graph

In the data entry (upper) window, try typing:

    I #like turtles ##because turtles #have shells
    turtles #are safe ##because turtles #have shells
    I #like pizza ##because of course

Enter that data by pressing M-Enter (Alt-Enter). Now those statements, and every object and relationship they involve, are in the graph.  (The number of hash marks indicates the precedence of the relationships. For more detail, see [The Hash Language](/Hash/the-hash-language.md).)

Press Tab to go to the query (lower) window. Type `/a` and press M-Enter. Now you see all (/a stands for "all") things in the graph.


# Query the graph

## Simple queries

Type `turtle` into the query window and press M-Enter. Below should appear every expression that matches the word "turtle". (There's only one.)

Queries can be about relationships, and can include variables. For instance, type `/any #have shells` and press M-Enter. Below should appear the expression "turtles #have shells".

You can query for nested relationships, too -- for instance, `/any ##because turtles #have shells`.


## Recursive queries -- If plants need dirt and dirt needs worms, then plants need worms

Try adding (in the data-entry window) a few more statements:

  turtles #are lizards
  lizards #are animals
  mammals #are animals
  squirrels #are mammals

Now ask (in the query window) for `/b (/from #are /to) turtles`. The UI should return "lizards" and "animals". (The /b stands for "branch"; it indicates recursive search, in the direction specified by "from" and "to".)


## Boolean operators on queries

Try querying for `(/b (/from #are /to) turtles) & (/b (/from #are /to) squirrels)`. The result should be every category that includes both turtles and squirrels.

`a & b` produces the intersection of the queries `a` and `b`. Similarly, `a | b` produces their union.
