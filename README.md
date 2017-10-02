# In a nutshell

DWT implements the RSLT, an unusually expressive kind of knowledge graph. It permits arbitrary-arity and arbitrarily nested (e.g. edges connecting edges) relationships. The Hash language allows a user to add statements to a knowledge graph without having to learn  to program. Some querying functions let the user traverse the graph, asking questions like, "What are all the things I have to do today?" "What do I care about that Professor X could talk about?" "What tasks does Sharon have to do that depend on tasks I have to do?"


# To try it

Clone the repo:

Run `stack ghci` (you'll need [Stack](https://docs.haskellstack.org/en/stable/README/) installed). Now you're in GHCI, the REPL for Haskell. Run `ui empty`. Now you're in the ui, starting with an empty graph.

In the first (data entry) window, try typing:

    I #like turtles ##because turtles #have shells
    turtles #are safe ##because turtles #have shells
    I #like pizza ##because of course

Enter that data by pressing M-Enter (Alt-Enter). Now those statements, and every object and relationship they involve, are in the graph.xs  (The number of hash marks indicates the precedence of the relationships. To read more about that language for entering data, see [The Hash Language](Hash/the-hash-language.md).)

Press Tab to go to the other window. Type `/a` and press M-Enter. Now you see all (/a stands for "all") things in the graph. Type `/any ##because turtles #have shells` and press M-Enter.

Try adding (in the data-entry window) a few more statements:

  turtles #are lizards
  lizards #are animals
  mammals #are animals
  squirrels #are mammals

(You can copy and paste that whole block, and just run M-Enter once.) Now try asking (in the query window) the following:

  (/b (/from #are /to) turtles) & (/b (/from #are /to) squirrels)

The result is every category that includes both turtles and squirrels. (The /b stands for "branch"; it indicates recursive search, in the direction specified by "from" and "to".) `&` produces the intersection; there is a similar `|` operator for producing the union.


# How it works

This [short paper](/introduction/the_rslt,_why_and_how/it.pdf) describes how the RSLT is implemented and why it is helpful. It requires no background in math or programming.

The [simplified model of the type system](/introduction/Minimal_Types.hs) might be the best place to start exploring the code.


# Related projects

[Semantic Synchrony](https://github.com/synchrony/smsn/wiki).
