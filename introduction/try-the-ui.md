# The UI in a nutshell

Press `M-a` to add Hash expressions, `M-v` to create a view based on a Hash expression. The `M` modifier is probably the Alt key. If it doesn't work, you can simulate pressing `M` by switching to the other window, using `Tab`.


# Getting to the UI

First install [Stack](https://docs.haskellstack.org/en/stable/README/).

Clone the repo. Within that folder, run `stack ghci`. Now you're in GHCI, the REPL for Haskell.

Run `ui empty`. Now you're in the ui, starting with an empty graph.


# Adding data to the graph

In the data entry (upper) window, try typing:

    I #like turtles ##because turtles #have shells
    turtles #are safe ##because turtles #have shells
    I #like pizza ##because of course

Enter that data by pressing `M-a`. The `a` stands for "add". The `M` is a modifier key; on most systems, it stands for "Alt". If it doesn't work, you can simulate pressing Alt by switching to the other window, using the Tab key.

In the data above, each relationship preceded by a single `#` mark is a member of a ##because relationship. The number of hash marks indicates the precedence of the relationships; any number is valid.

For more detail about representing knowledge using a RSLT, see [The Hash Language](/Hash/the-hash-language.md).


# Querying the graph

Type `turtle` and press `M-v` (the `v` stands for "view"). Below should appear every expression that matches the word "turtle". (There's only one.)

Type `/all` and press `M-v`. Now you're back to seeing everything.

A query does not need to specify everything. For instance, try querying for `/it #have shells`. Below should appear the expression `turtles`. You can query for nested relationships, too -- for instance, `/any ##because turtles #have shells`.

For more details on querying a RSLT using Hash, see [The Hash Language](/Hash/the-hash-language.md).
