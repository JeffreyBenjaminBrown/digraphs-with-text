# Table of contents
* Starting the UI
* Using the UI
  * In a nutshell
  * Adding data to the graph
  * Querying the graph
* Hacking it: Capturing native Haskell data from the UI
* References

# Starting the UI

Install [Stack](https://docs.haskellstack.org/en/stable/README/).

Clone this repo: `git clone https://github.com/JeffreyBenjaminBrown/digraphs-with-text/`. Doing that creates a folder called digraphs-with-text. Go there.

Run `stack ghci`. Now you're in GHCI, the REPL for Haskell.

Run `ui empty`. Now you're in the ui, starting with an empty graph.


# Using the UI

## In a nutshell

The UI lets you add to and query a Reflective Set of Labeled Tuples ("RSLT"), a data structure that generalizes graphs, using [the Hash language](/Hash/the-hash-language.md).

Type Hash expressions into the top window. Press `M-a` to add Hash a expression, or `M-v` to create a view based on a Hash expression. The `M` modifier is probably the Alt key. If it doesn't work, you can simulate pressing `M` by switching to the other window, using `Tab`. Press `Esc` to exit.


## Adding data to the graph

In the data entry (upper) window, try typing:

    I #like turtles ##because turtles #have shells
    turtles #are safe ##because turtles #have shells
    I #like pizza ##because of course

Enter that data by pressing `M-a`. The `a` stands for "add".

The `M` is a modifier key; on most systems, it stands for "Alt". If it doesn't work, you can simulate pressing Alt by switching to the other window, using the Tab key.

In the data above, each relationship preceded by a single `#` mark is a member of a ##because relationship. The number of hash marks indicates the precedence of the relationships; any number of them is valid.

For more detail about representing knowledge using a RSLT, see [The Hash Language](/Hash/the-hash-language.md).


## Querying the graph

Type `turtle` and press `M-v` (the `v` stands for "view"). Below should appear every expression that matches the word "turtles". (There can be only one -- distinct expressions cannot be textually identical.)

Type `/all` and press `M-v`. Now you're back to seeing everything.

A query does not need to specify everything. For instance, try querying for `I #like /it`. Below should appear the expressions `turtles` and `pizza`. You can query for nested relationships, too -- for instance, `/any ##because turtles #have shells`.

For more details on querying a RSLT using Hash, see [The Hash Language](/Hash/the-hash-language.md).


# Hacking it: Capturing native Haskell data from the UI

In GHCI, outside of the UI, we can ask what the type of the UI is:
```
>>> :t ui
ui :: RSLT -> IO RSLT
```
It takes a RSLT, does some IO, and returns a RSLT. Therefore, instead of `ui empty`, we can call the UI with `g <- ui empty`. That will run the UI as normal, but once it finishes, the graph it created can be referred to by the name `g`.

To start te UI with a preexisting graph `g`, can call it with `ui g`.

The UI includes more data than the graph -- there is a command history, the state of the windows involved, and more. If you would like to capture all of that data, there's a function for that, too:
```
>>> :t uist
uist :: St -> IO St
```
(`St` stands for "application state".) To start `uist` using graph `g` and save the result as `st`, call `st <- uist $ initialState g`. (Substitute `empty` for `g` to start with a blank graph.)


# Further reading
[This paper](/introduction/the_rslt%2C_why_and_how/it.pdf) describes how a RSLT is implemented.
