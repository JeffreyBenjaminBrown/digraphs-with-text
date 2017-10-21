The UI will let you add to and query a Reflective Set of Labeled Tuples ("RSLT"), a data structure that generalizes graphs, using [the Hash language](/Hash/the-hash-language.md).


# Table of contents
* Starting the UI
* Using the UI
  * Adding data to a RSLT
  * Querying a RSLT
* Importing and exporting data
  * Starting with a preexisting RSLT, and preserving the resulting RSLT
  * Saving to disk, and reading from disk
  * Capturing the full application state
* Further reading


# Starting the UI

Install [Stack](https://docs.haskellstack.org/en/stable/README/).

Clone this repo: `git clone https://github.com/JeffreyBenjaminBrown/digraphs-with-text/`. Doing that creates a folder called digraphs-with-text. Go there.

Run `stack ghci`. Now you're in GHCI, the REPL for Haskell. (You don't have to know Haskell, or any other programming language.)

Run `ui empty`. Now you're in the UI, starting with an empty RSLT.


# Using the UI

## Adding data to a RSLT

In the data entry (upper) window, try typing:
```
    I #like turtles ##because turtles #have shells
    turtles #are safe ##because turtles #have shells
    I #like pizza ##because duh
```

Enter that data by pressing `M-a`. The `a` stands for "add". (The `M` is a modifier key; on most systems, it stands for "Alt". If it doesn't work, you can simulate pressing Alt by switching to the other window, using the Tab key.)

In the turtles and pizza data above, each relationship preceded by a single `#` mark is a member of a ##because relationship. The number of hash marks indicates the precedence of the relationships; any number of them is valid.

For more detail about representing knowledge using a RSLT, see [The Hash Language](/Hash/the-hash-language.md).


## Querying a RSLT

Type `turtle` and press `M-v`. The `v` stands for "view". Below should appear every expression that matches the word "turtles". (There can be only one such match -- distinct expressions cannot be textually identical.)

Type `/all` and press `M-v`. Now you're back to seeing everything.

A query does not need to specify everything. For instance, try querying for `I #like /it`. Below should appear the expressions `turtles` and `pizza`. You can query for nested relationships, too -- for instance, `/any ##because turtles #have shells`.

For more details on querying a RSLT using Hash, see [The Hash Language](/Hash/the-hash-language.md).


# Importing and exporting data
## Starting with a preexisting RSLT, and preserving the resulting RSLT

In GHCI, outside of the UI, we can ask what the type of the UI is:
```
>>> :t ui
ui :: RSLT -> IO RSLT
```
GHCI is telling us that the `ui` function takes a RSLT, does some IO, and returns a RSLT. When we call `ui empty` we start the UI with an empty RSLT. If `g` is some preexisting RSLT, `ui g` begins the UI starting from `g` rather than `empty`.

After exiting the UI, the RSLT prints to screen. Immediately after that, we can assign it to the variable `g` (or any other word starting with a lowercase letter) by executing `g = it`. Alternatively, we could have called the UI with `g <- ui g`. That will run the UI starting from `g`, and then update the value of `g`.


## Saving to disk, and reading from disk

The RSLT has Read and Show instances -- that is, a RSLT can be read from a string and can be encoded ("shown") as a string. Therefore we can save to disk, and read from disk:

```
g <- ui empty               -- the UI runs, and its result is stored in g
gs = show g                 -- gs is a String representation of g
writeFile "data.rslt" gs    -- this saves g in a file called "data.rslt"
gs' <- readFile "data.rslt" -- this reads that file into a new String
g' = read gs' :: RSLT       -- this reads a RSLT from gs', storing it in g'
```


## Capturing the full application state

The UI includes more data than a RSLT -- there is a command history, the state of the windows involved, and more. If you would like to capture all of that data, there's a function for that, too:
```
>>> :t uist
uist :: St -> IO St
```
(`St` stands for "application state".) To start `uist` using RSLT `g` and save the result as `st`, call `st <- uist $ initialState g`. (Substitute `empty` for `g` to start with a blank RSLT.)


# Further reading
[This paper](/introduction/the_rslt%2C_why_and_how/it.pdf) describes how a RSLT is implemented. It's almost embarrassingly simple.
