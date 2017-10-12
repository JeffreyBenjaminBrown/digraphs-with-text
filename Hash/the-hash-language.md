# The Hash language

Hash is a language for reading and writing a RSLT. A RSLT is [like a graph, but more expressive](/introduction/the_rslt,_why_and_how/it.pdf). Surprisingly, the greater expressivity of the RSLT allows Hash to be easier than other graph-writing languages (such as RDF), and easier than other graph-reading languages (such as Sparql).

# Writing to a RSLT with Hash

You already know about words and parentheses. The only remaining thing to learn about is the # symbol.

## Using # to create simple and compound relationships

### A simple (level-1) binary relationship
`bob #needs legal counsel` creates a binary `needs`-relationship between `bob` and `legal counsel`. `bob` and `legal counsel` could be called level-0 expressions. The `#` indicates that `needs` lies one level above them, forming a relationship that connects them. If `bob` and `legal counsel` were not in the RSLT before, they are now; if they were, they are not duplicated. The same goes for the "relationship template" `_ needs _`.

`bob` and `legal counsel` can be thought of as level-0 expressions. They contain no subexpressions. `bob #needs legal counsel` is a level-1 expression. Loosely, `#` means "make it higher" or "make it connect the others".

### A simple (level-1) ternary relationship
`bob #needs bodyguards #when traveling` represents a ternary `needs-when`-relationship, between `bob`, `bodyguards` and `traveling`.

### A compound (level-2) relationship
In a level-2 relationship, at least one member is itself a level-1 relationship. An example is `bob #goes skiing ##in january`. That creates a `during`-relationship between the level-1 relationship `bob #goes skiing` and the level-0 expression `january`.

### A compound unary relationship
A `relationship` does not have to have multiple members. `maybe` and `not` are two important unary relationships. `##maybe I #need help` creates a `maybe`-relationship with only one member, `I #need help`.

### Any number of # marks is valid
For instance, `reasonable people #like watermelon ##when the weather #is hot ###because watermelon #is cold`.

## Parentheses help, too
Parentheses offer another way to delineate subexpressions. # can be applied to a parenthetical expression just like it was a single word -- as in, for instance, `gold #(produced by) a goose`.

# Querying a RSLT with Hash

Querying is a little more complex than writing, because we query for multiple expressions at once. We still use the same language, Hash, but we introduce a few special symbols. Each of them is preceded by the `/` symbol.

## Basic queries
#### Query for one thing by writing it
A query does not have to involve any special symbols. The query `hummingbirds #are amazing` will return the expression `hummingbirds #are amazing` if it is in the graph. If it is not, it will return nothing.

### Query for everything using /all
The simplest query for multiple things is `/all`. That will return every node in the graph.

### Query with wildcards using /any
Suppose the RSLT contained
```
I #must own a suit #by Friday
I #must locate Chicago #by Friday
I #must attend Eric and Stacy's wedding #on Saturday
```

`I #must /any #by Friday` would then return the first two expressions. `/any` is a "wildcard": it matches any expression.

### Query for subexpressions using /it
Using the same data, if we instead queried for `I #must /it #by Friday`, we would get `locate Chicago` and `own a suit`. The `/it` symbol causes the query to return the subexpression that appears in its place, rather than the entire expression.

### Boolean operations: & and |
`(I #like /it) & (you #like /it)` will return everything that you and I both like. `(I #like /it) | (you #like /it)` will return everything that either of us like. (`&` is called the "intersection" operator,  and `|` the "union" operator.)

## Advanced topics
### Early evaluation for sub-queries: Use /eval

Consider a RSLT with the following data:
```
Bran #likes the dog
Bran #likes the cat
the cat #is psychopathic
```
We might try the query `Bran #likes (/it #is psychopathic)`, as a way to find all the psychopathic things that Bran likes. That query would not work, however, because it specifies a search for a #likes relationship in which the second member is an #is relationship. We want the expression `/it #is psychopathic` to be evaluated in its entirety, not as a subexpression.

To do that, use the `/eval` keyword. `Bran #likes (/eval /it #is psychopathic)` will return what we want.

### Search recursively using /b (that stands for "branch"), /from and /to

Suppose we had this data:
```
the spaceship #needs the fuel pod
the spaceship #needs GPS connectivity
the fuel pod #needs petroleum
```

We might want to find everything the spaceship needs. Needs is a transitive relationship: if a needs b and b needs c, then a needs c. Thus the query `the spaceship #needs /it` is insufficient for these purposes. We need to specify a recursive search, starting from `the spaceship`, using the `_ needs _` relationship. Here's how:

`/b (/from #needs /to) the spaceship`

The keywords `/from` and `/to` indicate the direction to travel along the `needs` relationship. That relationship connects a thing that needs to a thing that is needed; it can be traversed in two directions, and they mean very different things. For recursive search, we must specify that direction.

Note that the keywords `/from` and `/to` can be used to specify searches involving relationships with more than two members.

### Relationship templates can also be in relationships
As described earlier, when you add `a #needs b` to the graph, the effect is to add (if they don't already exist) `a`, `b`, the relationship `a #needs b`, and the relationship template `_ needs _`. That template can itself be in a relationship. (Note that when referring to a template, we don't use the `#` symbol. We only use that when *using* the relationship to connect things.)

As an example, adding `(_ needs _) #(is equivalent to) (_ requires _)` would create (if they don't already exist) the "needs" template, the "requires" template, and the "is equivalent to" template, and would use the "is equivalent to" template to relate the other two.
