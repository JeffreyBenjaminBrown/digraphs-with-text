# The Hash language
Hash is a DSL for encoding natural language in a graph. It uses ordinary English words, and only one operator, the # symbol. Loosely, # means "make it higher".

## A level-1 binary relationship
`bob #needs money` represents a binary `needs` relationship between `bob` and `money`. `bob` and `money` are level-0 expressions.

## A level-1 ternary relationship
`bob #needs money #when traveling` represents a ternary `needs-when` relationship, between bob, money and traveling.

## A level-2 relationship
    -- The # indicates that `needs` lies in a level above the other words,
        -- forming a relationship that connects them.
    -- ## means level 2, ### level 3, etc.
    -- `bob #likes traveling ##during summer` creates
        -- a when-relationship, between
            -- the level-1 relationship `bob likes traveling`, and
            -- the level-0 expression `summer`

## Parentheses are also useful
Parentheses offer another way to delineate subexpressions. # can be applied to a parenthetical expression just like it was a single word.
