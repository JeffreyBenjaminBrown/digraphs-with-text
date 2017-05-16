# The Hash language
Hash is a DSL for encoding natural language in a graph. It uses ordinary natural language (Swahili, Chinese, anything) words, and only one operator, the `#` symbol. Loosely, `#` means "make it higher" or "make it connect the others".

# Build relationships with #

## A simple (level-1) binary relationship
`bob #needs money` represents a binary `needs` relationship between `bob` and `money`. `bob` and `money` are level-0 expressions. The `#` indicates that `needs` lies in a level above them, forming a relationship that connects them.

## A simple (level-1) ternary relationship
`bob #needs money #when traveling` represents a ternary `needs-when` relationship, between bob, money and traveling.

## A compound (level-2) relationship
In a compound relationship, at least one member is itself a relationship.

For example, `bob #likes traveling ##during summer` creates a `when` relationship, which connects the level-1 relationship `bob likes traveling` to the level-0 expression `summer`.

## A compound unary relationship
`##maybe I #need help` creates a level-2 `maybe` relationship with only one member, `I #need help`. `maybe` and `not` are two important unary relationships

# An unmodified series of words
`sharks #have skin made of teeth` creates a simple relationship between `sharks` and `skin made of teeth`. A series of words (with no # symbols) is treated just like a single word.

This raises another interpretation of the # symbol: # means "Computer, understand this". For instance, the user could have written `skin #made-of teeth`, in which case the computer would record a relationship between skin and teeth.

# Parentheses help too
Parentheses offer another way to delineate subexpressions. # can be applied to a parenthetical expression just like it was a single word -- as in, for instance, `skin #(made out of) teeth`.
