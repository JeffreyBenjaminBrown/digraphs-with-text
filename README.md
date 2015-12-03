# What is Dwt?

Dwt is an editor for hypergraphs with text on them. Everything I am aware of can be represented as a hypergraph -- tables, lists|arrays|tuples, graphs, dictionaries, bitmapped imagines ...

In Dwt an expression (Expr) can be a string of text (Str), a template for a k-ary relationship (Tplt k), or a k-ary relationship (Rel k). The elements of Rels are other Exprs.

The file data/minimalGraph.hs, 10 lines long, contains an example graph. It demonstrates both the meaning of those three Expr constructors and how Dwt uses FGL, the Functional Graph Library, to implement hypergraphs. (Thanks to Elliot Cameron for that and many other suggestions.)

# Glossary of Abbreviations

*  ch = change
*  ins = insert
*  mbr = member
  In a a k-ary Rel, there are k AsPos Roles for k member Exprs, 
  plus one more AsTplt Role for the Tplt (which must be k-ary).
*  pos = position
*  rel = relationship
*  sub = substitute
*  tplt = (relationship) template
*  usf = unsafe
