-- Once this file makes sense to you,
  -- the natural place to begin reading the codebase is at src/Dwt/Graph.hs.
-- This file is for illustration purposes only; it is not part of the codebase.

import Data.Graph.Inductive -- import code for working with graphs

-- Define some synonyms for "integer":
type Arity = Int -- Tuples have "arity": pairs have arity 2, triples arity 3, etc.
type MbrPos = Int -- a position in a relationship
  -- The k members of a k-ary Rel take MbrPos values 1 through k.

type RSLT = Gr Expr RSLTEdge -- This says "the RSLT is a Graph in which
  -- each node is an Expr(ession)
  -- and each edge is a RSLTEdge."

-- An Expr can be one of three things:
data Expr = Word String
          | Rel  -- labeled tuple, like "[cats] need [sun]"
          | Tempalte [String] -- template for a labeled tuple, like "_ need _"
  -- A k-ary Template (that is, a Tempalte for a relationship with k members)
    -- must contain k+1 Strings, among which the blanks are interspersed.
    -- For instance, the binary Template "I need _ to _ by tomorrow"
    -- uses the three Strings "I need", "to" and "by tomorrow".
  -- While Word and Template Exprs contain String information, a Rel contains no information at all. A Rel is meaningful only because of the edges it emits.

-- A RSLTEdge can be one of two things:
data RSLTEdge = RelTemplate | Member MbrPos
-- Not every possible edge is valid.
  -- Rule 1: Any Expr can receive an edge (be a member of a superexpression),
    -- but only Rels emit edges (have subexpressions).
  -- Rule 2: Each k-ary Rel emits one RelTemplate edge, toward a k-ary Template.
  -- Rule 3: Each k-ary Rel emits k Member edges, one to each of its k Members. 
    -- Each Member edge is labeled with a MbrPos values from 1 to k,
    -- indicating that Member's position in the Template.
  -- These rules are not reified; respecting them is the coder's responsibility.
