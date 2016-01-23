    type Mindmap = Gr Expr DwtEdge
    data Expr = Str String | Tplt [String] | Rel | Coll String
              | RelSpecExpr RelVarSpec

    data DwtEdge = RoleEdge Role | CollMbr
    data Role = RelTplt | Mbr RelPos
      deriving (Show,Read,Eq,Ord)

    data MbrVar = It | Any | Ana | Kata -- TODO: can oft (always?) omit the Any
    data MbrSpec = VarSpec MbrVar | MbrSpec Node

    type RelVarSpec = Map.Map Role MbrVar -- STILL TODO: RelRole, MbrVar
                                                  -- using Role, MbrVar instead
      -- specifies a subset of what a RelSpec does
      -- the other information is carried external to it, in the graph
    type RelSpec = Map.Map Role MbrSpec
      -- if well-formed, has a Tplt, and has RelPoss from 1 to the Tplt's Arity
      -- (but anything mapping to Any can be dropped)
