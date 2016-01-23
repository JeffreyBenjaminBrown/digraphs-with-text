    type Mindmap = Gr Expr DwtEdge
    data Expr = Str String | Tplt [String] | Rel | Coll String
              | RelSpecExpr RelVarSpec

    data DwtEdge = RoleEdge Role | CollMbr
    data Role = RelTplt | Mbr RelPos

    data MbrVar = It | Any | Ana | Kata -- TODO: can oft (always?) omit the Any

    data MbrSpec = VarSpec MbrVar | MbrSpec Node

    type RelVarSpec = Map.Map Role MbrVar
      -- subset of RelSpec info, but transformable into RelSpec, because the
      -- rest of that info is carried external to the RelVarSpec, in the graph
    type RelSpec = Map.Map Role MbrSpec
      -- if well-formed, has a Tplt, and has RelPoss from 1 to the Tplt's Arity
      -- but|todo ? any (Mbr _) mapped to Any could be omitted
