
*Main> g
mkGraph [(0,StrExpr "dog")] []
*Main> g <- pure $ insStr "cat" g
*Main> g
mkGraph [(0,StrExpr "dog"),(1,StrExpr "cat")] []
*Main> 
