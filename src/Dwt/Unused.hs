    replaceUsf :: Node -> Expr -> RSLT -> RSLT
    replaceUsf n expr g =
      let (Just (a,b,expr',d), g') = match n g
      in if areLikeExprs expr expr' then (a,b,expr,d) & g' 
                                    else error "unlike Exprs"

