    {-# LANGUAGE ViewPatterns #-}

    ghead :: Graph gr => gr a b -> Node
    ghead graph | isEmpty graph = error "Empty graph!"
    ghead (matchAny -> ((_, node, _, _), graph)) = node
