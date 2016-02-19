    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Search (
      module Dwt.Search
    ) where

    import Dwt.Graph
    import Data.Graph.Inductive

    data QNode = QNode Node | QStr String | QTplt [String] | QRel QNode [QNode]

    qGet :: QNode -> Mindmap -> Either String Node
    qGet (QNode n) _ = Right n
    qGet (QStr s) g = do
      let ns = nodes $ labfilter (\n -> case n of Str t -> s==t; _ -> False)
               $ gmap (\(_,b,c,_) -> ([], b, c, [])) g
      if length ns == 0 then Left "absent" else return ()
      if length ns > 1 then Left "multiple" else return ()
      Right $ head ns
