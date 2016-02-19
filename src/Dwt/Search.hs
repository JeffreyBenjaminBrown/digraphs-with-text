    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Search (
      module Dwt.Search
    ) where

    import Dwt.Graph
    import Data.Graph.Inductive

    import Data.Map as Map

    data QNode = QNode Node | QStr String | QTplt [String] | QRel QNode [QNode]

    lengthOne :: [a] -> Either String ()
    lengthOne ns = do
      if length ns == 0 then Left "absent" else return ()
      if length ns > 1 then Left "multiple" else return ()

    edgeless :: Mindmap -> Mindmap
    edgeless = gmap (\(_,b,c,_) -> ([], b, c, []))

    qGet :: QNode -> Mindmap -> Either String Node
    qGet (QNode n) g = gelemM g n >> Right n
    qGet (QStr s) g = do
      let ns = nodes $ labfilter (\n -> case n of Str t -> s==t; _ -> False)
               $ edgeless g
      lengthOne ns
      Right $ head ns
    qGet (QTplt s) g = do
      let ns = nodes $ labfilter (\n -> case n of Tplt t -> s==t; _ -> False)
               $ edgeless g
      lengthOne ns
      Right $ head ns
--    qGet (QRel qt qes) g = do
--      let tn = qGet qt g
--          es = mapM (flip qGet g) es
--          rs = Map.fromList $ tn : es :: RelSpec
--          ns = matchRel g rs
--      lengthOne ns -- ifdo clarify: errors look the same as those from qGet above
--      Right $ head ns
