    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Search (
      module Dwt.Search
    ) where

    import Text.Regex

    import Dwt.Graph
    import Data.Graph.Inductive

    import Data.Map as Map
    import Data.Maybe as Mb

    -- see also
    -- Graph.node :: RSLT -> Expr -> [Node] -- hopefully length = 1

    -- queries
    data QNode = QNode Node -- when you already know the Node
      | QStr String | QTplt [String] -- when you don't but you know its contents
      | QRel QNode [QNode] -- todo ? use
      deriving (Show, Eq)

    lengthOne :: [a] -> Either String ()
    lengthOne ns = do
      if length ns == 0 then Left "absent" else return ()
      if length ns > 1 then Left "multiple" else return ()

    edgeless :: Gr a b -> Gr a b -- ? faster or slower
    edgeless = gmap (\(_,b,c,_) -> ([], b, c, []))

    qGet :: RSLT -> QNode -> Either String Node
    qGet g (QNode n) = gelemM g n >> Right n
    qGet g (QStr s) = do
      let ns = nodes $ labfilter (\n -> case n of Str t -> s==t; _ -> False)
               $ edgeless g
      lengthOne ns
      Right $ head ns
    qGet g (QTplt s) = do
      let ns = nodes $ labfilter (\n -> case n of Tplt t -> s==t; _ -> False)
               $ edgeless g
      lengthOne ns
      Right $ head ns

    qRegexStr :: RSLT -> String -> Either String [Node]
    qRegexStr g s = do
      let r = mkRegex s
      let ns = nodes $ labfilter 
                       (\lab -> case lab of Str t -> Mb.isJust $ matchRegex r t;
                                            _ -> False)
                       $ edgeless g
      Right ns

    qInsRel :: QNode -> [QNode] -> RSLT -> Either String RSLT
    qInsRel qtn qns g = do
      tn <- qGet g qtn 
      ns <- mapM (qGet g) qns
      insRel tn ns g

-- very stale
    --    qGet g (QRel qt qes) = do
    --      let tn = qGet qt g
    --          es = mapM (flip qGet g) es
    --          rs = Map.fromList $ tn : es :: RelSpec
    --          ns = matchRel g rs
    --      lengthOne ns -- ifdo clarify: errors look the same as those from qGet above
    --      Right $ head ns
