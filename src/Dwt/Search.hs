{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
  module Dwt.Search
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Graph
import Dwt.Util (lengthOne)

import Data.Map as Map
import Data.Maybe as Mb

-- see also
-- Graph.node :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- queries
data QNode = QNode Node -- when you already know the Node
  | QWord String | QTplt [String] -- when you don't but you know its contents
  | QRel QNode [QNode] -- todo ? use
  deriving (Show, Eq)

dropEdges :: Gr a b -> Gr a b -- ? faster or slower
dropEdges = gmap (\(_,b,c,_) -> ([], b, c, []))

qGet :: RSLT -> QNode -> [Node]
qGet g (QWord s) = nodes
  $ labfilter (\n -> case n of Word t -> s==t; _ -> False)
  $ dropEdges g
qGet g (QTplt s) = nodes
  $ labfilter (\n -> case n of Tplt t -> s==t; _ -> False)
  $ dropEdges g
qGet g (QNode n) = error "qGet called on a QNode. Use qGet1 instead."

qGet1 :: RSLT -> QNode -> Either String Node
qGet1 g (QNode n) = if gelem n g then Right n
  else Left $ "qGet1: node " ++ show n ++ " not in graph."
qGet1 g q = let ns = qGet g q
            in case length ns of
                 0 -> Left "qGet1: Expected one match, found none."
                 1 -> Right $ head ns
                 _ -> Left "qGet1: Expected one match, found more."

qRegexWord :: RSLT -> String -> Either String [Node]
qRegexWord g s = do
  let r = mkRegex s
  let ns = nodes $ labfilter (\lab -> case lab of
                                 Word t -> Mb.isJust $ matchRegex r t;
                                 _ -> False)
                   $ dropEdges g
  Right ns

qInsRel :: QNode -> [QNode] -> RSLT -> Either String RSLT
qInsRel qtn qns g = do
  tn <- qGet1 g qtn 
  ns <- mapM (qGet1 g) qns
  insRel tn ns g

-- very stale
    --    qGet g (QRel qt qes) = do
    --      let tn = qGet qt g
    --          es = mapM (flip qGet g) es
    --          rs = Map.fromList $ tn : es :: RelSpec
    --          ns = matchRel g rs
    --      lengthOne ns -- ifdo clarify: errors look the same as those from qGet above
    --      Right $ head ns
