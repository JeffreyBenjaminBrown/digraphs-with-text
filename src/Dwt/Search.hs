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

edgeless :: Gr a b -> Gr a b -- ? faster or slower
edgeless = gmap (\(_,b,c,_) -> ([], b, c, []))

qGet :: RSLT -> QNode -> Either String Node
qGet g (QNode n) = gelemM g n >> Right n
qGet g (QWord s) = do
  let ns = nodes $ labfilter (\n -> case n of Word t -> s==t; _ -> False)
           $ edgeless g
  lengthOne ns
  Right $ head ns
qGet g (QTplt s) = do
  let ns = nodes $ labfilter (\n -> case n of Tplt t -> s==t; _ -> False)
           $ edgeless g
  lengthOne ns
  Right $ head ns

qRegexWord :: RSLT -> String -> Either String [Node]
qRegexWord g s = do
  let r = mkRegex s
  let ns = nodes $ labfilter (\lab -> case lab of
                                 Word t -> Mb.isJust $ matchRegex r t;
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
