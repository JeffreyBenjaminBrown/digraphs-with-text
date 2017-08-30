{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
  module Dwt.Search
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Graph
import Dwt.Util (lengthOne, dropEdges)

import Data.Maybe as Mb

-- see also
-- Graph.node :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- queries
data QNode = QNode Node -- when you already know the Node
  | QWord String | QTplt [String] -- when you don't but you know its contents
  | QRel QNode [QNode]
  deriving (Show, Eq)

_qGet :: (RSLT -> Node -> x) -- | Used for QNodes
      -> (RSLT -> [x])       -- | Used for everything else
      -> RSLT -> QNode -> [x]
_qGet f _ g (QNode n) =  if gelem n g then [f g n] else []
_qGet _ f g (QWord s) = f
  $ labfilter (\n -> case n of Word t -> s==t; _ -> False)
  $ dropEdges g
_qGet _ f g (QTplt s) = f
  $ labfilter (\n -> case n of Tplt t -> s==t; _ -> False)
  $ dropEdges g
-- TODO: Consider Dwt.Graph.matchRel (and RelSpec, ..)
--_qGet f g (QRel t ns) =
--  let matchedNodes = map (qGet g) ns :: [[Node]]
--      matchedTplts = qGet g t :: [Node]
--  -- TODO for speed: limit search to the intersection of predecessors of
--  -- the template and each Mbr k set
--      f :: Node -> Bool
--      f n = True -- TODO: lpre g n ...
--  in if True -- tpltArity t == length matchedNodes
--     then nodes $ nfilter f g
--     else error $ "arity of " ++ show t
--          ++ " not equal to length of member list"
--  -- find every node that sends a TpltRole to something in tpltWays
--  -- and a Mbr k to something in the kth _

qGet :: RSLT -> QNode -> [Node]
qGet = _qGet (\_ n -> n) nodes

qLGet :: RSLT -> QNode -> [LNode Expr]
qLGet = _qGet (\g n -> (n, fromJust $ lab g n)) labNodes
  -- this fromJust is excused by the gelem in _qGet

qMbGet :: RSLT -> QNode -> Either String (Maybe Node)
qMbGet g q = case qGet g q of
  [] -> Right Nothing
  [a] -> Right $ Just a
  many -> Left $ "qMbGet: searched for " ++ show q
             ++ ", found multiple: " ++ show many

qGet1 :: RSLT -> QNode -> Either String Node
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
