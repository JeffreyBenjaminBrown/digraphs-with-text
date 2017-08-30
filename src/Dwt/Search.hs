{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
  module Dwt.Search
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Graph
import Dwt.Util (maxNode, lengthOne, dropEdges)

import Data.Map as M
import Data.Maybe as Mb

-- see also
-- Graph.node :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- queries
data QNode = QAt Node -- when you already know the Node
           | QLeaf Expr -- when you don't but you know its contents
           | QRel QNode [QNode]
  deriving (Show, Eq)

_qGet :: (RSLT -> Node -> x) -- | Used for QAt
      -> (RSLT -> [x])       -- | Used for leaves
      -> RSLT -> QNode -> [x]
_qGet f _ g (QAt n) =  if gelem n g then [f g n] else []
_qGet _ f g (QLeaf l) = f $ labfilter (==l) $ dropEdges g
--_qGet _ f g (QRel qt qms) = -- ignoring case of multiple qt, qm matches
--  let t = qGet1 g qt
--      ms = fmap (qGet1 g) qms
--      mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpec ms)
--      relspec = M.fromList $ [(TpltRole, NodeSpec t)] ++ mbrSpecs
--  in matchRel g relspec

qGet :: RSLT -> QNode -> [Node]
qGet = _qGet (\_ n -> n) nodes

qPut :: RSLT -> QNode -> Either String (RSLT, Node)
qPut g (QRel qt qms) = do
  tplt <- qGet1 g qt
  members <- mapM (qGet1 g) qms
  g' <- insRel tplt members g
  Right (g', error "pending: write qGet for Rels")
qPut g q@(QLeaf l) = either left right $ qMbGet g q where
  right (Just n) = Right (g, n)
  right Nothing = Right (g', maxNode g') where g' = insLeaf l g
  left s = Left $ "qPut called: " ++ s

qLGet :: RSLT -> QNode -> [LNode Expr]
qLGet = _qGet (\g n -> (n, fromJust $ lab g n)) labNodes
  -- this fromJust is excused by the gelem in _qGet

qMbGet :: RSLT -> QNode -> Either String (Maybe Node)
qMbGet g q = case qGet g q of
  [] -> Right Nothing
  [a] -> Right $ Just a
  as -> Left $ "qMbGet: searched for " ++ show q
             ++ ", found multiple: " ++ show as

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
