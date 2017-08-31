{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
  module Dwt.Search
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Graph
import Dwt.Util (maxNode, lengthOne, dropEdges, fromRight)

import Data.Map as M
import Data.Maybe as Mb

-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.node :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- queries
data QNode = QAt Node -- when you already know the Node
           | QLeaf Expr -- when you don't but you know its contents
           | QRel QNode [QNode]
  deriving (Show, Eq)

_qGet :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either String [x]) -- | matchRel or matchRelLab; QRel
  -> RSLT -> QNode -> Either String [x]
_qGet f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGet _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g (QRel qt qms) = -- TODO: case of multiple qt, qm matches
  let t = fromRight $ qGet1 g qt
      ms = fmap (fromRight . qGet1 g) qms
      mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpec ms)
      relspec = M.fromList $ [(TpltRole, NodeSpec t)] ++ mbrSpecs
  in f g relspec

qGet :: RSLT -> QNode -> Either String [Node]
qGet = _qGet (\_ n -> n) nodes matchRel

qLGet :: RSLT -> QNode -> Either String [LNode Expr]
qLGet = _qGet (\g n -> (n, fromJust $ lab g n)) labNodes matchRelLab
  -- this fromJust is excused by the gelem in _qGet

qPut :: RSLT -> QNode -> Either String (RSLT, Node)
qPut g (QRel qt qms) = do
  tplt <- qGet1 g qt
  members <- mapM (qGet1 g) qms
  g' <- insRel tplt members g --TODO: what if it's already there?
  Right (g', 1234567890) -- TODO: why does this already work?
qPut g q@(QLeaf l) = case qMbGet g q of
  Right (Just n) -> Right (g, n)
  Right Nothing -> Right (g', maxNode g') where g' = insLeaf l g
  Left s -> Left $ "qPut: " ++ s

qMbGet :: RSLT -> QNode -> Either String (Maybe Node)
qMbGet g q = case qGet g q of
  Right [] -> Right Nothing
  Right [a] -> Right $ Just a
  Right as -> Left $ "qMbGet: searched for " ++ show q
             ++ ", found multiple: " ++ show as
  Left s -> Left $ "qMbGet: " ++ s

qGet1 :: RSLT -> QNode -> Either String Node
qGet1 g q = case qGet g q of
  Left s -> Left $ "qGet1: " ++ s
  Right [] -> Left "qGet1: Expected one match, found none."
  Right [a] -> Right a
  Right as -> Left "qGet1: Expected one match, found more."

qRegexWord :: RSLT -> String -> Either String [Node]
qRegexWord g s = do
  let r = mkRegex s
  let ns = nodes $ labfilter (\lab -> case lab of
                                 Word t -> Mb.isJust $ matchRegex r t;
                                 _ -> False)
                 $ dropEdges g
  Right ns
