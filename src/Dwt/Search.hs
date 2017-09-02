{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
  qGet
  , qLGet
  , qPut
  , qMbGet
  , qGet1De
  , qGet1
  , qRegexWord
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Util (fr, maxNode, lengthOne, dropEdges, fromRight)
import Dwt.Leaf (insLeaf)

import Data.Map as M
import Data.Maybe as Mb

-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.node :: RSLT -> Expr -> [Node] -- hopefully length = 1

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
      relspec = mkRelSpec t ms
  in f g relspec

qGet :: RSLT -> QNode -> Either String [Node]
qGet = _qGet (\_ n -> n) nodes matchRel

qLGet :: RSLT -> QNode -> Either String [LNode Expr]
qLGet = _qGet (\g n -> (n, fromJust $ lab g n)) labNodes matchRelLab
  -- this fromJust is excused by the gelem in _qGet

qPut :: RSLT -> QNode -> Either String (RSLT, Node)
qPut g (QRel qt qms) = do
  tplt <- qGet1 g qt --TODO: make qMbGet-like, use an okay Left for 0
  members <- mapM (qGet1 g) qms
  matches <- matchRel g $ mkRelSpec tplt members
  case matches of
    [a] -> Right (g,a)
    [] -> Right (g', maxNode g') where g' = fr $ insRel tplt members g
      -- fromRight is safe because tplt and members come from qGet1
    _ -> Left "qPut: can't handle multiple Rel matches yet"
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

qGet1De :: RSLT -> QNode -> Either DwtErr Node
qGet1De g q = case qGet g q of
  Right [] -> Left (FoundNo, noErrOpts, msg)
  Right [a] -> Right a
  Right as -> Left (FoundMany, noErrOpts, msg)
  Left s -> Left (Legacy, noErrOpts, s)
  where msg = "qGet1dwtErr searched for " ++ show q

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
