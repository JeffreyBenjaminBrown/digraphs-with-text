{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
  qGet, qLGet, qGet1
  , qMbGet
  , qGetDe, qGet1De
  , qPut, qPutDe, qPutDeSt
  , qRegexWord
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Util (fr, maxNode, lengthOne, dropEdges, fromRight, prependCaller)
import Dwt.Leaf (insLeaf)

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Lens
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import Control.Monad (foldM)

-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.whereis :: RSLT -> Expr -> [Node] -- hopefully length = 1

-- == Get
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

_qGetDe :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErr [x])
    -- | matchRelDe or matchRelLabDe; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGetDe f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGetDe _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGetDe _ _ f g (QRel qt qms) = prependCaller "_qGetDe: " $ do
  t <- qGet1De g qt   -- TODO ? case of multiple qt, qms matches
  ms <- mapM (qGet1De g) qms
  let relspec = mkRelSpec t ms
  f g relspec

qGet :: RSLT -> QNode -> Either String [Node]
qGet = _qGet (\_ n -> n) nodes matchRel

qLGet :: RSLT -> QNode -> Either String [LNode Expr]
qLGet = _qGet (\g n -> (n, Mb.fromJust $ lab g n)) labNodes matchRelLab
  -- this fromJust is excused by the gelem in _qGet

qGet1 :: RSLT -> QNode -> Either String Node
qGet1 g q = case qGet g q of
  Left s -> Left $ "qGet1: " ++ s
  Right [] -> Left "qGet1: Expected one match, found none."
  Right [a] -> Right a
  Right as -> Left "qGet1: Expected one match, found more."

qMbGet :: RSLT -> QNode -> Either String (Maybe Node)
qMbGet g q = case qGet g q of
  Right [] -> Right Nothing
  Right [a] -> Right $ Just a
  Right as -> Left $ "qMbGet: searched for " ++ show q
             ++ ", found multiple: " ++ show as
  Left s -> Left $ "qMbGet: " ++ s

qGetDe :: RSLT -> QNode -> Either DwtErr [Node]
qGetDe = _qGetDe (\_ n -> n) nodes matchRelDe

qGet1De :: RSLT -> QNode -> Either DwtErr Node
qGet1De g q = prependCaller "qGet1De: " $ case qGetDe g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = mQNode .~ Just q $ noErrOpts 

-- == Put
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

qPutDeSt :: QNode -> StateT RSLT (Either DwtErr) Node
qPutDeSt (QRel qt qms) = do
  let tag = prependCaller "qPutDeSt: "
  g <- get
  t <- qPutDeSt qt
  ms <- mapM qPutDeSt qms
  let matches = matchRelDe g $ mkRelSpec t ms
  lift $ Right 0 -- TODO
qPutDeSt (QAt n) = lift $ Right n
qPutDeSt q@(QLeaf x) = get >>= \g -> case qGet1De g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutDeSt: " $ Left e

qPutDe :: RSLT -> QNode -> Either DwtErr (RSLT, Node)
qPutDe g (QRel qt qms) = prependCaller "qPutDe: " $ do
  (g1, tplt) <- qPutDe g qt
  -- TODO: something like: members <- map snd $ mapM (qPutDe g1) qms
  members <- mapM (qGet1De g1) qms
  matches <- matchRelDe g $ mkRelSpec tplt members
  case matches of
    [a] -> return (g,a)
    [] -> Right (g', maxNode g') where g' = fr $ insRel tplt members g
      -- fromRight is safe because tplt and members come from qGet1
    _ -> Left (FoundMany, noErrOpts, ".")
qPutDe g q@(QLeaf l) = prependCaller "qPutDe: " $ case qGet1De g q of
  Right n -> Right (g, n)
  Left (FoundNo,_,_) -> Right (g', maxNode g') where g' = insLeaf l g
  Left e -> Left e

-- == Regex
qRegexWord :: RSLT -> String -> Either String [Node]
qRegexWord g s = do
  let r = mkRegex s
  let ns = nodes $ labfilter (\lab -> case lab of
                                 Word t -> Mb.isJust $ matchRegex r t;
                                 _ -> False)
                 $ dropEdges g
  Right ns
