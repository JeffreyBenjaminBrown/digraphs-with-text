{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
    qGet
  , qGetLab, qGetLabSum
  , qGet1, qGet1Sum 
  , qPutSt, qPutStSum
  , qRegexWord
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Util (fr, maxNode, dropEdges, fromRight, prependCaller, prependCallerSum)
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

-- ==== The old verbose DwtErr way
-- == Get
_qGet :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErr [x])
    -- | matchRel or matchRelLab; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGet f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGet _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g (QRel qt qms) = prependCaller "_qGet: " $ do
  t <- qGet1 g qt   -- TODO ? case of multiple qt, qms matches
  ms <- mapM (qGet1 g) qms
  let relspec = mkRelSpec t ms
  f g relspec

qGet :: RSLT -> QNode -> Either DwtErr [Node]
qGet = _qGet (\_ n -> n) nodes matchRel

qGetLab :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLab = _qGet f labNodes matchRelLab where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1 :: RSLT -> QNode -> Either DwtErr Node
qGet1 g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = mQNode .~ Just q $ noErrOpts 

---- == Put
qPutSt :: QNode -> StateT RSLT (Either DwtErr) Node
qPutSt (QRel qt qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  let tag = prependCaller "qPutSt: " -- TODO: use
  t <- qPutSt qt
  ms <- mapM qPutSt qms
  g <- get
  let matches = matchRel g $ mkRelSpec t ms
  insRelSt t ms
qPutSt (QAt n) = lift $ Right n
qPutSt q@(QLeaf x) = get >>= \g -> case qGet1 g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutSt: " $ Left e

-- ==== The new terse DwtErrSum way
_qGetSum :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErrSum [x])
    -- | matchRelSum or matchRelLabSum; used for QRel
  -> RSLT -> QNode -> Either DwtErrSum [x]
_qGetSum f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGetSum _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGetSum _ _ f g (QRel qt qms) = prependCallerSum "_qGet: " $ do
  t <- qGet1Sum g qt   -- TODO ? case of multiple qt, qms matches
  ms <- mapM (qGet1Sum g) qms
  let relspec = mkRelSpec t ms
  f g relspec

qGetSum :: RSLT -> QNode -> Either DwtErrSum [Node]
qGetSum = _qGetSum (\_ n -> n) nodes matchRelSum

qGetLabSum :: RSLT -> QNode -> Either DwtErrSum [LNode Expr]
qGetLabSum = _qGetSum f labNodes matchRelLabSum where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1Sum :: RSLT -> QNode -> Either DwtErrSum Node
qGet1Sum g q = prependCallerSum "qGet1Sum: " $ case qGetSum g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = [ErrQNode q]

qPutStSum :: QNode -> StateT RSLT (Either DwtErrSum) Node
qPutStSum (QRel qt qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  let tag = prependCallerSum "qPutSt: " -- TODO: use
  t <- qPutStSum qt
  ms <- mapM qPutStSum qms
  g <- get
  let matches = matchRelSum g $ mkRelSpec t ms
  insRelStSum t ms
qPutStSum (QAt n) = lift $ Right n
qPutStSum q@(QLeaf x) = get >>= \g -> case qGet1Sum g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCallerSum "qPutSt: " $ Left e

-- == Regex
qRegexWord :: RSLT -> String -> [Node]
qRegexWord g s = nodes $ labfilter f $ dropEdges g
  where r = mkRegex s
        f (Word t) = Mb.isJust $ matchRegex r t
        f _ = False
