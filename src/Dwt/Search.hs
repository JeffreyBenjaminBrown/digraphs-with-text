{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
    qGetStrErr
  , qGetLab
  , qGet1
  , qPutSt
  , qRegexWord
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Util (fr, maxNode, dropEdges, fromRight, prependCaller)
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
_qGetStrErr :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErr [x])
    -- | matchRel or matchRelLab; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGetStrErr f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGetStrErr _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGetStrErr _ _ f g (QRel qt qms) = prependCaller "_qGetStrErr: " $ do
  t <- qGet1 g qt   -- TODO ? case of multiple qt, qms matches
  ms <- mapM (qGet1 g) qms
  let relspec = mkRelSpec t ms
  f g relspec

qGetStrErr :: RSLT -> QNode -> Either DwtErr [Node]
qGetStrErr = _qGetStrErr (\_ n -> n) nodes matchRel

qGetLab :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLab = _qGetStrErr f labNodes matchRelLab where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1 :: RSLT -> QNode -> Either DwtErr Node
qGet1 g q = prependCaller "qGet1: " $ case qGetStrErr g q of
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

-- == Regex
qRegexWord :: RSLT -> String -> Either String [Node]
qRegexWord g s = do
  let r = mkRegex s
  let ns = nodes $ labfilter (\lab -> case lab of
                                 Word t -> Mb.isJust $ matchRegex r t;
                                 _ -> False)
                 $ dropEdges g
  Right ns
