{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search (
    qGetDe
  , qGetLabDe
  , qGet1De
  , qPutDeSt
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
_qGetDe :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
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

qGetDe :: RSLT -> QNode -> Either DwtErr [Node]
qGetDe = _qGetDe (\_ n -> n) nodes matchRelDe

qGetLabDe :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLabDe = _qGetDe f labNodes matchRelLabDe where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1De :: RSLT -> QNode -> Either DwtErr Node
qGet1De g q = prependCaller "qGet1De: " $ case qGetDe g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = mQNode .~ Just q $ noErrOpts 

---- == Put
qPutDeSt :: QNode -> StateT RSLT (Either DwtErr) Node
qPutDeSt (QRel qt qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  let tag = prependCaller "qPutDeSt: " -- TODO: use
  t <- qPutDeSt qt
  ms <- mapM qPutDeSt qms
  g <- get
  let matches = matchRelDe g $ mkRelSpec t ms
  insRelDeSt t ms
qPutDeSt (QAt n) = lift $ Right n
qPutDeSt q@(QLeaf x) = get >>= \g -> case qGet1De g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutDeSt: " $ Left e

-- == Regex
qRegexWord :: RSLT -> String -> Either String [Node]
qRegexWord g s = do
  let r = mkRegex s
  let ns = nodes $ labfilter (\lab -> case lab of
                                 Word t -> Mb.isJust $ matchRegex r t;
                                 _ -> False)
                 $ dropEdges g
  Right ns
