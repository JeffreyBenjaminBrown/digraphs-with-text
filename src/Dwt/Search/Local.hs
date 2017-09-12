{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search.Local (
    qGet
  , qGetLab
  , qGet1 
  , qPutSt
  , qRegexWord

  -- exported for testing, but not for interface
  , _matchRelSpecNodes
  , _matchRelSpecNodesLab
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Util (fr, maxNode, dropEdges, fromRight, prependCaller, listIntersect)
import Dwt.Leaf (insLeaf)
import Data.Maybe (fromJust)

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Lens
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import Control.Monad (foldM)

_matchRelSpecNodes :: RSLT -> RelSpec -> Either DwtErr [Node]
_matchRelSpecNodes g spec = prependCaller "_matchRelSpecNodes: " $ do
  let nodeSpecs = M.toList
        $ M.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRole g n r) nodeSpecs
  return $ listIntersect nodeListList

-- ifdo speed: this searches for nodes, then searches again for labels
_matchRelSpecNodesLab :: RSLT -> RelSpec -> Either DwtErr [LNode Expr]
_matchRelSpecNodesLab g spec = prependCaller "_matchRelSpecNodesLab: " $ do
  ns <- _matchRelSpecNodes g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe because _matchRelSpecNodes only returns Nodes in g

-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.whereis :: RSLT -> Expr -> [Node] -- hopefully length = 1

_qGet :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErr [x])
    -- | matchRelSpecNodes or matchRelSpecNodesLab; used for QRel
  -> RSLT -> QNode -> Either DwtErr [x]
_qGet f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGet _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g (QRel qt qms) = prependCaller "_qGet: " $ do
  t <- qGet1 g qt   -- TODO ? case of multiple qt, qms matches
  ms <- mapM (qGet1 g) qms
  let relspec = mkRelSpec t ms
  f g relspec

qGet :: RSLT -> QNode -> Either DwtErr [Node]
qGet = _qGet (\_ n -> n) nodes matchRelSpecNodes

qGetLab :: RSLT -> QNode -> Either DwtErr [LNode Expr]
qGetLab = _qGet f labNodes matchRelSpecNodesLab where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1 :: RSLT -> QNode -> Either DwtErr Node
qGet1 g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = [ErrQNode q]

qPutSt :: QNode -> StateT RSLT (Either DwtErr) Node
qPutSt (QRel qt qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  let tag = prependCaller "qPutSt: " -- TODO: use
  t <- qPutSt qt
  ms <- mapM qPutSt qms
  g <- get
  let matches = matchRelSpecNodes g $ mkRelSpec t ms
  insRelSt t ms
qPutSt (QAt n) = lift $ Right n
qPutSt q@(QLeaf x) = get >>= \g -> case qGet1 g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutSt: " $ Left e

-- == Regex
qRegexWord :: RSLT -> String -> [Node]
qRegexWord g s = nodes $ labfilter f $ dropEdges g
  where r = mkRegex s
        f (Word t) = Mb.isJust $ matchRegex r t
        f _ = False
