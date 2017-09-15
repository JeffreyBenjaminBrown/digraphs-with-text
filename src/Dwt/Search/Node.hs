{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Search.Node (
  qGet
  , qGetLab
  , qGet1XX
  , qPutSt

  , qRegexWord

  -- for internal export, not for interface
  , NodeOrVarConcrete(..), RelSpecConcrete
  , _matchRelSpecNodes
  , _matchRelSpecNodesLab
  , _usersInRole
  , _mkRelSpec
) where

import Text.Regex

import Data.Graph.Inductive
import Dwt.Types
import Dwt.Graph
import Dwt.Util (fr, maxNode, dropEdges, fromRight, prependCaller, listIntersect, gelemM)
import Dwt.Leaf (insLeaf, extractTplt)
import Data.Maybe (fromJust)

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Lens
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import Control.Monad (foldM)

-- | "Concrete" in the sense that it requires specific nodes, not queries
data NodeOrVarConcrete = NodeSpecC Node
  | VarSpecC Mbrship deriving (Show,Read,Eq)
type RelSpecConcrete = M.Map RelRole NodeOrVarConcrete

_matchRelSpecNodes :: RSLT -> RelSpecConcrete -> Either DwtErr [Node]
_matchRelSpecNodes g spec = prependCaller "_matchRelSpecNodes: " $ do
  let nodeSpecs = M.toList
        $ M.filter (\ns -> case ns of NodeSpecC _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVarConcrete)]
  nodeListList <- mapM (\(r,NodeSpecC n) -> _usersInRole g n r) nodeSpecs
  return $ listIntersect nodeListList

-- ifdo speed: this searches for nodes, then searches again for labels
_matchRelSpecNodesLab :: RSLT -> RelSpecConcrete -> Either DwtErr [LNode Expr]
_matchRelSpecNodesLab g spec = prependCaller "_matchRelSpecNodesLab: " $ do
  ns <- _matchRelSpecNodes g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe because _matchRelSpecNodes only returns Nodes in g

-- | Rels using Node n in RelRole r
_usersInRole :: RSLT -> Node -> RelRole -> Either DwtErr [Node]
_usersInRole g n r = prependCaller "usersInRoleVerboseTypes: " $
  do gelemM g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

-- | Use when all the nodes the Rel involves are known.
_mkRelSpec :: Node -> [Node] -> RelSpecConcrete
_mkRelSpec t ns = M.fromList $ [(TpltRole, NodeSpecC t)] ++ mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpecC ns)


-- TODO: simplify some stuff (maybe outside of this file?) by using 
-- Graph.whereis :: RSLT -> Expr -> [Node] -- hopefully length = 1

_qGet :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpecConcrete -> Either DwtErr [x])
    -- | matchRelSpecNodesVerboseTypes or matchRelSpecNodesLabVerboseTypes; used for QRel
  -> RSLT -> Insertion -> Either DwtErr [x]
_qGet f _ _ g (At n) = return $ if gelem n g then [f g n] else []
_qGet _ f _ g (InsLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGet _ _ f g i@(InsRel _ _ qms) = prependCaller "_qGetVerboseTypes: " $ do
  t <- qGet1XX g (InsLeaf $ extractTplt i) -- todo ? multiple qt, qms matches
  ms <- mapM (qGet1XX g) qms
  let relspec = _mkRelSpec t ms
  f g relspec

qGet :: RSLT -> Insertion -> Either DwtErr [Node]
qGet = _qGet (\_ n -> n) nodes _matchRelSpecNodes

qGetLab :: RSLT -> Insertion -> Either DwtErr [LNode Expr]
qGetLab = _qGet f labNodes _matchRelSpecNodesLab where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1 :: RSLT -> Insertion -> Either DwtErr Node
qGet1 g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = [ErrInsertion q]

qGet1XX :: RSLT -> Insertion -> Either DwtErr Node
qGet1XX g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = [ErrInsertion q]

qPutSt :: Insertion -> StateT RSLT (Either DwtErr) Node
qPutSt i@(InsRel _ _ qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  -- let tag = prependCaller "qPutStVerboseTypes: " -- TODO: use
  t <- qPutSt $ InsLeaf $ extractTplt i
  ms <- mapM qPutSt $ filter (not . isAbsent) qms
  g <- get
  insRelSt t ms
qPutSt (At n) = lift $ Right n
qPutSt q@(InsLeaf x) = get >>= \g -> case qGet1XX g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutStVerboseTypes: " $ Left e

-- == Regex
qRegexWord :: RSLT -> String -> [Node]
qRegexWord g s = nodes $ labfilter f $ dropEdges g
  where r = mkRegex s
        f (Word t) = Mb.isJust $ matchRegex r t
        f _ = False
