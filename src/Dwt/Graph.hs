{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Graph (
  insRelUsf
  , insRel
  , insRelSt
  , insColl
  , mkRelSpec, partitionRelSpec
  , insRelSpec
  , relNodeSpec
  , relSpec
  , chLeaf
  , chRelRole
  , whereis
  , tpltAt
  , relElts
  , validRole
  , relTplt
  , collPrinciple
  , rels, mbrs
  , users
  , usersInRole
  , matchRelSpecNodes
  , matchRelSpecNodesLab
  , has1Dir, otherDir
  , fork1Dir
  , subNodeForVars
  , dwtDfs, dwtBfs
  ) where

import Dwt.Types
import Dwt.Leaf
import Dwt.Util
import Data.Graph.Inductive hiding (lift)
import Data.Either (partitionEithers)
import Data.List (intersect, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (mapM_)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Text (pack, unpack, strip, splitOn)
import Control.Lens hiding ((&))

-- ======== build
insRel :: Node -> [Node] -> RSLT -> Either DwtErr RSLT
insRel template mbrs g =
  do mapM_ (gelemM g) $ template:mbrs
     tplt <- tpltAt g template
     mbrListMatchesTpltArity mbrs tplt
     return $ addMbrs (zip mbrs [1..tpltArity tplt]) $ addTplt g
  where newNode = head $ newNodes 1 g
        addMbrs []     g = g
        addMbrs (p:ps) g = addMbrs ps $ insEdge
          (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
        addTplt = insEdge (newNode, template, RelEdge TpltRole)
                  . insNode (newNode, Rel) :: RSLT -> RSLT

insRelSt :: Node -> [Node] -> StateT RSLT (Either DwtErr) Node
insRelSt template mbrs =
  do g <- get
     let newNode = head $ newNodes 1 g
         addMbrs []     g = g
         addMbrs (p:ps) g = addMbrs ps $ insEdge
           (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
         addTplt = insEdge (newNode, template, RelEdge TpltRole)
           . insNode (newNode, Rel) :: RSLT -> RSLT
     lift $ mapM_ (gelemM g) $ template:mbrs
     tplt <- tpltAt g template
     mbrListMatchesTpltArity mbrs tplt
     modify $ addMbrs (zip mbrs [1..tpltArity tplt]) . addTplt
     g' <- get
     return $ maxNode g'

-- | Deprecated
insColl :: (Maybe Node) -- title|principle, e.g. "alternatives"
        -> [Node] -> RSLT -> Either DwtErr RSLT
insColl mt ns g = do
  mapM_ (gelemM g) ns
  let newNode = head $ newNodes 1 g
      nameEdges = case mt of
        Nothing -> []
        Just tn -> [(newNode, tn,CollEdge CollPrinciple)]
      newEdges = nameEdges ++
        map (\n -> (newNode, n, CollEdge CollMbr)) ns
  return $ insEdges newEdges $ insNode (newNode,Coll) g

-- | Covers the case where all the nodes the Rel involves are known.
-- | For a framework involving uncertainty, see Dwt.Search.QNode
mkRelSpec :: Node -> [Node] -> RelSpec
mkRelSpec t ns = Map.fromList $ [(TpltRole, NodeSpec t)] ++ mbrSpecs
  where mbrSpecs = zip (fmap Mbr [1..]) (fmap NodeSpec ns)

partitionRelSpec :: RelSpec -> (RelVarSpec, RelNodeSpec)
partitionRelSpec rSpec =
  let f mSpec = case mSpec of VarSpec _ -> True
                              NodeSpec _ -> False
      (vs,ns) = Map.partition f rSpec
  in ( Map.map  (\(VarSpec  v) -> v)  vs
     , Map.map  (\(NodeSpec n) -> n)  ns )

insRelSpec :: RelSpec -> RSLT -> Either DwtErr RSLT
insRelSpec rSpec g = do
  let (varMap, nodeMap) = partitionRelSpec rSpec
      newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelSpecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemM g) $ Map.elems nodeMap
  let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                $ Map.toList nodeMap
        -- these edges specify the addressed nodes
  return $ insEdges newLEdges $ insNode newLNode g

relNodeSpec :: RSLT -> Node -> Either DwtErr RelNodeSpec
relNodeSpec g n = prependCaller "relNodeSpec: " $ do
  gelemM g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
    Just _ -> Left
      (NotRelSpecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

relSpec :: RSLT -> Node -> Either DwtErr RelSpec
  -- name ? getRelSpecDe
  -- is nearly inverse to partitionRelSpec
relSpec g n = prependCaller "relSpec: " $ do
  gelemM g n
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      let rnsl = Map.toList $ fromRight $ relNodeSpec g n
          rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'

-- ======== edit (but not insert)
chLeaf :: RSLT -> Node -> Expr -> Either DwtErr RSLT
chLeaf g n e' = prependCaller "chLeaf: " $ do
  let me = lab g n
  case me of
    Just e@(isLeaf -> True) -> if areLikeExprs e e' then return ()
      else Left (ConstructorMistmatch, [], ".")
    Nothing -> Left (FoundNo, [ErrNode n], ".")
    _       -> Left (NotLeaf, [ErrNode n], ".")
  return $ _chLeafUsf g n e'

_chLeafUsf :: RSLT -> Node -> Expr -> RSLT
_chLeafUsf g n newExpr = let (Just (a,b,c,d),g') = match n g
  in (a,b,newExpr,d) & g'

chRelRole :: RSLT -> Node -> Node -> RelRole -> Either DwtErr RSLT
chRelRole g user newMbr role = do
  isRelM g user
  gelemM g newMbr
  let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      err = (Invalid, [ErrNode user, ErrRelRole role], "chRelRole.")
  case candidates of
    [] -> Left $ _1 .~ FoundNo $ err
    [a] -> return $ delLEdge (user,a,RelEdge role)
           $ insEdge (user,newMbr,RelEdge role) g
    _ -> Left $ _1 .~ FoundMany $ err

-- ======== query
-- ==== more complex ("locate"?) queries
whereis :: RSLT -> Expr -> [Node]
  -- TODO: dependent types. (hopefully, length = 1)
  -- move ? Search.hs
  -- name ? exprOf
whereis g x = nodes $ labfilter (== x) g

tpltAt :: (MonadError DwtErr m) => RSLT -> Node -> m Expr
tpltAt g tn = let name = "tpltAt." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, [ErrNode tn], name)
  _       -> throwError (NotTplt, [ErrNode tn], name)

-- todo: add prependCaller
relElts :: RSLT -> Node -> [RelRole] -> Either DwtErr [Node]
relElts g relNode roles = do
  isRelM g relNode
  mapM_  (validRole g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

validRole :: RSLT -> Node -> RelRole -> Either DwtErr ()
validRole g relNode role = isRelM g relNode >> case role of
  TpltRole -> return ()
  Mbr p -> do
    if p >= 1 then return () else Left err
    t <- relTplt g relNode
    let a = tpltArity t
    if p <= a then return ()
      else Left $ _1 .~ ArityMismatch $ _2 %~ (ErrExpr t:) $ err
  where err = (Invalid, [ErrRelRole role], "validRoleStrErr.")

relTplt :: RSLT -> Node -> Either DwtErr Expr -- unsafe
  -- might not be called on a template
relTplt g relNode = do
  [n] <- relElts g relNode [TpltRole]
  return $ fromJust $ lab g n

collPrinciple :: RSLT -> Node -> Either DwtErr Expr
  -- analogous to relTplt
collPrinciple g collNode = do
  prependCaller "collPrincipleDe: " $ isCollM g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

-- ==== .. -> [Node]
rels :: Gr Expr b -> [Node]
rels = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False)

-- opposites: mbrs, users
  -- though they would not be if Tplts pointed to|had members of their own
mbrs :: RSLT -> Node -> [Node]
mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
  where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

-- Words and Tplts are used, but are not users. (Rels and Colls use them.)
users :: Graph gr => gr a b -> Node -> Either DwtErr [Node]
users g n = do gelemM g n
               return [m | (m,label@_) <- lpre g n]

-- | Rels using Node n in RelRole r
usersInRole :: RSLT -> Node -> RelRole -> Either DwtErr [Node]
usersInRole g n r = prependCaller "usersInRole: " $
  do gelemM g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

matchRelSpecNodes :: RSLT -> RelSpec -> Either DwtErr [Node]
matchRelSpecNodes g spec = prependCaller "matchRelSpecNodes: " $ do
  let nodeSpecs = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,NodeOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRole g n r) nodeSpecs
  return $ listIntersect nodeListList

-- ifdo speed: this searches for nodes, then searches again for labels
matchRelSpecNodesLab :: RSLT -> RelSpec -> Either DwtErr [LNode Expr]
matchRelSpecNodesLab g spec = prependCaller "matchRelSpecNodesLab: " $ do
  ns <- matchRelSpecNodes g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe because matchRelSpecNodes only returns Nodes in g

-- ======== using directions (RelSpecs)
-- todo ? 1Dir because it should have one such direction. I forget why.
  -- clarif: if I want a generation in the Down direction of the rel "has/",
  -- the RelSpec has to have only one Up variable.
-- TODO ? check: Up|Down good, Any|It bad
  -- fork1Up uses otherDir, so it will catch those errors, but obscurely

has1Dir :: Mbrship -> RelSpec -> Bool
has1Dir mv rc = 1 == length (Map.toList $ Map.filter f rc)
  where f (VarSpec y) = y == mv
        f _ = False

otherDir :: Mbrship -> Either DwtErr Mbrship
otherDir Up = Right Down
otherDir Down = Right Up
otherDir mv = Left (ConstructorMistmatch, [ErrMbrship mv]
                   , "otherDir: Only accepts Up or Down.")

fork1Dir :: RSLT -> Node -> (Mbrship,RelSpec) -> Either DwtErr [Node]
fork1Dir g from (dir,axis) = do -- returns one generation, neighbors
  fromDir <- otherDir dir
  if has1Dir fromDir axis then return ()
     else Left (Invalid, [ErrRelSpec axis]
               , "fork1DirSum: should have only one " ++ show fromDir)
  let axis' = subNodeForVars from fromDir axis
      dirRoles = Map.keys $ Map.filter (== VarSpec dir) axis
  rels <- matchRelSpecNodes g axis'
  concat <$> mapM (\rel -> relElts g rel dirRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
      -- EXCEPT: that might hurt the dfs, bfs functions below

fork1Dirs :: RSLT -> Node -> [(Mbrship,RelSpec)] -> Either DwtErr [Node]
fork1Dirs g n rs = concat <$> mapM (fork1Dir g n) rs

subNodeForVars :: Node -> Mbrship -> RelSpec -> RelSpec
subNodeForVars n v r = Map.map f r -- ^ change each VarSpec v to NodeSpec n
  where f (VarSpec v') = if v == v' then NodeSpec n else VarSpec v'
        f x = x -- yes, the v,v' distinction is needed

-- ==== dfs and bfs
  -- algorithmically, the difference is only newNodes++ns v. ns++newNodes
_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- | determines dfs|bfs
  -> RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> Either DwtErr [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g dir pending@(n:ns) acc = do
  newNodes <- fork1Dir g n dir -- ifdo speed: calls has1Dir redundantly
  _bfsOrDfs collector g dir (nub $ collector newNodes ns) (n:acc)
    -- ifdo speed: discard visited nodes from graph

_dwtBfs = _bfsOrDfs (\new old -> old ++ new)
_dwtDfs = _bfsOrDfs (\new old -> new ++ old)

dwtDfs :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either DwtErr [Node]
dwtDfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> _dwtDfs g dir starts []

dwtBfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either DwtErr [Node]
dwtBfs g dir starts = do mapM_ (gelemM g) $ starts
                         (nub . reverse) <$> _dwtBfs g dir starts []

-- ========= Deprecated
insRelUsf :: Node -> [Node] -> RSLT -> RSLT
insRelUsf t ns g = case insRel t ns g of
  Left s -> error $ show s
  Right r -> r
