{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Graph (
  insRelUsf
  , insRelDeprecatoryName, insRelSum, insRelStDeprecatoryName, insRelStSum
  , insColl
  , mkRelSpec, partitionRelSpec, insRelSpecDeprecatoryName, insRelSpecSum
  , relNodeSpecDeprecatoryName, relNodeSpecSum, relSpecDeprecatoryName, relSpecSum
  , chLeafDeprecatoryName, chLeafSum, chRelRoleDeprecatoryName, chRelRoleSum
  , whereis, tpltAtDeprecatoryName, tpltAtSum
  , relEltsDeprecatoryName, validRoleDeprecatoryName, relTpltDeprecatoryName
  , collPrincipleDeprecatoryName, collPrincipleSum
  , rels, mbrs, usersDeprecatoryName, usersSum, usersInRoleDeprecatoryName, usersInRoleSum
  , matchRelDeprecatoryName, matchRelSum, matchRelLabDeprecatoryName, matchRelLabSum
  , has1Dir, otherDir, fork1Dir, subNodeForVars, dwtDfs, dwtBfs
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
insRelDeprecatoryName :: Node -> [Node] -> RSLT -> Either DwtErrDeprecatoryName RSLT
insRelDeprecatoryName template mbrs g =
  do mapM_ (gelemMDeprecatoryName g) $ template:mbrs
     tplt <- tpltAtDeprecatoryName g template
     mbrListMatchesTpltArityDeprecatoryName mbrs tplt
     return $ addMbrs (zip mbrs [1..tpltArity tplt]) $ addTplt g
  where newNode = head $ newNodes 1 g
        addMbrs []     g = g
        addMbrs (p:ps) g = addMbrs ps $ insEdge
          (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
        addTplt = insEdge (newNode, template, RelEdge TpltRole)
                  . insNode (newNode, Rel) :: RSLT -> RSLT

insRelSum :: Node -> [Node] -> RSLT -> Either DwtErrSum RSLT
insRelSum template mbrs g =
  do mapM_ (gelemMSum g) $ template:mbrs
     tplt <- tpltAtSum g template
     mbrListMatchesTpltAritySum mbrs tplt
     return $ addMbrs (zip mbrs [1..tpltArity tplt]) $ addTplt g
  where newNode = head $ newNodes 1 g
        addMbrs []     g = g
        addMbrs (p:ps) g = addMbrs ps $ insEdge
          (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
        addTplt = insEdge (newNode, template, RelEdge TpltRole)
                  . insNode (newNode, Rel) :: RSLT -> RSLT

insRelStDeprecatoryName :: Node -> [Node] -> StateT RSLT (Either DwtErrDeprecatoryName) Node
insRelStDeprecatoryName template mbrs =
  do g <- get
     let newNode = head $ newNodes 1 g
         addMbrs []     g = g
         addMbrs (p:ps) g = addMbrs ps $ insEdge
           (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
         addTplt = insEdge (newNode, template, RelEdge TpltRole)
           . insNode (newNode, Rel) :: RSLT -> RSLT
     lift $ mapM_ (gelemMDeprecatoryName g) $ template:mbrs
     tplt <- tpltAtDeprecatoryName g template
     mbrListMatchesTpltArityDeprecatoryName mbrs tplt
     modify $ addMbrs (zip mbrs [1..tpltArity tplt]) . addTplt
     g' <- get
     return $ maxNode g'

insRelStSum :: Node -> [Node] -> StateT RSLT (Either DwtErrSum) Node
insRelStSum template mbrs =
  do g <- get
     let newNode = head $ newNodes 1 g
         addMbrs []     g = g
         addMbrs (p:ps) g = addMbrs ps $ insEdge
           (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
         addTplt = insEdge (newNode, template, RelEdge TpltRole)
           . insNode (newNode, Rel) :: RSLT -> RSLT
     lift $ mapM_ (gelemMSum g) $ template:mbrs
     tplt <- tpltAtSum g template
     mbrListMatchesTpltAritySum mbrs tplt
     modify $ addMbrs (zip mbrs [1..tpltArity tplt]) . addTplt
     g' <- get
     return $ maxNode g'

-- | Deprecated
insRelUsf :: Node -> [Node] -> RSLT -> RSLT
insRelUsf t ns g = case insRelDeprecatoryName t ns g of
  Left s -> error $ show s
  Right r -> r

insColl :: (MonadError String m)
        => (Maybe Node) -- title|principle, e.g. "alternatives"
        -> [Node] -> RSLT -> m RSLT
insColl mt ns g = do
  mapM_ (gelemMStrErr g) ns
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

insRelSpecDeprecatoryName :: RelSpec -> RSLT -> Either DwtErrDeprecatoryName RSLT
insRelSpecDeprecatoryName rSpec g = do
  let (varMap, nodeMap) = partitionRelSpec rSpec
      newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelSpecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemMDeprecatoryName g) $ Map.elems nodeMap
  let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                $ Map.toList nodeMap
        -- these edges specify the addressed nodes
  return $ insEdges newLEdges $ insNode newLNode g

insRelSpecSum :: RelSpec -> RSLT -> Either DwtErrSum RSLT
insRelSpecSum rSpec g = do
  let (varMap, nodeMap) = partitionRelSpec rSpec
      newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelSpecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemMSum g) $ Map.elems nodeMap
  let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                $ Map.toList nodeMap
        -- these edges specify the addressed nodes
  return $ insEdges newLEdges $ insNode newLNode g

relNodeSpecDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName RelNodeSpec
relNodeSpecDeprecatoryName g n = prependCallerDeprecatoryName "relNodeSpecDeprecatoryName: " $ do
  gelemMDeprecatoryName g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
    Just _ -> Left
      (NotRelSpecExpr, mNode .~ Just n $ noErrOpts, "")
    Nothing -> Left (FoundNo, mNode .~ Just n $ noErrOpts, "")

relNodeSpecSum :: RSLT -> Node -> Either DwtErrSum RelNodeSpec
relNodeSpecSum g n = prependCallerSum "relNodeSpecDeprecatoryName: " $ do
  gelemMSum g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
    Just _ -> Left
      (NotRelSpecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

relSpecDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName RelSpec
  -- name ? getRelSpecDe
  -- is nearly inverse to partitionRelSpec
relSpecDeprecatoryName g n = prependCallerDeprecatoryName "relSpecDeprecatoryName: " $ do
  gelemMDeprecatoryName g n
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      let rnsl = Map.toList $ fromRight $ relNodeSpecDeprecatoryName g n
          rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'

relSpecSum :: RSLT -> Node -> Either DwtErrSum RelSpec
  -- name ? getRelSpecDe
  -- is nearly inverse to partitionRelSpec
relSpecSum g n = prependCallerSum "relSpecDeprecatoryName: " $ do
  gelemMSum g n
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      let rnsl = Map.toList $ fromRight $ relNodeSpecDeprecatoryName g n
          rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'

-- ======== edit (but not insert)
chLeafDeprecatoryName :: RSLT -> Node -> Expr -> Either DwtErrDeprecatoryName RSLT
chLeafDeprecatoryName g n e' = prependCallerDeprecatoryName "chLeafDeprecatoryName: " $ do
  let me = lab g n
  case me of
    Just e@(isLeaf -> True) -> if areLikeExprs e e' then return ()
      else Left (ConstructorMistmatch, noErrOpts, ".")
    Nothing -> Left (FoundNo, mNode .~ Just n $ noErrOpts, ".")
    _       -> Left (NotLeaf, mNode .~ Just n $ noErrOpts, ".")
  return $ _chLeafUsf g n e'

chLeafSum :: RSLT -> Node -> Expr -> Either DwtErrSum RSLT
chLeafSum g n e' = prependCallerSum "chLeafDeprecatoryName: " $ do
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

chRelRoleDeprecatoryName :: RSLT -> Node -> Node -> RelRole -> Either DwtErrDeprecatoryName RSLT
chRelRoleDeprecatoryName g user newMbr role = do
  isRelMDeprecatoryName g user
  gelemMDeprecatoryName g newMbr
  let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      err = (Invalid, mNode .~ Just user $ mRelRole .~ Just role
              $ noErrOpts, "chRelRoleDeprecatoryName.")
  case candidates of
    [] -> Left $ _1 .~ FoundNo $ err
    [a] -> return $ delLEdge (user,a,RelEdge role)
           $ insEdge (user,newMbr,RelEdge role) g
    _ -> Left $ _1 .~ FoundMany $ err

chRelRoleSum :: RSLT -> Node -> Node -> RelRole -> Either DwtErrSum RSLT
chRelRoleSum g user newMbr role = do
  isRelMSum g user
  gelemMSum g newMbr
  let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      err = (Invalid, [ErrNode user, ErrRelRole role], "chRelRoleDeprecatoryName.")
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

tpltAtDeprecatoryName :: (MonadError DwtErrDeprecatoryName m) => RSLT -> Node -> m Expr
tpltAtDeprecatoryName g tn = let name = "tpltAtDeprecatoryName." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, mNode .~ Just tn $ noErrOpts, name)
  _       -> throwError (NotTplt, mNode .~ Just tn $ noErrOpts, name)

tpltAtSum :: (MonadError DwtErrSum m) => RSLT -> Node -> m Expr
tpltAtSum g tn = let name = "tpltAtDeprecatoryName." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, [ErrNode tn], name)
  _       -> throwError (NotTplt, [ErrNode tn], name)

-- todo: add prependCallerDeprecatoryName
relEltsDeprecatoryName :: RSLT -> Node -> [RelRole] -> Either DwtErrDeprecatoryName [Node]
relEltsDeprecatoryName g relNode roles = do
  isRelMDeprecatoryName g relNode
  mapM_  (validRoleDeprecatoryName g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

-- todo: add prependCallerDeprecatoryName
relEltsSum :: RSLT -> Node -> [RelRole] -> Either DwtErrSum [Node]
relEltsSum g relNode roles = do
  isRelMSum g relNode
  mapM_  (validRoleSum g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

-- todo: add prependCallerDeprecatoryName
validRoleDeprecatoryName :: RSLT -> Node -> RelRole -> Either DwtErrDeprecatoryName ()
validRoleDeprecatoryName g relNode role = isRelMDeprecatoryName g relNode >> case role of
  TpltRole -> return ()
  Mbr p -> do
    if p >= 1 then return () else Left err
    t <- relTpltDeprecatoryName g relNode
    let a = tpltArity t
    if p <= a then return ()
      else Left $ _1 .~ ArityMismatch $ _2 . mExpr .~ Just t $ err
  where err = (Invalid, mRelRole .~ Just role $ noErrOpts, "validRoleStrErr.")

validRoleSum :: RSLT -> Node -> RelRole -> Either DwtErrSum ()
validRoleSum g relNode role = isRelMSum g relNode >> case role of
  TpltRole -> return ()
  Mbr p -> do
    if p >= 1 then return () else Left err
    t <- relTpltSum g relNode
    let a = tpltArity t
    if p <= a then return ()
      else Left $ _1 .~ ArityMismatch $ _2 %~ (ErrExpr t:) $ err
  where err = (Invalid, [ErrRelRole role], "validRoleStrErr.")

-- todo: add prependCallerDeprecatoryName
relTpltDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName Expr -- unsafe
  -- might not be called on a template
relTpltDeprecatoryName g relNode = do
  [n] <- relEltsDeprecatoryName g relNode [TpltRole]
  return $ fromJust $ lab g n

relTpltSum :: RSLT -> Node -> Either DwtErrSum Expr -- unsafe
  -- might not be called on a template
relTpltSum g relNode = do
  [n] <- relEltsSum g relNode [TpltRole]
  return $ fromJust $ lab g n

-- todo : change to DwtErrDeprecatoryName
collPrincipleDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName Expr
  -- analogous to relTpltDeprecatoryName
collPrincipleDeprecatoryName g collNode = do
  prependCallerDeprecatoryName "collPrincipleDe: " $ isCollMDeprecatoryName g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

collPrincipleSum :: RSLT -> Node -> Either DwtErrSum Expr
  -- analogous to relTpltDeprecatoryName
collPrincipleSum g collNode = do
  prependCallerSum "collPrincipleDe: " $ isCollMSum g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

-- ==== .. -> [Node]
rels :: Gr Expr b -> [Node]
rels = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False)

-- opposites: mbrs, usersDeprecatoryName
  -- though they would not be if Tplts pointed to|had members of their own
mbrs :: RSLT -> Node -> [Node]
mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
  where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

-- Words and Tplts are used, but are not usersDeprecatoryName. (Rels and Colls use them.)
usersDeprecatoryName :: Graph gr => gr a b -> Node -> Either DwtErrDeprecatoryName [Node]
usersDeprecatoryName g n = do gelemMDeprecatoryName g n
                              return [m | (m,label@_) <- lpre g n]

usersSum :: Graph gr => gr a b -> Node -> Either DwtErrSum [Node]
usersSum g n = do gelemMSum g n
                  return [m | (m,label@_) <- lpre g n]

-- | Rels using Node n in RelRole r
usersInRoleDeprecatoryName :: RSLT -> Node -> RelRole -> Either DwtErrDeprecatoryName [Node]
usersInRoleDeprecatoryName g n r = prependCallerDeprecatoryName "usersInRoleDeprecatoryName: " $
  do gelemMDeprecatoryName g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

usersInRoleSum :: RSLT -> Node -> RelRole -> Either DwtErrSum [Node]
usersInRoleSum g n r = prependCallerSum "usersInRoleDeprecatoryName: " $
  do gelemMSum g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

matchRelDeprecatoryName :: RSLT -> RelSpec -> Either DwtErrDeprecatoryName [Node]
matchRelDeprecatoryName g spec = prependCallerDeprecatoryName "matchRelDeprecatoryName: " $ do
  let specList = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,AddressOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRoleDeprecatoryName g n r) specList
  return $ listIntersect nodeListList

matchRelSum :: RSLT -> RelSpec -> Either DwtErrSum [Node]
matchRelSum g spec = prependCallerSum "matchRelDeprecatoryName: " $ do
  let specList = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,AddressOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRoleSum g n r) specList
  return $ listIntersect nodeListList

matchRelLabDeprecatoryName :: RSLT -> RelSpec -> Either DwtErrDeprecatoryName [LNode Expr]
matchRelLabDeprecatoryName g spec = prependCallerDeprecatoryName "matchRelLabDeprecatoryName: " $ do
  ns <- matchRelDeprecatoryName g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe here, because matchRelStrErr only returns Nodes in g

matchRelLabSum :: RSLT -> RelSpec -> Either DwtErrSum [LNode Expr]
matchRelLabSum g spec = prependCallerSum "matchRelLabDeprecatoryName: " $ do
  ns <- matchRelSum g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe here, because matchRelStrErr only returns Nodes in g

-- >>>
-- ======== using directions (RelSpecs)
-- todo ? 1Dir because it should have one such direction. I forget why.
  -- clarif: if I want a generation in the Down direction of the rel "has/",
  -- the RelSpec has to have only one Up variable.
-- TODO ? check: Up|Down good, Any|It bad
  -- fork1Up uses otherDir, so it will catch those errors, but obscurely

--    has1Up :: RelSpec -> Bool
--    has1Up rc = length as == 1
--      where as = Map.toList
--               $ Map.filter (\x -> case x of
--                              VarSpec Up -> True; _ -> False)
--               rc

has1Dir :: Mbrship -> RelSpec -> Bool
has1Dir mv rc = length as == 1
  where as = Map.toList
           $ Map.filter (\x -> case x of VarSpec y -> y==mv; _ -> False)
           rc

otherDir :: Mbrship -> Mbrship -- incomplete; non-invertible cases will err
otherDir Up = Down
otherDir Down = Up

fork1Dir:: RSLT -> Node -> (Mbrship,RelSpec) -> Either DwtErrDeprecatoryName [Node]
fork1Dir g n (dir,r) = do -- returns one generation, neighbors
  if has1Dir (otherDir dir) r then return ()
     else Left (Invalid,  mRelSpec .~ Just r $ noErrOpts
               ,  "should have only one " ++ show (otherDir dir))
  let r' = subNodeForVars n (otherDir dir) r
      dirRoles = Map.keys $ Map.filter (== VarSpec dir) r
  rels <- matchRelDeprecatoryName g r'
  concat <$> mapM (\rel -> relEltsDeprecatoryName g rel dirRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
    -- EXCEPT: that might hurt the dfs, bfs functions below

fork1DirSumDeprecatoryName:: RSLT -> Node -> (Mbrship,RelSpec) -> Either DwtErrSum [Node]
fork1DirSumDeprecatoryName g n (dir,r) = do -- returns one generation, neighbors
  if has1Dir (otherDir dir) r then return ()
     else Left (Invalid,  [ErrRelSpec r]
               , "fork1DirSumDeprecatoryName: should have only one " ++ show (otherDir dir))
  let r' = subNodeForVars n (otherDir dir) r
      dirRoles = Map.keys $ Map.filter (== VarSpec dir) r
  rels <- matchRelSum g r'
  concat <$> mapM (\rel -> relEltsSum g rel dirRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
    -- EXCEPT: that might hurt the dfs, bfs functions below

fork1Dirs :: RSLT -> Node -> [(Mbrship,RelSpec)] -> Either DwtErrDeprecatoryName [Node]
fork1Dirs g n rs = concat <$> mapM (fork1Dir g n) rs

fork1DirsSumDeprecatoryName :: RSLT -> Node -> [(Mbrship,RelSpec)] -> Either DwtErrSum [Node]
fork1DirsSumDeprecatoryName g n rs = concat <$> mapM (fork1DirSumDeprecatoryName g n) rs

subNodeForVars :: Node -> Mbrship -> RelSpec  -> RelSpec
subNodeForVars n v r = Map.map -- change each VarSpec v to NodeSpec n
  (\x -> case x of VarSpec v' -> if v==v' then NodeSpec n else VarSpec v'
                   _          -> x   -- yes, the v,v' distinction is needed
  ) r

-- ==== dfs and bfs
  -- algorithmically, the difference is only newNodes++ns v. ns++newNodes
_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- | determines dfs|bfs
  -> RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> Either DwtErrDeprecatoryName [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g dir pending@(n:ns) acc = do
  newNodes <- fork1Dir g n dir -- ifdo speed: calls has1Dir redundantly
  _bfsOrDfs collector g dir (nub $ collector newNodes ns) (n:acc)
    -- ifdo speed: discard visited nodes from graph

_bfsOrDfsSumDeprecatoryName :: ([Node] -> [Node] -> [Node]) -- | determines dfs|bfs
  -> RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> Either DwtErrSum [Node]
_bfsOrDfsSumDeprecatoryName _ _ _ [] acc = return acc
_bfsOrDfsSumDeprecatoryName collector g dir pending@(n:ns) acc = do
  newNodes <- fork1DirSumDeprecatoryName g n dir -- ifdo speed: calls has1Dir redundantly
  _bfsOrDfsSumDeprecatoryName collector g dir (nub $ collector newNodes ns) (n:acc)
    -- ifdo speed: discard visited nodes from graph

_dwtBfs = _bfsOrDfs (\new old -> old ++ new)
_dwtDfs = _bfsOrDfs (\new old -> new ++ old)

_dwtBfsSumDeprecatoryName = _bfsOrDfsSumDeprecatoryName (\new old -> old ++ new)
_dwtDfsSumDeprecatoryName = _bfsOrDfsSumDeprecatoryName (\new old -> new ++ old)

dwtDfs :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either DwtErrDeprecatoryName [Node]
dwtDfs g dir starts = do mapM_ (gelemMDeprecatoryName g) $ starts
                         (nub . reverse) <$> _dwtDfs g dir starts []

dwtBfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either DwtErrDeprecatoryName [Node]
dwtBfs g dir starts = do mapM_ (gelemMDeprecatoryName g) $ starts
                         (nub . reverse) <$> _dwtBfs g dir starts []

dwtDfsSumDeprecatoryName :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either DwtErrSum [Node]
dwtDfsSumDeprecatoryName g dir starts = do mapM_ (gelemMSum g) $ starts
                                           (nub . reverse) <$> _dwtDfsSumDeprecatoryName g dir starts []

dwtBfsSumDeprecatoryName :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either DwtErrSum [Node]
dwtBfsSumDeprecatoryName g dir starts = do mapM_ (gelemMSum g) $ starts
                                           (nub . reverse) <$> _dwtBfsSumDeprecatoryName g dir starts []

