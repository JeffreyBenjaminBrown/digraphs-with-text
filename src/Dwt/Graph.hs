{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Dwt.Graph (
  insRelUsf
  , insRelLongErr, insRelSum, insRelStLongErr, insRelStSum
  , insColl
  , mkRelSpec, partitionRelSpec, insRelSpecLongErr, insRelSpecSum
  , relNodeSpecLongErr, relNodeSpecSum, relSpecLongErr, relSpecSum
  , chLeafLongErr, chLeafSum, chRelRoleLongErr, chRelRoleSum
  , whereis, tpltAtLongErr, tpltAtSum
  , relEltsLongErr, validRoleLongErr, relTpltLongErr
  , collPrincipleLongErr, collPrincipleSum
  , rels, mbrs, usersLongErr, usersSum, usersInRoleLongErr, usersInRoleSum
  , matchRelLongErr, matchRelSum, matchRelLabLongErr, matchRelLabSum
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
insRelLongErr :: Node -> [Node] -> RSLT -> Either DwtErrLongErr RSLT
insRelLongErr template mbrs g =
  do mapM_ (gelemMLongErr g) $ template:mbrs
     tplt <- tpltAtLongErr g template
     mbrListMatchesTpltArityLongErr mbrs tplt
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

insRelStLongErr :: Node -> [Node] -> StateT RSLT (Either DwtErrLongErr) Node
insRelStLongErr template mbrs =
  do g <- get
     let newNode = head $ newNodes 1 g
         addMbrs []     g = g
         addMbrs (p:ps) g = addMbrs ps $ insEdge
           (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
         addTplt = insEdge (newNode, template, RelEdge TpltRole)
           . insNode (newNode, Rel) :: RSLT -> RSLT
     lift $ mapM_ (gelemMLongErr g) $ template:mbrs
     tplt <- tpltAtLongErr g template
     mbrListMatchesTpltArityLongErr mbrs tplt
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
insRelUsf t ns g = case insRelLongErr t ns g of
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

insRelSpecLongErr :: RelSpec -> RSLT -> Either DwtErrLongErr RSLT
insRelSpecLongErr rSpec g = do
  let (varMap, nodeMap) = partitionRelSpec rSpec
      newAddr = head $ newNodes 1 g
      newLNode = (newAddr, RelSpecExpr varMap)
        -- this node specifies the variable nodes
  mapM_ (gelemMLongErr g) $ Map.elems nodeMap
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

relNodeSpecLongErr :: RSLT -> Node -> Either DwtErrLongErr RelNodeSpec
relNodeSpecLongErr g n = prependCallerLongErr "relNodeSpecLongErr: " $ do
  gelemMLongErr g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
    Just _ -> Left
      (NotRelSpecExpr, mNode .~ Just n $ noErrOpts, "")
    Nothing -> Left (FoundNo, mNode .~ Just n $ noErrOpts, "")

relNodeSpecSum :: RSLT -> Node -> Either DwtErrSum RelNodeSpec
relNodeSpecSum g n = prependCallerSum "relNodeSpecLongErr: " $ do
  gelemMSum g n
  case lab g n of
    Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
      where f (node,RelEdge r) = (r,node)
    Just _ -> Left
      (NotRelSpecExpr, [ErrNode n], "")
    Nothing -> Left (FoundNo, [ErrNode n], "")

relSpecLongErr :: RSLT -> Node -> Either DwtErrLongErr RelSpec
  -- name ? getRelSpecDe
  -- is nearly inverse to partitionRelSpec
relSpecLongErr g n = prependCallerLongErr "relSpecLongErr: " $ do
  gelemMLongErr g n
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      let rnsl = Map.toList $ fromRight $ relNodeSpecLongErr g n
          rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'

relSpecSum :: RSLT -> Node -> Either DwtErrSum RelSpec
  -- name ? getRelSpecDe
  -- is nearly inverse to partitionRelSpec
relSpecSum g n = prependCallerSum "relSpecLongErr: " $ do
  gelemMSum g n
  case (fromJust $ lab g n) of
    RelSpecExpr rvs -> do
      let rnsl = Map.toList $ fromRight $ relNodeSpecLongErr g n
          rvsl = Map.toList rvs
          rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
          rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
      return $ Map.fromList $ rvsl' ++ rnsl'

-- ======== edit (but not insert)
chLeafLongErr :: RSLT -> Node -> Expr -> Either DwtErrLongErr RSLT
chLeafLongErr g n e' = prependCallerLongErr "chLeafLongErr: " $ do
  let me = lab g n
  case me of
    Just e@(isLeaf -> True) -> if areLikeExprs e e' then return ()
      else Left (ConstructorMistmatch, noErrOpts, ".")
    Nothing -> Left (FoundNo, mNode .~ Just n $ noErrOpts, ".")
    _       -> Left (NotLeaf, mNode .~ Just n $ noErrOpts, ".")
  return $ _chLeafUsf g n e'

chLeafSum :: RSLT -> Node -> Expr -> Either DwtErrSum RSLT
chLeafSum g n e' = prependCallerSum "chLeafLongErr: " $ do
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

chRelRoleLongErr :: RSLT -> Node -> Node -> RelRole -> Either DwtErrLongErr RSLT
chRelRoleLongErr g user newMbr role = do
  isRelMLongErr g user
  gelemMLongErr g newMbr
  let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      err = (Invalid, mNode .~ Just user $ mRelRole .~ Just role
              $ noErrOpts, "chRelRoleLongErr.")
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
      err = (Invalid, [ErrNode user, ErrRelRole role], "chRelRoleLongErr.")
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

tpltAtLongErr :: (MonadError DwtErrLongErr m) => RSLT -> Node -> m Expr
tpltAtLongErr g tn = let name = "tpltAtLongErr." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, mNode .~ Just tn $ noErrOpts, name)
  _       -> throwError (NotTplt, mNode .~ Just tn $ noErrOpts, name)

tpltAtSum :: (MonadError DwtErrSum m) => RSLT -> Node -> m Expr
tpltAtSum g tn = let name = "tpltAtLongErr." in case lab g tn of
  Just t@(Tplt _) -> return t
  Nothing -> throwError (FoundNo, [ErrNode tn], name)
  _       -> throwError (NotTplt, [ErrNode tn], name)

-- todo: add prependCallerLongErr
relEltsLongErr :: RSLT -> Node -> [RelRole] -> Either DwtErrLongErr [Node]
relEltsLongErr g relNode roles = do
  isRelMLongErr g relNode
  mapM_  (validRoleLongErr g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

-- todo: add prependCallerLongErr
relEltsSum :: RSLT -> Node -> [RelRole] -> Either DwtErrSum [Node]
relEltsSum g relNode roles = do
  isRelMSum g relNode
  mapM_  (validRoleSum g relNode) roles
  return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

-- todo: add prependCallerLongErr
validRoleLongErr :: RSLT -> Node -> RelRole -> Either DwtErrLongErr ()
validRoleLongErr g relNode role = isRelMLongErr g relNode >> case role of
  TpltRole -> return ()
  Mbr p -> do
    if p >= 1 then return () else Left err
    t <- relTpltLongErr g relNode
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

-- todo: add prependCallerLongErr
relTpltLongErr :: RSLT -> Node -> Either DwtErrLongErr Expr -- unsafe
  -- might not be called on a template
relTpltLongErr g relNode = do
  [n] <- relEltsLongErr g relNode [TpltRole]
  return $ fromJust $ lab g n

relTpltSum :: RSLT -> Node -> Either DwtErrSum Expr -- unsafe
  -- might not be called on a template
relTpltSum g relNode = do
  [n] <- relEltsSum g relNode [TpltRole]
  return $ fromJust $ lab g n

-- todo : change to DwtErrLongErr
collPrincipleLongErr :: RSLT -> Node -> Either DwtErrLongErr Expr
  -- analogous to relTpltLongErr
collPrincipleLongErr g collNode = do
  prependCallerLongErr "collPrincipleDe: " $ isCollMLongErr g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

collPrincipleSum :: RSLT -> Node -> Either DwtErrSum Expr
  -- analogous to relTpltLongErr
collPrincipleSum g collNode = do
  prependCallerSum "collPrincipleDe: " $ isCollMSum g collNode
  return $ fromJust $ lab g $ head
    [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

-- ==== .. -> [Node]
rels :: Gr Expr b -> [Node]
rels = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False)

-- opposites: mbrs, usersLongErr
  -- though they would not be if Tplts pointed to|had members of their own
mbrs :: RSLT -> Node -> [Node]
mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
  where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

-- Words and Tplts are used, but are not usersLongErr. (Rels and Colls use them.)
usersLongErr :: Graph gr => gr a b -> Node -> Either DwtErrLongErr [Node]
usersLongErr g n = do gelemMLongErr g n
                      return [m | (m,label@_) <- lpre g n]

usersSum :: Graph gr => gr a b -> Node -> Either DwtErrSum [Node]
usersSum g n = do gelemMSum g n
                  return [m | (m,label@_) <- lpre g n]

-- | Rels using Node n in RelRole r
usersInRoleLongErr :: RSLT -> Node -> RelRole -> Either DwtErrLongErr [Node]
usersInRoleLongErr g n r = prependCallerLongErr "usersInRoleLongErr: " $
  do gelemMLongErr g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

usersInRoleSum :: RSLT -> Node -> RelRole -> Either DwtErrSum [Node]
usersInRoleSum g n r = prependCallerSum "usersInRoleLongErr: " $
  do gelemMSum g n -- makes f safe
     return $ f g n r
  where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
        f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

matchRelLongErr :: RSLT -> RelSpec -> Either DwtErrLongErr [Node]
matchRelLongErr g spec = prependCallerLongErr "matchRelLongErr: " $ do
  let specList = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,AddressOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRoleLongErr g n r) specList
  return $ listIntersect nodeListList

matchRelSum :: RSLT -> RelSpec -> Either DwtErrSum [Node]
matchRelSum g spec = prependCallerSum "matchRelLongErr: " $ do
  let specList = Map.toList
        $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False)
        $ spec :: [(RelRole,AddressOrVar)]
  nodeListList <- mapM (\(r,NodeSpec n) -> usersInRoleSum g n r) specList
  return $ listIntersect nodeListList

matchRelLabLongErr :: RSLT -> RelSpec -> Either DwtErrLongErr [LNode Expr]
matchRelLabLongErr g spec = prependCallerLongErr "matchRelLabLongErr: " $ do
  ns <- matchRelLongErr g spec
  return $ zip ns $ map (fromJust . lab g) ns
    -- fromJust is safe here, because matchRelStrErr only returns Nodes in g

matchRelLabSum :: RSLT -> RelSpec -> Either DwtErrSum [LNode Expr]
matchRelLabSum g spec = prependCallerSum "matchRelLabLongErr: " $ do
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

fork1Dir:: RSLT -> Node -> (Mbrship,RelSpec) -> Either DwtErrLongErr [Node]
fork1Dir g n (dir,r) = do -- returns one generation, neighbors
  if has1Dir (otherDir dir) r then return ()
     else Left (Invalid,  mRelSpec .~ Just r $ noErrOpts
               ,  "should have only one " ++ show (otherDir dir))
  let r' = subNodeForVars n (otherDir dir) r
      dirRoles = Map.keys $ Map.filter (== VarSpec dir) r
  rels <- matchRelLongErr g r'
  concat <$> mapM (\rel -> relEltsLongErr g rel dirRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
    -- EXCEPT: that might hurt the dfs, bfs functions below

fork1DirSumLongErr:: RSLT -> Node -> (Mbrship,RelSpec) -> Either DwtErrSum [Node]
fork1DirSumLongErr g n (dir,r) = do -- returns one generation, neighbors
  if has1Dir (otherDir dir) r then return ()
     else Left (Invalid,  [ErrRelSpec r]
               , "fork1DirSumLongErr: should have only one " ++ show (otherDir dir))
  let r' = subNodeForVars n (otherDir dir) r
      dirRoles = Map.keys $ Map.filter (== VarSpec dir) r
  rels <- matchRelSum g r'
  concat <$> mapM (\rel -> relEltsSum g rel dirRoles) rels
    -- TODO: this line is unnecessary. just return the rels, not their elts.
    -- EXCEPT: that might hurt the dfs, bfs functions below

fork1Dirs :: RSLT -> Node -> [(Mbrship,RelSpec)] -> Either DwtErrLongErr [Node]
fork1Dirs g n rs = concat <$> mapM (fork1Dir g n) rs

fork1DirsSumLongErr :: RSLT -> Node -> [(Mbrship,RelSpec)] -> Either DwtErrSum [Node]
fork1DirsSumLongErr g n rs = concat <$> mapM (fork1DirSumLongErr g n) rs

subNodeForVars :: Node -> Mbrship -> RelSpec  -> RelSpec
subNodeForVars n v r = Map.map -- change each VarSpec v to NodeSpec n
  (\x -> case x of VarSpec v' -> if v==v' then NodeSpec n else VarSpec v'
                   _          -> x   -- yes, the v,v' distinction is needed
  ) r

-- ==== dfs and bfs
  -- algorithmically, the difference is only newNodes++ns v. ns++newNodes
_bfsOrDfs :: ([Node] -> [Node] -> [Node]) -- | determines dfs|bfs
  -> RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> Either DwtErrLongErr [Node]
_bfsOrDfs _ _ _ [] acc = return acc
_bfsOrDfs collector g dir pending@(n:ns) acc = do
  newNodes <- fork1Dir g n dir -- ifdo speed: calls has1Dir redundantly
  _bfsOrDfs collector g dir (nub $ collector newNodes ns) (n:acc)
    -- ifdo speed: discard visited nodes from graph

_bfsOrDfsSumLongErr :: ([Node] -> [Node] -> [Node]) -- | determines dfs|bfs
  -> RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> Either DwtErrSum [Node]
_bfsOrDfsSumLongErr _ _ _ [] acc = return acc
_bfsOrDfsSumLongErr collector g dir pending@(n:ns) acc = do
  newNodes <- fork1DirSumLongErr g n dir -- ifdo speed: calls has1Dir redundantly
  _bfsOrDfsSumLongErr collector g dir (nub $ collector newNodes ns) (n:acc)
    -- ifdo speed: discard visited nodes from graph

_dwtBfs = _bfsOrDfs (\new old -> old ++ new)
_dwtDfs = _bfsOrDfs (\new old -> new ++ old)

_dwtBfsSumLongErr = _bfsOrDfsSumLongErr (\new old -> old ++ new)
_dwtDfsSumLongErr = _bfsOrDfsSumLongErr (\new old -> new ++ old)

dwtDfs :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either DwtErrLongErr [Node]
dwtDfs g dir starts = do mapM_ (gelemMLongErr g) $ starts
                         (nub . reverse) <$> _dwtDfs g dir starts []

dwtBfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either DwtErrLongErr [Node]
dwtBfs g dir starts = do mapM_ (gelemMLongErr g) $ starts
                         (nub . reverse) <$> _dwtBfs g dir starts []

dwtDfsSumLongErr :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either DwtErrSum [Node]
dwtDfsSumLongErr g dir starts = do mapM_ (gelemMSum g) $ starts
                                   (nub . reverse) <$> _dwtDfsSumLongErr g dir starts []

dwtBfsSumLongErr :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either DwtErrSum [Node]
dwtBfsSumLongErr g dir starts = do mapM_ (gelemMSum g) $ starts
                                   (nub . reverse) <$> _dwtBfsSumLongErr g dir starts []

