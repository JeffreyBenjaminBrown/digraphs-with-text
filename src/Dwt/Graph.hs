    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Graph (
      insRel, insRelDe, insRelDeSt, insRelUsf, insColl
      , mkRelSpec, partitionRelSpec, insRelSpec, insRelSpecDe
      , relNodeSpec, relNodeSpecDe, relSpec, relSpecDe
      , chLeaf, chLeafDe, chRelRole
      , whereis, tpltAt, tpltAtDe
      , relElts, relEltsDe, validRole, validRoleDe, relTplt, relTpltDe
      , collPrinciple
      , rels, mbrs, users, usersDe, usersInRole, usersInRoleDe
      , matchRel, matchRelDe, matchRelLab, matchRelLabDe
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

-- build
    insRel :: Node -> [Node] -> RSLT -> Either String RSLT
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

    insRelDe :: Node -> [Node] -> RSLT -> Either DwtErr RSLT
    insRelDe template mbrs g =
      do mapM_ (gelemMDe g) $ template:mbrs
         tplt <- tpltAtDe g template
         mbrListMatchesTpltArityDe mbrs tplt
         return $ addMbrs (zip mbrs [1..tpltArity tplt]) $ addTplt g
      where newNode = head $ newNodes 1 g
            addMbrs []     g = g
            addMbrs (p:ps) g = addMbrs ps $ insEdge
              (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
            addTplt = insEdge (newNode, template, RelEdge TpltRole)
                      . insNode (newNode, Rel) :: RSLT -> RSLT

    insRelDeSt :: Node -> [Node] -> StateT RSLT (Either DwtErr) Node
    insRelDeSt template mbrs =
      do g <- get
         let newNode = head $ newNodes 1 g
             addMbrs []     g = g
             addMbrs (p:ps) g = addMbrs ps $ insEdge
               (newNode, fst p, RelEdge $ Mbr $ snd p) g :: RSLT
             addTplt = insEdge (newNode, template, RelEdge TpltRole)
               . insNode (newNode, Rel) :: RSLT -> RSLT
         lift $ mapM_ (gelemMDe g) $ template:mbrs
         tplt <- tpltAtDe g template
         mbrListMatchesTpltArityDe mbrs tplt
         modify $ addMbrs (zip mbrs [1..tpltArity tplt]) . addTplt
         g' <- get
         return $ maxNode g'

    insRelUsf :: Node -> [Node] -> RSLT -> RSLT
    insRelUsf t ns g = case insRel t ns g of
      Left s -> error s
      Right r -> r

    insColl :: (MonadError String m)
            => (Maybe Node) -- title|principle, e.g. "alternatives"
            -> [Node] -> RSLT -> m RSLT
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

    insRelSpec :: (MonadError String m) => RelSpec -> RSLT -> m RSLT
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

    insRelSpecDe :: RelSpec -> RSLT -> Either DwtErr RSLT
    insRelSpecDe rSpec g = do
      let (varMap, nodeMap) = partitionRelSpec rSpec
          newAddr = head $ newNodes 1 g
          newLNode = (newAddr, RelSpecExpr varMap)
            -- this node specifies the variable nodes
      mapM_ (gelemMDe g) $ Map.elems nodeMap
      let newLEdges = map (\(role,n) -> (newAddr, n, RelEdge role))
                    $ Map.toList nodeMap
            -- these edges specify the addressed nodes
      return $ insEdges newLEdges $ insNode newLNode g

    relNodeSpec :: (MonadError String m) => RSLT -> Node -> m RelNodeSpec
      -- name ? getRelNodeSpec
    relNodeSpec g n = do
      gelemM g n
      case (fromJust $ lab g n) of
        RelSpecExpr _ -> return $ Map.fromList $ map f $ lsuc g n
          where f (node,RelEdge r) = (r,node)
        _ -> throwError $ "Node " ++ show n ++ " not a RelSpecExpr."

    relNodeSpecDe :: RSLT -> Node -> Either DwtErr RelNodeSpec
    relNodeSpecDe g n = prependCaller "relNodeSpecDe: " $ do
      gelemMDe g n
      case lab g n of
        Just (RelSpecExpr _) -> return $ Map.fromList $ map f $ lsuc g n
          where f (node,RelEdge r) = (r,node)
        Just _ -> Left
          (NotRelSpecExpr, mNode .~ Just n $ noErrOpts, "")
        Nothing -> Left (FoundNo, mNode .~ Just n $ noErrOpts, "")

    relSpec :: RSLT -> Node -> Either String RelSpec
      -- name ? getRelSpec
    relSpec g n = do -- nearly inverse to partitionRelSpec
      gelemM g n
      case (fromJust $ lab g n) of
        RelSpecExpr rvs -> do
          let rnsl = Map.toList $ fromRight $ relNodeSpec g n
              rvsl = Map.toList rvs
              rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
              rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
          return $ Map.fromList $ rvsl' ++ rnsl'

    relSpecDe :: RSLT -> Node -> Either DwtErr RelSpec
      -- name ? getRelSpecDe
      -- is nearly inverse to partitionRelSpec
    relSpecDe g n = prependCaller "relSpecDe: " $ do
      gelemMDe g n
      case (fromJust $ lab g n) of
        RelSpecExpr rvs -> do
          let rnsl = Map.toList $ fromRight $ relNodeSpec g n
              rvsl = Map.toList rvs
              rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
              rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
          return $ Map.fromList $ rvsl' ++ rnsl'

  -- edit (but not insert)
    chLeaf :: (MonadError String m) => RSLT -> Node -> Expr -> m RSLT
    chLeaf g n e' = do
      let me = lab g n
          mismatch = throwError $ "chLeaf: constructor mismatch"
      case me of
        Just e@(Word _)  -> if areLikeExprs e e' then return () else mismatch
        Just e@(Tplt _) -> if areLikeExprs e e' then return () else mismatch
        Nothing -> throwError $ "chLeaf: Node " ++ show n ++ " absent."
        _       -> throwError $ "chLeaf: Node " ++ show n ++ " is a user."
      return $ _chLeafUsf g n e'

    chLeafDe :: RSLT -> Node -> Expr -> Either DwtErr RSLT
    chLeafDe g n e' = prependCaller "chLeafDe: " $ do
      let me = lab g n
      case me of
        Just e@(isLeaf -> True) -> if areLikeExprs e e' then return ()
          else Left (ConstructorMistmatch, noErrOpts, ".")
        Nothing -> Left (FoundNo, mNode .~ Just n $ noErrOpts, ".")
        _       -> Left (NotLeaf, mNode .~ Just n $ noErrOpts, ".")
      return $ _chLeafUsf g n e'

    _chLeafUsf :: RSLT -> Node -> Expr -> RSLT
    _chLeafUsf g n newExpr = let (Just (a,b,c,d),g') = match n g
      in (a,b,newExpr,d) & g'

    chRelRole :: (MonadError String m) => 
      RSLT -> Node -> Node -> RelRole -> m RSLT
    chRelRole g user newMbr role = do
      isRelM g user `catchError` (\_ -> throwError $ 
        "chRelRole: Node " ++ show user ++ " absent or not a Rel.")
      gelemM g newMbr
      let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      if length candidates /= 1
        then throwError "chRelRole: invalid graph state, or MbrPos out of range"
        else return ()
      let oldMbr = head candidates
      return $ delLEdge (user,oldMbr,RelEdge role)
             $ insEdge (user,newMbr,RelEdge role) g

    chRelRoleDe :: RSLT -> Node -> Node -> RelRole -> Either DwtErr RSLT
    chRelRoleDe g user newMbr role = do
      isRelMDe g user
      gelemMDe g newMbr
      let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
          err = (Invalid, mNode .~ Just user $ mRelRole .~ Just role
                  $ noErrOpts, "chRelRoleDe.")
      case candidates of
        [] -> Left $ _1 .~ FoundNo $ err
        [a] -> return $ delLEdge (user,a,RelEdge role)
               $ insEdge (user,newMbr,RelEdge role) g
        _ -> Left $ _1 .~ FoundMany $ err

-- query
  -- more complex ("locate"?) queries
    whereis :: RSLT -> Expr -> [Node]
      -- TODO: dependent types. (hopefully, length = 1)
      -- move ? Search.hs
      -- name ? exprOf
    whereis g x = nodes $ labfilter (== x) g

    tpltAt :: (MonadError String m) => RSLT -> Node -> m Expr
    tpltAt g tn = case lab g tn of
      Just t@(Tplt _) -> return t
      Nothing -> throwError $ "tpltAt: Node " ++ show tn ++ " absent."
      _       -> throwError $ "tpltAt: LNode " ++ show tn ++ " not a Tplt."

    tpltAtDe :: (MonadError DwtErr m) => RSLT -> Node -> m Expr
    tpltAtDe g tn = let name = "tpltAtDe." in case lab g tn of
      Just t@(Tplt _) -> return t
      Nothing -> throwError (FoundNo, mNode .~ Just tn $ noErrOpts, name)
      _       -> throwError (NotTplt, mNode .~ Just tn $ noErrOpts, name)

    relElts :: RSLT -> Node -> [RelRole] -> Either String [Node]
    relElts g relNode roles = do
      isRelM g relNode `catchError` (\_ -> throwError $
        "relElts: Node " ++ show relNode ++ " absent or not a Rel.")
      mapM_  (validRole g relNode) roles `catchError` (\_ -> throwError $
        "relElts: at least one member out of bounds")
      return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

    relEltsDe :: RSLT -> Node -> [RelRole] -> Either DwtErr [Node]
    relEltsDe g relNode roles = do
      isRelMDe g relNode
      mapM_  (validRoleDe g relNode) roles
      return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

    validRole :: RSLT -> Node -> RelRole -> Either String ()
    validRole g relNode role = do
      isRelM g relNode `catchError` (\_ -> throwError 
        $ "validRole: Node " ++ show relNode ++ " absent or not a Rel.")
      case role of
        TpltRole -> return ()
        Mbr p -> do
          if p < 1 then throwError $ "validRole: MbrPos < 1" else return ()
          t <- relTplt g relNode
          let a = tpltArity t
          if p <= a then return ()
            else throwError $ "validRole: Arity " ++ show a ++ 
              " < MbrPos " ++ show p

    validRoleDe :: RSLT -> Node -> RelRole -> Either DwtErr ()
    validRoleDe g relNode role = isRelMDe g relNode >> case role of
      TpltRole -> return ()
      Mbr p -> do
        if p >= 1 then return () else Left err
        t <- relTpltDe g relNode
        let a = tpltArity t
        if p <= a then return ()
          else Left $ _1 .~ ArityMismatch $ _2 . mExpr .~ Just t $ err
      where err = (Invalid, mRelRole .~ Just role $ noErrOpts, "validRole.")

    relTplt :: RSLT -> Node -> Either String Expr -- unsafe
      -- might not be called on a template
    relTplt g relNode = do
      [n] <- relElts g relNode [TpltRole]
      return $ fromJust $ lab g n

    relTpltDe :: RSLT -> Node -> Either DwtErr Expr -- unsafe
      -- might not be called on a template
    relTpltDe g relNode = do
      [n] <- relEltsDe g relNode [TpltRole]
      return $ fromJust $ lab g n

    -- todo : change to DwtErr
    collPrinciple :: (MonadError String m) => RSLT -> Node -> m Expr
      -- analogous to relTplt
    collPrinciple g collNode = do
      isCollM g collNode `catchError` (\_ -> throwError $ 
        "collPrinciple: Node " ++ show collNode ++ " absent or not a Coll.")
      return $ fromJust $ lab g $ head
        [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

  -- .. -> [Node]
    rels :: Gr Expr b -> [Node]
    rels = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False) 

    -- opposites: mbrs, users
      -- though they would not be if Tplts pointed to|had members of their own
    mbrs :: RSLT -> Node -> [Node]
    mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
      where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

    -- Words and Tplts are used, but are not users. (Rels and Colls use them.)
    users :: (MonadError String m, Graph gr) => gr a b -> Node -> m [Node]
    users g n = do gelemM g n
                   return [m | (m,label@_) <- lpre g n]

    usersDe :: Graph gr => gr a b -> Node -> Either DwtErr [Node]
    usersDe g n = do gelemMDe g n
                     return [m | (m,label@_) <- lpre g n]

    usersInRole :: (MonadError String m) -- | Rels using Node n in RelRole r
                => RSLT -> Node -> RelRole -> m [Node]
    usersInRole g n r = do gelemM g n -- makes f safe
                           return $ f g n r
      where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
            f g n r = [m | (m,r') <- lpre g n, r'==RelEdge r]

    -- | Rels using Node n in RelRole r
    usersInRoleDe :: RSLT -> Node -> RelRole -> Either DwtErr [Node]
    usersInRoleDe g n r = prependCaller "usersInRoleDe: " $
      do gelemMDe g n -- makes f safe
         return $ f g n r
      where f :: (Graph gr) => gr a RSLTEdge -> Node -> RelRole -> [Node]
            f g n r = [m | (m,r') <- lpre g n, r' == RelEdge r]

    matchRel :: RSLT -> RelSpec -> Either String [Node]
    matchRel g spec = do
      let specList = Map.toList
            $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False) 
            $ spec :: [(RelRole,AddressOrVar)]
      nodeListList <- mapM (\(r,NodeSpec n) -> usersInRole g n r) specList
      return $ listIntersect nodeListList

    matchRelDe :: RSLT -> RelSpec -> Either DwtErr [Node]
    matchRelDe g spec = prependCaller "matchRelDe: " $ do
      let specList = Map.toList
            $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False) 
            $ spec :: [(RelRole,AddressOrVar)]
      nodeListList <- mapM (\(r,NodeSpec n) -> usersInRoleDe g n r) specList
      return $ listIntersect nodeListList

    matchRelLab :: RSLT -> RelSpec -> Either String [LNode Expr]
    matchRelLab g spec = case matchRel g spec of
      Left s -> Left $ "matchRelLab: " ++ s
      Right ns -> Right $ zip ns $ map (fromJust . lab g) ns
        -- fromJust is safe here, because matchRel only returns Nodes in g

    matchRelLabDe :: RSLT -> RelSpec -> Either DwtErr [LNode Expr]
    matchRelLabDe g spec = prependCaller "matchRelLabDe: " $ do
      ns <- matchRelDe g spec
      return $ zip ns $ map (fromJust . lab g) ns
        -- fromJust is safe here, because matchRel only returns Nodes in g

-- using directions (RelSpecs)
    -- todo ? 1Dir because it should have one such direction. I forget why.
      -- clarif: if I want a generation in the Down direction of the rel "has/",
      -- the RelSpec has to have only one Up variable.
    -- TODO ? check: Up|Down good, Any|It bad
      -- fork1Up uses otherDir, so it will catch those errors, but obscurely

--    has1Up :: RelSpec -> Bool
--    has1Up rc = length as == 1
--      where as = Map.toList
--               $ Map.filter (\x -> case x of VarSpec Up -> True; _ -> False) 
--               rc

    has1Dir :: Mbrship -> RelSpec -> Bool
    has1Dir mv rc = length as == 1
      where as = Map.toList
               $ Map.filter (\x -> case x of VarSpec y -> y==mv; _ -> False) 
               rc

    otherDir :: Mbrship -> Mbrship -- incomplete; non-invertible cases will err
    otherDir Up = Down
    otherDir Down = Up

    fork1Dir:: RSLT -> Node -> (Mbrship,RelSpec) -> Either String [Node]
    fork1Dir g n (dir,r) = do -- returns one generation, neighbors
      if has1Dir (otherDir dir) r then return ()
         else throwError $ "fork1Dir: RelSpec " ++ show r
                         ++ " has a number of " ++ show (otherDir dir)
                         ++ " variables other than 1."
      let r' = subNodeForVars n (otherDir dir) r
          dirRoles = Map.keys $ Map.filter (== VarSpec dir) r
      rels <- matchRel g r'
      concat <$> mapM (\rel -> relElts g rel dirRoles) rels
        -- TODO: this line is unnecessary. just return the rels, not their elts.
        -- EXCEPT: that might hurt the dfs, bfs functions below

    fork1Dirs :: RSLT -> Node -> [(Mbrship,RelSpec)] -> Either String [Node]
    fork1Dirs g n rs = concat <$> mapM (fork1Dir g n) rs

    subNodeForVars :: Node -> Mbrship -> RelSpec  -> RelSpec
    subNodeForVars n v r = Map.map -- change each VarSpec v to NodeSpec n
      (\x -> case x of VarSpec v' -> if v==v' then NodeSpec n else VarSpec v'
                       _          -> x   -- yes, the v,v' distinction is needed
      ) r

  -- dfs and bfs
    -- algorithmically, the difference is only newNodes++ns v. ns++newNodes

    _dwtDfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] ->
               Either String [Node]
    _dwtDfs _ _   []             acc = return acc
    _dwtDfs g dir pending@(n:ns) acc = do
      newNodes <- fork1Dir g n dir
        -- ifdo speed: redundant, calls has1Dir a lot
      _dwtDfs g dir (nub $ newNodes++ns) (n:acc)
        -- ifdo speed: discard visited nodes from graph (bfs too)

    dwtDfs :: RSLT -> (Mbrship,RelSpec) -> [Node] -> Either String [Node]
    dwtDfs g dir starts = do
      mapM_ (gelemM g) $ starts
      (nub . reverse) <$> _dwtDfs g dir starts []

    _dwtBfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> [Node] -> 
               Either String [Node]
    _dwtBfs _ _   []             acc = return acc
    _dwtBfs g dir pending@(n:ns) acc = do
      newNodes <- fork1Dir g n dir
      _dwtBfs g dir (nub $ ns++newNodes) (n:acc)

    dwtBfs :: RSLT -> (Mbrship, RelSpec) -> [Node] -> Either String [Node]
    dwtBfs g dir starts = do
      mapM_ (gelemM g) $ starts
      (nub . reverse) <$> _dwtBfs g dir starts []

    -- chase :: Var -> RSLT -> [RelSpec] -> [Node] -> Either String [Node]
