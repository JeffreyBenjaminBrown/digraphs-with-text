    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    -- trying (in Vew.hs): order 
      -- not by comprehension (? impossible since traversed nonlinear)
      -- rather by priority, the user before what it uses

    module Dwt.Graph
      (
        RelPos, Arity
      , Mindmap, Expr(..), DwtEdge(..), RelRole(..), CollRole(..)
      , MbrVar(..), MbrSpec(..), RelVarSpec, RelNodeSpec, RelSpec
      , _splitStringForTplt, mkTplt
      , subInTplt, padTpltStrings, subInTpltWithDollars
      , tpltArity, nodesMatchTplt
      , insLeaf, insRel, insRelUsf, insColl, partitionRelSpec, insRelSpec
        , relNodeSpec, relSpec
        , insStr, insTplt, insFl -- insLeaf generalizes these
      , chNonUser, chNonUserUsf, chRelMbr
      , gelemM, hasLEdgeM, isStr, isStrM, isTplt, isTpltM, isFl, isFlM
      , isRel, isRelM, isColl, isCollM, isLeaf, areLikeExprs
      , node, tpltAt, relElts, validRole, relTplt, collPrinciple
      , rels, mbrs, users, usersInRole, usersInRoleUsf
      , matchRel, has1Up, fork1Up, subNodeForVars, dwtDfs, dwtBfs
      ) where

    import Dwt.Util
    import Data.Graph.Inductive
    import Data.Either (partitionEithers)
    import Data.List (intersect, nub)
    import qualified Data.Map as Map
    import Data.Maybe (catMaybes, fromJust)
    import Control.Monad (mapM_)
    import Control.Monad.Except (MonadError, throwError, catchError)
    import Data.Text (splitOn, pack, unpack, strip)

-- types
    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    type Arity = Int

    type Mindmap = Gr Expr DwtEdge
    data Expr = Str String | Fl Float -- Str, Fl, Tplt: leaves(graph, not tree)
              | Tplt [String] | Rel | Coll
              | RelSpecExpr RelVarSpec deriving(Show,Read,Eq,Ord)
    -- better?: data Expr = Whole Expr |.. where the 2nd Expr cannot be a Whole
      -- could then limit views to Whole Exprs
      -- would have to rewrite a lot of functions

    data DwtEdge = RelEdge RelRole | CollEdge CollRole deriving(Show,Read,Eq,Ord)
    data RelRole = RelTplt | Mbr RelPos deriving(Show,Read,Eq,Ord)
    data CollRole = CollMbr | CollPrinciple deriving(Show,Read,Eq,Ord)

  -- for (partially) specifying Rels
    data MbrVar = It | Any | Up | Down -- todo ? use omission instead of Any
      deriving (Show,Read,Eq,Ord)
    data MbrSpec = VarSpec MbrVar | NodeSpec Node deriving(Show,Read,Eq,Ord)

    type RelVarSpec = Map.Map RelRole MbrVar -- subset of RelSpec info, but
      -- a RelVarSpec in a Mindmap is transformable into a RelSpec.
      -- The rest of the info can be inferred from the edges connected to it.
    type RelNodeSpec = Map.Map RelRole Node -- set-complement of RelVarSpec
    type RelSpec =     Map.Map RelRole MbrSpec
      -- if well-formed, has a Tplt, and RelPoss from 1 to the Tplt's Arity

-- Tplts
    _splitStringForTplt :: String -> [String]
    _splitStringForTplt t = map unpack $ splitOn (pack "_") (pack t)

    -- was : mkTplt = Tplt . _splitStringForTplt -- even length=0 works
    mkTplt :: String -> Expr
    mkTplt = Tplt
      . map (unpack . strip . pack)
      . _splitStringForTplt

    subInTpltWithDollars :: Expr -> [String] -> Int -> String
      -- todo ? test length (should match arity), use Either
      -- todo ? test each tplt-string; if has space, wrap in parens
    subInTpltWithDollars (Tplt ts) ss prefixCount =
      let ts' = padTpltStrings (Tplt ts)
              $ replicate prefixCount '#'
          pairList = zip ts' $ ss ++ [""]
           -- append "" because there are n+1 segments in an n-ary Tplt; 
             -- zipper ends early otherwise
      in foldl (\s (a,b) -> s++a++b) "" pairList
    subInTpltWithDollars _ _ _ = error "subInTplt: not a Tplt" -- todo ? omit

    subInTplt :: Expr -> [String] -> String
    subInTplt (Tplt ts) ss = subInTpltWithDollars (Tplt ts) ss 0

    padTpltStrings :: Expr -> String -> [String]
    padTpltStrings (Tplt ss) prefix =
      let a = head ss
          z = last ss
          middle = reverse $ tail $ reverse $ tail ss
          doToMiddle s = " " ++ prefix ++ s ++ " "
          doToFirst s = case s of "" -> ""
                                  _ -> prefix ++ s ++ " "
          doToLast  s = case s of "" -> ""
                                  _ -> " " ++ prefix ++ s
      in [doToFirst a] ++ map doToMiddle middle ++ [doToLast z]

    tpltArity :: Expr -> Arity
    tpltArity e = case e of Tplt ss -> length ss - 1
                            _       -> error "tpltArity: Expr not a Tplt."

    nodesMatchTplt :: (MonadError String m) => [Node] -> Expr -> m ()
    nodesMatchTplt ns e = case e of
      Tplt _ -> if (tpltArity e) == length ns
        then return ()
        else throwError "nodesMatchTplt: Tplt Arity /= number of member Nodes." 
      _ -> throwError "nodesMatchTplt: Expr not a Tplt."

-- build
  -- insert
    -- TODO ! use a single ins function for Str, Tplt, Fl
      -- Will otherwise need similar duplicates to delete, replace ...
    insLeaf :: Expr -> Mindmap -> Mindmap
    insLeaf e g = case isLeaf e of
      True -> insNode (newNode, e) g
        where [newNode] = newNodes 1 g
      False -> error $ "insLeaf: " ++ show e ++ "is not a leaf."

    insRel :: Node -> -- the template node
              [Node] -> Mindmap -> Either String Mindmap
    insRel tn ns g =
      do mapM_ (gelemM g) $ tn:ns
         t <- tpltAt g tn
         let a = tpltArity t
         nodesMatchTplt ns t
         return $ f (zip ns [1..a]) g'
      where newNode = head $ newNodes 1 g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelEdge $ Mbr $ snd p) g
            g' =                insEdge (newNode, tn, RelEdge RelTplt)
                              $ insNode (newNode, Rel) g

    insRelUsf :: Node -> [Node] -> Mindmap -> Mindmap
    insRelUsf t ns g = if ta /= length ns -- t is tplt, otherwise like ns
        then error "insRelUsf: Tplt Arity /= number of members Nodes."
        else if any (==False) $ map (flip gelem g) $ (t:ns)
          then error "insRelUsf: One of those Nodes is not in the Mindmap."
        else f (zip ns [1..ta]) g'
      where te@(Tplt ts) = fromJust $ lab g t -- can also error:
              -- by finding Str or Rel where expected Tplt
            ta = tpltArity te
            newNode = head $ newNodes 1 g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelEdge $ Mbr $ snd p) g
            g' =                insEdge (newNode, t, RelEdge RelTplt)
                              $ insNode (newNode, Rel) g

    insColl :: (MonadError String m) => 
      (Maybe Node) -> -- title
      [Node] -> Mindmap -> m Mindmap
    insColl mt ns g = do
      mapM_ (gelemM g) ns
      let newNode = head $ newNodes 1 g
          nameEdges = case mt of Nothing -> []
                                 Just tn -> [(newNode, tn,CollEdge CollPrinciple)]
          newEdges = nameEdges ++
            map (\n -> (newNode, n, CollEdge CollMbr)) ns
      return $ insEdges newEdges $ insNode (newNode,Coll) g

    partitionRelSpec :: RelSpec -> (RelVarSpec, RelNodeSpec)
    partitionRelSpec rSpec =
      let (vs,ns) = Map.partition (\mSpec -> case mSpec of VarSpec _ -> True
                                                           NodeSpec _ -> False
                                  ) rSpec
      in ( Map.map  (\(VarSpec  v) -> v)  vs
         , Map.map  (\(NodeSpec n) -> n)  ns )

    insRelSpec :: (MonadError String m) => RelSpec -> Mindmap -> m Mindmap
    insRelSpec rSpec g = do
      let (varMap, nodeMap) = partitionRelSpec rSpec
          newNode = head $ newNodes 1 g
          newLNode = (newNode, RelSpecExpr varMap)
      mapM_ (gelemM g) $ Map.elems nodeMap
      let newLEdges = map (\(r,n) -> (newNode, n, RelEdge r))
                    $ Map.toList nodeMap
      return $ insEdges newLEdges
             $ insNode newLNode g

    relNodeSpec :: (MonadError String m) => Mindmap -> Node -> m RelNodeSpec
    relNodeSpec g n = do
      gelemM g n
      case (fromJust $ lab g n) of
        RelSpecExpr _ -> return $ Map.fromList 
          $ map (\(node,RelEdge r)->(r,node)) $ lsuc g n
        _ -> throwError $ "Node " ++ show n ++ " not a RelSpecExpr."

    relSpec :: Mindmap -> Node -> Either String RelSpec
    relSpec g n = do -- nearly inverse to partitionRelSpec
      gelemM g n
      case (fromJust $ lab g n) of
        RelSpecExpr rvs -> do
          let rnsl = Map.toList $ fromRight $ relNodeSpec g n -- RelNodeSpec list
              rvsl = Map.toList rvs -- RelVarSpec list
              rvsl' = map (\(role,var) ->(role,VarSpec  var )) rvsl
              rnsl' = map (\(role,node)->(role,NodeSpec node)) rnsl
          return $ Map.fromList $ rvsl' ++ rnsl'

    insStr :: String -> Mindmap -> Mindmap
    insStr str g = insNode (newNode, Str str) g
      where newNode = head $ newNodes 1 g

    insTplt :: String -> Mindmap -> Mindmap
    insTplt s g = insNode (newNode, mkTplt s) g
      where newNode = head $ newNodes 1 g

    insFl :: Float -> Mindmap -> Mindmap
    insFl f g = insNode (newNode, Fl f) g
      where newNode = head $ newNodes 1 g

  -- edit
    chNonUser :: (MonadError String m) => Mindmap -> Node -> Expr -> m Mindmap
      -- Strs and Tplts are used, but are not users. (Rels and Colls use them.)
    chNonUser g n e' = do
      let me = lab g n
      let mismatch = throwError $ "chNonUser: constructor mismatch"
      case me of
        Just e@(Str _)  -> if areLikeExprs e e' then return () else mismatch
        Just e@(Tplt _) -> if areLikeExprs e e' then return () else mismatch
        Nothing -> throwError $ "chNonUser: Node " ++ show n ++ " absent."
        _       -> throwError $ "chNonUser: Node " ++ show n ++ " is a user."
      return $ chNonUserUsf g n e'

    chNonUserUsf :: Mindmap -> Node -> Expr -> Mindmap
    chNonUserUsf g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'

    chRelMbr :: (MonadError String m) => 
      Mindmap -> Node -> Node -> RelRole -> m Mindmap
    chRelMbr g user newMbr role = do
      isRelM g user `catchError` (\_ -> throwError $ 
        "chRelMbr: Node " ++ show user ++ " absent or not a Rel.")
      gelemM g newMbr
      let candidates = [n | (n,lab) <- lsuc g user, lab == RelEdge role]
      if length candidates /= 1
        then throwError "chRelMbr: invalid graph state, or RelPos out of range"
        else return ()
      let oldMbr = head candidates
      return $ delLEdge (user,oldMbr,RelEdge role)
             $ insEdge (user,newMbr,RelEdge role) g

-- query
  -- the simplest
    gelemM :: (MonadError String m, Graph gr) => gr a b -> Node -> m ()
    gelemM g n = if gelem n g then return () 
      else throwError $ "gelemM: Node " ++ show n ++ " absent."

    hasLEdgeM :: (MonadError String m, Graph gr, Eq b, Show b) => 
      gr a b -> LEdge b -> m ()
    hasLEdgeM g le = if hasLEdge g le then return ()
      else throwError $ "hasLEdgeM: LEdge " ++ show le ++ " absent."

    _isExprMConstructor :: (MonadError String m, Graph gr) => (a -> Bool) ->
      gr a b -> Node -> m () -- TODO: catch these erors, append strings
      -- otherwise the distinction bewteen absence and inequality is lost
    _isExprMConstructor pred g n = case mExpr of 
        Nothing -> throwError $ "Node " ++ show n ++ " absent."
        Just expr ->  case pred expr of True -> return ()
                                        False -> throwError $ "is not"
      where mExpr = lab g n

    isStr :: Expr -> Bool
    isStr x = case x of Str _ -> True; _ -> False

    isStrM :: (MonadError String m) => Mindmap -> Node -> m ()
    isStrM = _isExprMConstructor isStr

    isTplt :: Expr -> Bool
    isTplt x = case x of Tplt _ -> True; _ -> False

    isTpltM :: (MonadError String m) => Mindmap -> Node -> m ()
    isTpltM = _isExprMConstructor isTplt

    isFl :: Expr -> Bool
    isFl x = case x of Fl _ -> True; _ -> False

    isFlM :: (MonadError String m) => Mindmap -> Node -> m ()
    isFlM = _isExprMConstructor isFl

    isRel :: Expr -> Bool
    isRel x = case x of Rel -> True; _ -> False

    isRelM :: (MonadError String m) => Mindmap -> Node -> m ()
    isRelM = _isExprMConstructor isRel

    isColl :: Expr -> Bool
    isColl x = case x of Coll -> True; _ -> False

    isCollM :: (MonadError String m) => Mindmap -> Node -> m ()
    isCollM = _isExprMConstructor isColl

    isLeaf :: Expr -> Bool -- TODO ? make Leaf an Expr constructor
    isLeaf (Str _) = True
    isLeaf (Fl _) = True
    isLeaf (Tplt _) = True
    isLeaf _ = False

    areLikeExprs :: Expr -> Expr -> Bool
    areLikeExprs e f = case e of
      Str _  ->  case f of Str  _ -> True;  _ -> False
      Tplt _ ->  case f of Tplt _ -> True;  _ -> False
      Rel    ->  case f of Rel    -> True;  _ -> False
      Coll   ->  case f of Coll   -> True;  _ -> False

  -- more ("locate"?)
    node :: Mindmap -> Expr -> [Node] -- hopefully length = 1
    node g x = nodes $ labfilter (== x) g

    tpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr
    tpltAt g tn = case lab g tn of
      Just t@(Tplt _) -> return t
      Nothing -> throwError $ "tpltAt: Node " ++ show tn ++ " absent."
      _       -> throwError $ "tpltAt: LNode " ++ show tn ++ " not a Tplt."

    relElts :: Mindmap -> Node -> [RelRole] -> Either String [Node]
    relElts g relNode roles = do
      isRelM g relNode `catchError` (\_ -> throwError $
        "relElts: Node " ++ show relNode ++ " absent or not a Rel.")
      mapM_  (validRole g relNode) roles `catchError` (\_ -> throwError $
        "relElts: at least one member out of bounds")
      return [n | (n, RelEdge r) <- lsuc g relNode, elem r roles]

    validRole :: Mindmap -> Node -> RelRole -> Either String ()
    validRole g relNode role = do
      isRelM g relNode `catchError` (\_ -> throwError 
        $ "validRole: Node " ++ show relNode ++ " absent or not a Rel.")
      case role of
        RelTplt -> return ()
        Mbr p -> do
          if p < 1 then throwError $ "validRole: RelPos < 1" else return ()
          t <- relTplt g relNode
          let a = tpltArity t
          if p <= a then return ()
            else throwError $ "validRole: Arity " ++ show a ++ 
              " < RelPos " ++ show p

    relTplt :: Mindmap -> Node -> Either String Expr
    relTplt g relNode = do
      [n] <- relElts g relNode [RelTplt]
      return $ fromJust $ lab g n

    collPrinciple :: (MonadError String m) => Mindmap -> Node -> m Expr
    collPrinciple g collNode = do
      isCollM g collNode `catchError` (\_ -> throwError $ 
        "collPrinciple: Node " ++ show collNode ++ " absent or not a Coll.")
      return $ fromJust $ lab g $ head
        [n | (n, CollEdge CollPrinciple) <- lsuc g collNode]

  -- .. -> [Node]
    rels :: Gr Expr b -> [Node]
    rels = nodes . labfilter (\n -> case n of Tplt _ -> True; _ -> False) 

    mbrs :: Mindmap -> Node -> [Node]
    mbrs g n = [addr | (addr,elab) <- lsuc g n, isMbrEdge elab]
      where isMbrEdge e = case e of (RelEdge (Mbr _)) -> True; _ -> False

    users :: (MonadError String m, Graph gr) => gr a b -> Node -> m [Node]
    users g n = do gelemM g n
                   return [m | (m,label@_) <- lpre g n]

    usersInRole :: (MonadError String m) => Mindmap -> Node -> RelRole -> m [Node]
    usersInRole g n r = do -- Rels using Node n in RelRole r
      gelemM g n
      return $ usersInRoleUsf g n r

    usersInRoleUsf :: (Graph gr) => gr a DwtEdge -> Node -> RelRole -> [Node]
    usersInRoleUsf g n r = [m | (m,r') <- lpre g n, r'==RelEdge r]

    matchRel :: Mindmap -> RelSpec -> Either String [Node]
    matchRel g spec = do
      let specList = Map.toList
            $ Map.filter (\ns -> case ns of NodeSpec _ -> True; _ -> False) 
            $ spec :: [(RelRole,MbrSpec)]
      nodeListList <- mapM (\(r,NodeSpec n) -> usersInRole g n r) specList
      return $ listIntersect nodeListList

-- using directions (RelSpecs)
    has1Up :: RelSpec -> Bool -- Up should be only one direction. I forget why.
    has1Up rc = length as == 1
      where as = Map.toList
               $ Map.filter (\x -> case x of VarSpec Up -> True; _ -> False) 
               rc

    fork1Up :: Mindmap -> Node -> RelSpec -> Either String [Node]
    fork1Up g n r = do -- one generation of successors in the Down direction
      if has1Up r then return [] else throwError $ "fork1Up: RelSpec " ++ show r
        ++ " has a number of Up variables other than 1."
      let r' = subNodeForVars n Up r
          kataRoles = Map.keys $ Map.filter (\x -> case x of VarSpec Down -> True;
                                                             _ -> False) r
      rels <- matchRel g r'
      concat <$> mapM (\rel -> relElts g rel kataRoles) rels

    subNodeForVars :: Node -> MbrVar -> RelSpec  -> RelSpec
    subNodeForVars n v r = Map.map -- change each VarSpec v to NodeSpec n
      (\x -> case x of VarSpec v' -> if v==v' then NodeSpec n else VarSpec v'
                       _          -> x   -- yes, the v,v' distinction is needed
      ) r

  -- dfs and bfs
    -- algorithmically, the difference is only newNodes++ns v. ns++newNodes

    _dwtDfs :: Mindmap -> RelSpec -> [Node] -> [Node] -> Either String [Node]
    _dwtDfs _ _   []             acc = return acc
    _dwtDfs g dir pending@(n:ns) acc = do
      newNodes <- fork1Up g n dir -- ifdo speed: redundant, calls has1Up a lot
      _dwtDfs g dir (nub $ newNodes++ns) (n:acc)
        -- ifdo speed: discard visited nodes from graph (bfs too)

    dwtDfs :: Mindmap -> RelSpec -> [Node] -> Either String [Node]
    dwtDfs g dir starts = do
      mapM_ (gelemM g) $ starts
      (nub . reverse) <$> _dwtDfs g dir starts []

    _dwtBfs :: Mindmap -> RelSpec -> [Node] -> [Node] -> Either String [Node]
    _dwtBfs _ _   []             acc = return acc
    _dwtBfs g dir pending@(n:ns) acc = do
      newNodes <- fork1Up g n dir -- ifdo speed: redundant, calls has1Up a lot
      _dwtBfs g dir (nub $ ns++newNodes) (n:acc)

    dwtBfs :: Mindmap -> RelSpec -> [Node] -> Either String [Node]
    dwtBfs g dir starts = do
      mapM_ (gelemM g) $ starts
      (nub . reverse) <$> _dwtBfs g dir starts []

    -- chase :: Var -> Mindmap -> [RelSpec] -> [Node] -> Either String [Node]
