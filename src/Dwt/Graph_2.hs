    {-# LANGUAGE FlexibleContexts #-}

    module Dwt.Graph_2 (
      module Dwt.Graph_2
    ) where

    import Dwt.Util
    import Data.Graph.Inductive
    import Data.Either (partitionEithers)
    import Data.List (intersect)
    import qualified Data.Map as Map
    import Data.Maybe (catMaybes, fromJust)
    import Control.Monad (mapM_)
    import Control.Monad.Except (MonadError, throwError)
    import Data.Text (splitOn, pack, unpack)

    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    type Arity = Int

    type Mindmap = Gr Expr DwtEdge
    data Expr = Str String | Tplt [String] | Rel | Coll String
              | RelSpecExpr RelVarSpec
    data DwtEdge = RoleEdge Role | CollMbr
    data Role = TpltRole | MbrRole RelPos
      deriving (Show,Read,Eq,Ord)

    data MbrVar = It | Any | Ana | Kata -- TODO: can oft (always?) omit the Any
    data MbrSpec = VarSpec MbrVar | MbrSpec Node

    type RelVarSpec = Map.Map Role MbrVar -- STILL TODO: RelRole
                                                  -- using Role instead
      -- specifies a subset of what a RelSpec does
      -- the other information is carried external to it, in the graph
    type RelSpec = Map.Map Role MbrSpec
      -- if well-formed, has a Tplt, and has RelPoss from 1 to the Tplt's Arity
      -- (but anything mapping to Any can be dropped)

-- build
  -- Tplt <-> String
    splitStringForTplt :: String -> [String]
    splitStringForTplt t = map unpack $ splitOn (pack "_") (pack t)

    stringToTplt :: String -> Expr
    stringToTplt = Tplt . splitStringForTplt -- even length=0 works

    subInTplt :: Expr -> [String] -> String -- todo ? test length, use Either
    subInTplt (Tplt ts) ss = let pairList = zip ts $ ss ++ [""] 
      -- append "" because there are n+1 segments in an n-ary Tplt; 
        -- zipper ends early otherwise
      in foldl (\s (a,b) -> s++a++b) "" pairList
    subInTplt _ _ = error "subInTplt: not a Tplt"

  -- insert
    insStr :: String -> Mindmap -> Mindmap
    insStr str g = insNode (int, Str str) g
      where int = head $ newNodes 1 g

    insTplt :: String -> Mindmap -> Mindmap
    insTplt s g = insNode (newNode, stringToTplt s) g
      where newNode = head $ newNodes 1 g

    insRel :: (MonadError String m) => Node -> [Node] -> Mindmap -> m Mindmap
    insRel tn ns g =
      do mapM_ (gelemM g) $ tn:ns
         t <- tpltAt g tn
         let a = tpltArity t
         nodesMatchTplt ns t
         return $ f (zip ns [1..a]) g'
      where newNode = head $ newNodes 1 g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelMbr $ snd p) g
            g' =                insEdge (newNode, tn, RelTplt)
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
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelMbr $ snd p) g
            g' =                insEdge (newNode, t, RelTplt)
                              $ insNode (newNode, Rel) g

    insColl :: (MonadError String m) => String -> [Node] -> Mindmap -> m Mindmap
    insColl prefix ns g = do
      mapM_ (gelemM g) ns
      let newNode = head $ newNodes 1 g
          newEdges = map (\n -> (newNode,n,CollMbr)) ns
      return $ insEdges newEdges $ insNode (newNode,Coll prefix) g

  -- edit
    chNonUserAt :: (MonadError String m) => Mindmap -> Node -> Expr -> m Mindmap
      -- Strs and Tplts are used, but are not users. (Rels and Colls use them.)
    chNonUserAt g n e' = do
      let me = lab g n
      let mismatch = throwError $ "chNonUserAt: constructor mismatch"
      case me of
        Just e@(Str _)  -> if isLikeExpr e e' then return () else mismatch
        Just e@(Tplt _) -> if isLikeExpr e e' then return () else mismatch
        Nothing -> throwError $ "chNonUserAt: Node " ++ show n ++ " absent."
        _       -> throwError $ "chNonUserAt: Node " ++ show n ++ " is a user."
      return $ chNonUserAtUsf g n e'

    chNonUserAtUsf :: Mindmap -> Node -> Expr -> Mindmap
    chNonUserAtUsf g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'

    chRelMbr :: (MonadError String m) => 
      Mindmap -> Node -> Node -> Role -> m Mindmap
    chRelMbr g user newMbr role = do
      isRel g user
      gelemM g newMbr
      let candidates = [n | (n,lab) <- lsuc g user, lab == role]
      if length candidates /= 1
        then throwError "chRelMbr: invalid graph state, or RelPos out of range"
        else return ()
      let oldMbr = head candidates
      return $ delLEdge (user,oldMbr,role)
             $ insEdge (user,newMbr,role) g

-- query
  -- tests and lookups for smaller-than-graph types
    gelemM :: (MonadError String m, Graph gr) => gr a b -> Node -> m ()
    gelemM g n = if gelem n g then return () 
      else throwError $ "gelemM: Node " ++ show n ++ " absent."

    hasLEdgeM :: (MonadError String m, Graph gr, Eq b, Show b) => 
      gr a b -> LEdge b -> m ()
    hasLEdgeM g le = if hasLEdge g le then return ()
      else throwError $ "hasLEdgeM: LEdge " ++ show le ++ " absent."

    _isExprConstructor :: (MonadError String m, Graph gr) => (a -> Bool) ->
      gr a b -> Node -> m Bool
    _isExprConstructor pred g n = case mExpr of 
        Nothing -> throwError $ "Node " ++ show n ++ " absent."
          -- todo ? report the using function (isStr, isTplt, isRel) in the error
        Just expr ->  return $ pred expr
      where mExpr = lab g n

    isStr :: (MonadError String m) => Mindmap -> Node -> m Bool
    isStr = _isExprConstructor (\x -> case x of Str _ -> True; _ -> False)

    isTplt :: (MonadError String m) => Mindmap -> Node -> m Bool
    isTplt = _isExprConstructor (\x -> case x of Tplt _ -> True; _ -> False)

    isRel :: (MonadError String m) => Mindmap -> Node -> m Bool
    isRel = _isExprConstructor (\x -> case x of Rel -> True; _ -> False)

    isColl :: (MonadError String m) => Mindmap -> Node -> m Bool
    isColl = _isExprConstructor (\x -> case x of Coll _ -> True; _ -> False)

    isLikeExpr :: Expr -> Expr -> Bool
    isLikeExpr e f = case e of
      Str _  ->  case f of Str _  -> True;  _ -> False
      Tplt _ ->  case f of Tplt _ -> True;  _ -> False
      Rel    ->  case f of Rel    -> True;  _ -> False
      Coll _ ->  case f of Coll _ -> True;  _ -> False

    tpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr
    tpltAt g tn = case lab g tn of
      Just t@(Tplt _) -> return t
      Nothing -> throwError $ "tpltAt: Node " ++ show tn ++ " absent."
      _       -> throwError $ "tpltAt: LNode " ++ show tn ++ " not a Tplt."

    relTpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr
    relTpltAt g rn = do
      ir <-isRel g rn
      if not ir
        then throwError $ "relTpltAt: LNode " ++ show rn ++ " not a Rel."
        else return $ fromJust $ lab g -- fromJust: safe b/c found in next line
          $ head [n | (n,RelTplt) <- lsuc g rn]
            -- head is only unsafe if graph takes an invalid state
            -- because each Rel should have exactly one Tplt

    tpltArity :: Expr -> Arity
    tpltArity e = case e of Tplt ss -> length ss - 1
                            _       -> error "tpltArity: Expr not a Tplt."

    nodesMatchTplt :: (MonadError String m) => [Node] -> Expr -> m ()
    nodesMatchTplt ns e = case e of
      Tplt _ -> if (tpltArity e) == length ns
        then return ()
        else throwError "nodesMatchTplt: Tplt Arity /= number of member Nodes." 
      _ -> throwError "nodesMatchTplt: Expr not a Tplt."

  -- .. -> [Node]
    users :: (MonadError String m, Graph gr) => gr a b -> Node -> m [Node]
    users g n = do gelemM g n
                   return [m | (m,label) <- lpre g n]

    -- Rels using Node n in Role r
    specUsers :: (MonadError String m) => Mindmap -> Node -> Role -> m [Node]
    specUsers g n r = do
      gelemM g n
      return $ specUsersUsf g n r

    specUsersUsf :: (Graph gr) => gr a Role -> Node -> Role -> [Node]
    specUsersUsf g n r = [m | (m,r') <- lpre g n, r==r']

    redundancySubs :: RelSpec -> Map.Map Node String
    redundancySubs = Map.fromList 
      . map (\(MbrSpec n) -> (n,show n))
      . Map.elems
      . Map.filter (\ns -> case ns of MbrSpec n -> True; _ -> False) 

    matchRel :: (MonadError String m) => Mindmap -> RelSpec -> m [Node]
    matchRel g spec = do
      let specList = Map.toList
            $ Map.filter (\ns -> case ns of MbrSpec n -> True; _ -> False) 
            $ spec :: [(Role,MbrSpec)]
      nodeListList <- mapM (\(r,MbrSpec n) -> specUsers g n r) specList
      return $ listIntersect nodeListList
