-- usually folded
  -- PROB: view should not be type .. ->  IO ()
  -- WANT
    -- parents, children: count per node
  -- "todo": find an automatic way to check for unused functions
    -- otherwise when pattern matching, if the function name handed one of the 
    -- patterns is misspelled, I can get a "non-exhaustive patterns" exception

-- pragmas, export, import
    {-# LANGUAGE FlexibleContexts #-}

    module Dwt.Graph
      ( module Data.Graph.Inductive
      , Arity, RelPos, Expr(..), Role(..), Mindmap
      , splitTpltStr, stringToTplt, subInTplt -- Tplt
      , insStr, insTplt, insRel, insColl -- build Mindmap
      , chNonRelAt, chMbr -- edit Mindmap
      -- query Mindmap
        -- minor
          , gelemM, hasLEdgeM, isStr, isTplt, isRel, isColl
          , tpltAt, relTpltAt, tpltArity, nodesMatchTplt
        -- .. -> [Node]
          , users, specUsersUsf, specUsers, redundancySubs, matchRel
      , insRelUsf, chNonRelAtUsf -- unsafe, duplicates
      ) where

    import Dwt.Util
    import Data.Graph.Inductive
    import Data.Either (partitionEithers)
    import Data.List (intersect)
    import qualified Data.Map as Map
    import Data.Maybe (isJust, catMaybes, fromJust)
    import Control.Monad (mapM_)
    import Control.Monad.Except (MonadError, throwError)
    import Data.Text (splitOn, pack, unpack)

-- types
    -- Exprs (expressions) play Roles in Rels (relationships).
    -- Each Arity-k Rel emits k+1 Edges toward the other Exprs:
      -- one connects it to its RelTplt (relationship template)
      -- k more connect it to each of its k RelMbrs (relationship members)
    -- data/minmalGraph.hs demonstrates these types (over like 20 lines)

    type Mindmap = Gr Expr Role
    data Role = RelTplt | RelMbr RelPos | CollMbr
      deriving (Show,Read,Eq,Ord)
    data Expr = Str String
              | Tplt Arity [String] -- TODO ? deduce Arity from the [String]
              | Rel Arity -- TODO ? deduce Arity from the associated Tplt
              | Coll String
      deriving (Show,Read,Eq,Ord)
    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    type Arity = Int

    data NodeSpec = It | Any | NodeSpec Node
    -- REPLACING second with first
    type RelSpec' = Map.Map Role NodeSpec -- TODO: Role should not be ColMbr
    type RelSpec = [Maybe Node] -- to spec an Arity k Rel, should be length k+1
                                -- first is RelTplt, others RelPos, in order

-- build
  -- Tplt <-> String
    splitTpltStr :: String -> [String]
    splitTpltStr t = map unpack $ splitOn (pack "_") (pack t)

    stringToTplt :: String -> Expr
    stringToTplt s = Tplt (length ss-1) ss -- even length=0 works
      where ss = splitTpltStr s

    subInTplt :: Expr -> [String] -> String
    subInTplt (Tplt k ts) ss = let pairList = zip ts $ ss ++ [""] 
      -- append "" because there are n+1 segments in an n-ary Tplt; 
        -- zipper ends early otherwise
      in foldl (\s (a,b) -> s++a++b) "" pairList

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
         a <- tpltArity t
         nodesMatchTplt ns t
         return $ let 
             newNode = head $ newNodes 1 g
             f []     g = g
             f (p:ps) g = f ps $ insEdge (newNode, fst p, RelMbr $ snd p) g
             g' =                insEdge (newNode, tn, RelTplt)
                               $ insNode (newNode, Rel a) g
           in f (zip ns [1..a]) g'

    insColl :: (MonadError String m) => String -> [Node] -> Mindmap -> m Mindmap
    insColl prefix ns g = do
      mapM_ (gelemM g) ns
      let newNode = head $ newNodes 1 g
          newEdges = map (\n -> (newNode,n,CollMbr)) ns
      return $ insEdges newEdges $ insNode (newNode,Coll prefix) g

  -- edit
    chNonRelAt :: (MonadError String m) => Mindmap -> Node -> Expr -> m Mindmap
    chNonRelAt g n e = do -- todo? absorb def of chNonRelAtUsf.
      -- todo ? verify e is Tplt or Str, and that it matches the label of n in g
      gelemM g n
      return $ chNonRelAtUsf g n e

    chMbr :: (MonadError String m) => Mindmap -> Node -> Node -> Role -> m Mindmap
    chMbr g user newMbr role = do
      isRel g user
      gelemM g newMbr
      let oldMbr = head [n | (n,lab) <- lsuc g user, lab == role]
        -- todo ? head is unsafe
      return $ delLEdge (user,oldMbr,role)
             $ insEdge (user,newMbr,role) g

-- query
  -- tests and lookups for smaller-than-graph types
    gelemM :: (MonadError String m, Graph gr) => gr a b -> Node -> m ()
    gelemM g n = if gelem n g 
      then return () 
      else throwError $ "gelemM: Node " ++ show n ++ " absent."

    hasLEdgeM :: (MonadError String m, Graph gr, Eq b, Show b) => 
      gr a b -> LEdge b -> m ()
    hasLEdgeM g le = if hasLEdge g le
      then return ()
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
    isTplt = _isExprConstructor (\x -> case x of Tplt _ _ -> True; _ -> False)

    isRel :: (MonadError String m) => Mindmap -> Node -> m Bool
    isRel = _isExprConstructor (\x -> case x of Rel _ -> True; _ -> False)

    isColl :: (MonadError String m) => Mindmap -> Node -> m Bool
    isColl = _isExprConstructor (\x -> case x of Coll _ -> True; _ -> False)

    tpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr
    tpltAt g tn = case lab g tn of -- todo ? rewrite using isTplt
      Just t@(Tplt _ _) -> return $ t
      Nothing -> throwError $ "tpltAt: Node " ++ show tn ++ " absent."
      _ -> throwError $ "tpltAt: LNode " ++ show tn ++ " not a Tplt."

    relTpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr
    relTpltAt g rn = do
      ir <-isRel g rn
      if not ir
        then throwError $ "relTpltAt: LNode " ++ show rn ++ " not a Rel."
        else return $ fromJust $ lab g 
          $ head [n | (n,RelTplt) <- lsuc g rn] -- todo ? head unsafe
            -- but is only unsafe if graph takes an invalid state
            -- because each Rel should have exactly one Tplt

    tpltArity :: (MonadError String m) => Expr -> m Arity
    tpltArity e = case e of Tplt a _ -> return a
                            _        -> throwError "tpltArity: Expr not a Tplt."

    nodesMatchTplt :: (MonadError String m) => [Node] -> Expr -> m () -- todo:test
    nodesMatchTplt ns e = case e of
      Tplt k _ -> if k /= length ns 
        then throwError "nodesMatchTplt: Tplt Arity /= number of member Nodes."
        else return ()
      _ -> throwError "nodesMatchTplt: Expr not a Tplt."

  -- .. -> [Node]
    users :: (MonadError String m) => Mindmap -> Node -> m [Node]
    users g n = do gelemM g n
                   return [m | (m,label) <- lpre g n]

    specUsersUsf :: Mindmap -> Arity -> Node -> Role -> [Node]
    specUsersUsf g k n r = -- all k-ary Rels using Node n in Role r
      [m | (m,r) <- lpre g n, lab g m == (Just $ Rel k)]

    specUsers :: (MonadError String m) =>
      Mindmap -> Arity -> Node -> Role -> m [Node]
    specUsers g k n r = do -- all k-ary Rels using Node n in Role r
      gelemM g n
      return $ specUsersUsf g k n r

    redundancySubs :: [Maybe Node] -> Map.Map Node String
    redundancySubs mns = Map.fromList 
      $ map (\n -> (n,show n)) $ catMaybes mns

    -- REPLACING second with first
--    matchRel' :: Mindmap -> RelSpec -> [Node]
--    matchRel' g spec =
--      -- keep the SpecItems that spec a node
--      -- DETOUR: would be better with a specUser that needs no arity
--      -- for each key, specUser g arity 
--      map (specUsersUsf g arity n) relSpec

    matchRel :: Mindmap -> [Maybe Node] -> [Node]
    matchRel g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, RelPos)]
              -- kind of cheating; using RelPos 0 for RelTplt
            f (Just n, 0) = specUsersUsf g arity n RelTplt
            f (Just n, k) = specUsersUsf g arity n (RelMbr k)

-- deprecating: non-monadic, unsafe, duplicate functions (used elsewhere)
    insRelUsf :: Node -> [Node] -> Mindmap -> Mindmap
    insRelUsf t ns g = if ti /= length ns -- t is tplt, otherwise like ns
        then error "insRelUsf: Tplt Arity /= number of members Nodes."
        else if any (==False) $ map (flip gelem g) $ (t:ns)
          then error "insRelUsf: One of those Nodes is not in the Mindmap." 
        else f (zip ns [1..ti]) g'
      where Tplt ti ts = fromJust $ lab g t -- can also error:
              -- by finding Str or Rel where expected Tplt
            newNode = head $ newNodes 1 g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelMbr $ snd p) g
            g' =                insEdge (newNode, t, RelTplt)
                              $ insNode (newNode, Rel ti) g

    chNonRelAtUsf :: Mindmap -> Node -> Expr -> Mindmap
    chNonRelAtUsf g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'
