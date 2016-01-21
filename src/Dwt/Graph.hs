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
      , module Dwt.Graph
      )
--      , Arity, RelPos, Expr(..), Role(..), Mindmap
--      , NodeSpec(..), RelSpec
--      , splitStringForTplt, stringToTplt, subInTplt -- Tplt
--      , insStr, insTplt, insRel, insColl -- build Mindmap
--      , chNonRelAt, chMbr -- edit Mindmap
--      -- query Mindmap
--        -- minor
--          , gelemM, hasLEdgeM, isStr, isTplt, isRel, isColl
--          , tpltAt, relTpltAt, tpltArity, nodesMatchTplt
--        -- .. -> [Node]
--          , users, specUsersUsf, specUsersUsfOld, specUsers
--          , redundancySubs, matchRel
--      , insRelUsf, chNonRelAtUsf -- unsafe, duplicates
--      ) 
    where

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

    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    type Arity = Int

    type Mindmap = Gr Expr Role
    data Expr = Str String
              | Tplt [String]
              | Rel
              | Coll String
      deriving (Show,Read,Eq,Ord)
    data Role = RelTplt | RelMbr RelPos | CollMbr
      deriving (Show,Read,Eq,Ord)

    data NodeSpec = It | Any | NodeSpec Node deriving (Show,Eq)
    type RelSpec = Map.Map Role NodeSpec 
      -- if well-formed: 
        -- has a Tplt, and RelPoss from 1 to the Tplt's Arity
        -- has no ColMbr
      -- todo ? any RelPos mapped to Any could be omitted

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
         a <- tpltArity t
         nodesMatchTplt ns t
         return $ let 
             newNode = head $ newNodes 1 g
             f []     g = g
             f (p:ps) g = f ps $ insEdge (newNode, fst p, RelMbr $ snd p) g
             g' =                insEdge (newNode, tn, RelTplt)
                               $ insNode (newNode, Rel) g
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
    isTplt = _isExprConstructor (\x -> case x of Tplt _ -> True; _ -> False)

    isRel :: (MonadError String m) => Mindmap -> Node -> m Bool
    isRel = _isExprConstructor (\x -> case x of Rel -> True; _ -> False)

    isColl :: (MonadError String m) => Mindmap -> Node -> m Bool
    isColl = _isExprConstructor (\x -> case x of Coll _ -> True; _ -> False)

    tpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr
    tpltAt g tn = case lab g tn of -- todo ? rewrite using isTplt
      Just t@(Tplt _) -> return $ t
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

    relTpltArity :: (MonadError String m) => Mindmap -> Node -> m Arity
    relTpltArity g rn = do
      t <- relTpltAt g rn
      tpltArity t

    tpltArity :: (MonadError String m) => Expr -> m Arity -- todo ? no MonadError
    tpltArity e = case e of Tplt ss -> return $ length ss - 1
                            _       -> throwError "tpltArity: Expr not a Tplt."

    nodesMatchTplt :: (MonadError String m) => [Node] -> Expr -> m ()
    nodesMatchTplt ns e = case e of -- TODO ! test
      Tplt _ -> do
        a <- tpltArity e
        if a /= length ns
        then throwError "nodesMatchTplt: Tplt Arity /= number of member Nodes."
        else return ()
      _ -> throwError "nodesMatchTplt: Expr not a Tplt."

  -- .. -> [Node]
    users :: (MonadError String m, Graph gr) => gr a b -> Node -> m [Node]
    users g n = do gelemM g n
                   return [m | (m,label) <- lpre g n]

    -- MAYBE REPLACING second with first
    specUsersUsf :: (Graph gr) => gr a Role -> Node -> Role -> [Node]
    specUsersUsf g n r = -- Rels using Node n in Role r
      [m | (m,r') <- lpre g n, r==r']

    specUsersUsfOld :: (MonadError String m) => 
      Mindmap -> Node -> Role -> m [Node]
    specUsersUsfOld g n r = do -- Rels (of any Arity) using Node n in Role r
      return [m | (m,r') <- lpre g n, r'==r, lab g m == (Just $ Rel)]

    specUsers :: (MonadError String m) =>
      Mindmap -> Node -> Role -> m [Node]
    specUsers g n r = do -- Rels (of any Arity) using Node n in Role r
      gelemM g n
      specUsersUsfOld g n r

    redundancySubs :: RelSpec -> Map.Map Node String
    redundancySubs m = Map.fromList $
      map (\(NodeSpec n) -> (n,show n)) 
      $ Map.elems
      $ Map.filter (\nspec -> case nspec of NodeSpec n -> True; _ -> False) m

    matchRel :: Graph gr => gr a Role -> RelSpec -> [Node]
    matchRel g spec = listIntersect 
      $ map (\(r,NodeSpec n) -> specUsersUsf g n r)
      $ Map.toList
      $ Map.filter (\ns -> case ns of NodeSpec n -> True; _ -> False) 
                   spec

-- deprecating
  -- non-monadic, unsafe, duplicate functions (used elsewhere)
    insRelUsf :: Node -> [Node] -> Mindmap -> Mindmap
    insRelUsf t ns g = if ta /= length ns -- t is tplt, otherwise like ns
        then error "insRelUsf: Tplt Arity /= number of members Nodes."
        else if any (==False) $ map (flip gelem g) $ (t:ns)
          then error "insRelUsf: One of those Nodes is not in the Mindmap." 
        else f (zip ns [1..ta]) g'
      where te@(Tplt ts) = fromJust $ lab g t -- can also error:
              -- by finding Str or Rel where expected Tplt
            ta = fromRight $ tpltArity te
            newNode = head $ newNodes 1 g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, RelMbr $ snd p) g
            g' =                insEdge (newNode, t, RelTplt)
                              $ insNode (newNode, Rel) g

    chNonRelAtUsf :: Mindmap -> Node -> Expr -> Mindmap
    chNonRelAtUsf g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'
