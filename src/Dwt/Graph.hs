-- TODO: find an automatic way to check for unused functions
  -- otherwise when pattern matching, if the function name handed one of the 
  -- patterns is misspelled, I can get a "non-exhaustive patterns" exception

-- pragmas, export, import
    {-# LANGUAGE FlexibleContexts #-}

    module Dwt.Graph
      ( module Data.Graph.Inductive
      , Arity, RelPos, Expr(..), Role(..), Mindmap
      , splitTpltStr, stringToTplt, subInTplt -- Tplt
      , insStr, insTplt, insRel -- build Mindmap
      , chExprAt, compressGraph -- edit Mindmap
      -- query Mindmap
        , gelemM, tpltAt, tpltArity, nodesMatchTplt -- minor
        , users, specUsersUsf, specUsers, matchRel, allRels -- .. -> [Node]
      , insRelUsf, chExprAtUsf, usersUsf -- unsafe, duplicates
      ) where

    import Data.Graph.Inductive
    import Data.Either (partitionEithers)
    import Data.List (intersect)
    import qualified Data.Map as Map
    import Data.Maybe (isJust, catMaybes, fromJust)
    import Control.Monad (mapM_)
    import Control.Monad.Except (MonadError, throwError)
    import Data.Text (splitOn, pack, unpack)

-- types
    type Arity = Int -- relationships, which some expressions are, have arities
    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    data Expr = Str String | Tplt Arity [String] | Rel Arity
      deriving (Show,Read,Eq,Ord)
    data Role = RelTplt | RelMbr RelPos
      deriving (Show,Read,Eq,Ord)
    type Mindmap = Gr Expr Role

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

  -- edit
    chExprAt :: (MonadError String m) => Mindmap -> Node -> Expr -> m Mindmap
    chExprAt g n e = do -- TODO: test. TODO? absorb def of chExprAtUsf.
      gelemM g n
      return $ chExprAtUsf g n e

    compressGraph :: DynGraph gr => gr a b -> gr a b
    compressGraph g = let ns = nodes g
                          ns' = [1 .. length ns]
                          mp = Map.fromList $ zip ns ns'
                          chNode n = mp Map.! n
                          chAdj (b,n) = (b, mp Map.! n)
      in gmap (\(a,b,lab,d) -> (map chAdj a, chNode b, lab, map chAdj d)) g

    -- chMbr :: Role -> Node -> Node -> Mindmap -> Mindmap
    -- chMbr role newMbr user g = ... -- TODO

-- query
  -- tests and lookups for smaller-than-graph types
    gelemM :: (MonadError String m) => Mindmap -> Node -> m ()
    gelemM g n = if gelem n g then return () 
                              else throwError $ "gelemM: Node "
                                   ++ show n ++ " not in Mindmap"

    tpltAt :: (MonadError String m) => Mindmap -> Node -> m Expr -- TODO test
    tpltAt g tn = case lab g tn of 
      Just t@(Tplt a b) -> return $ t
      Nothing -> throwError $ "tpltAt: Node " ++ show tn ++ "not in Mindmap"
      _ -> throwError $ "tpltAt: Node " ++ show tn ++ " indexes not a Tplt"

    tpltArity :: (MonadError String m) => Expr -> m Arity
    tpltArity e = case e of Tplt a _ -> return a
                            _        -> throwError "tpltArity: Expr not a Tplt"

    nodesMatchTplt :: (MonadError String m) => [Node] -> Expr -> m () -- TODO test
    nodesMatchTplt ns e = case e of
      Tplt k _ -> if k /= length ns 
        then throwError "nodesMatchTplt: Tplt Arity /= number of member Nodes"
        else return ()
      _ -> throwError "nodesMatchTplt: Expr not a Tplt"

  -- .. -> [Node]
    users :: (MonadError String m) => Mindmap -> Node -> m [Node]
    users g n = do gelemM g n
                   return $ [m | (m,n,label) <- inn g n]

    specUsersUsf :: Mindmap -> Role -> Arity -> Node -> [Node] --TODO test
    specUsersUsf g r k n = -- all k-ary Rels using Node n in Role r
      let isKAryRel m = lab g m == (Just $ Rel k)
      in [m | (m,n,r') <- inn g n, r' == r, isKAryRel m]

    specUsers :: (MonadError String m) => -- TODO: test
      Mindmap -> Role -> Arity -> Node -> m [Node]
    specUsers g r k n = do -- all k-ary Rels using Node n in Role r
      gelemM g n
      return $ let isKAryRel m = lab g m == (Just $ Rel k)
        in [m | (m,_,r') <- inn g n, r' == r, isKAryRel m]
          -- the _ is always n

    matchRel :: Mindmap -> [Maybe Node] -> [Node]
    matchRel g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, RelPos)]
            f (Just n, 0) = specUsersUsf g RelTplt    arity n
            f (Just n, k) = specUsersUsf g (RelMbr k) arity n
            listIntersect [] = []
            listIntersect (x:xs) = foldl intersect x xs

    allRels :: Mindmap -> Node -> [Node]
    allRels = pre

-- deprecating: non-monadic, unsafe, duplicate functions
    insRelUsf :: Node -> [Node] -> Mindmap -> Mindmap
    insRelUsf t ns g = if ti /= length ns -- t is tplt, otherwise like ns
        then error "insRelUsf: Tplt Arity /= number of members Nodes"
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

    chExprAtUsf :: Mindmap -> Node -> Expr -> Mindmap
    chExprAtUsf g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'

    usersUsf :: Mindmap ->  Node -> [Node] -- TODO: test
    usersUsf g n = [m | (m,n,label) <- inn g n]
