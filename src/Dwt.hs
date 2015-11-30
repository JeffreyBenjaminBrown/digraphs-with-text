-- usually folded
  -- NOTES TO A READER
    -- how Dwt.Exprs and Dwt.Rels are made from FGL.Nodes and FGL.Edges
      -- This 10-line file explains it best:
        -- data/minimalGraph.hs
      -- Rels instantiate Tplts
        -- For example, the relationship "flower needs watering" 
        -- instantiates the template "_ needs _"
      -- how to read edges
        -- predecessor Rels are built from their successor Exprs
        -- in (n,m,lab :: MmLab) :: LEdge MmLab, n is a Rel referring to m
          -- usually, m is a Str, but it could be a Rel or even a Tplt
    -- abbreviations
      -- ch = change
      -- mbr = member
        -- in a a k-ary Rel, there are k AsPos Roles for k members to play,
        -- plus one more Role for the Tplt (which must be k-ary) to play
      -- tplt = (relationship) template
      -- rel = relationship
      -- pos = position
  -- NOTES TO SELF
    -- make|keep my Node|Label notation consistent with tradition
    -- Kinds of view
      -- e.g. with Nodes, without
    -- Delete LNode
    -- Make another Rel type (called Rel'? RelSpec? RelRequest?)
      -- Rel' = (MmNode, [MmNode]), where data MmNode = MmNode Int | Blank
    -- ? Add [classes?] for checking arity

-- export & import
    module Dwt
      ( -- exports:
      module Data.Graph.Inductive -- export for testing, not production
      , module Dwt -- exports everything in this file
      -- , module Dwt.Graph -- etc. Will need to import below to match.
      ) where    
    import Data.Graph.Inductive
    import Data.String (String)
    import Data.Either (partitionEithers)
    import Data.List (intersect, sortOn, intercalate)
    import Data.Maybe (isJust, catMaybes, fromJust)
    import Control.Monad (mapM_)
    import qualified Data.Text as T

-- types
    type Arity = Int -- relationships, which some expressions are, have arities
    type RelPos = Int -- the k members of a k-ary Rel take RelPos values [1..k]
    data Expr = Str String | Tplt Arity [String] | Rel Arity
      deriving (Show,Read,Eq,Ord)
    data Role = AsTplt | AsPos RelPos
      deriving (Show,Read,Eq,Ord)
    type Mindmap = Gr Expr Role

-- build
  -- Tplt <-> String
    splitTpltStr :: String -> [String]
    splitTpltStr t = map T.unpack $ T.splitOn (T.pack "_") (T.pack t)

    stringToTplt :: String -> Expr
    stringToTplt s = Tplt (length ss-1) ss -- even length=0 works
      where ss = splitTpltStr s

    subInTplt :: Expr -> [String] -> String
    subInTplt (Tplt k ts) ss = let pairList = zip ts $ ss ++ [""] -- append "" because there are n+1 segments in an n-ary Tplt; zipper ends early otherwise
      in foldl (\s (a,b) -> s++a++b) "" pairList

  -- insert
   -- node-free
    insStr :: String -> Mindmap -> Mindmap
    insStr str g = insNode (int, Str str) g
      where int = head $ newNodes 1 g

    insTplt :: String -> Mindmap -> Mindmap
    insTplt s g = insNode (newNode, stringToTplt s) g
      where newNode = head $ newNodes 1 g

   -- involving Nodes
    insRel :: Node -> [Node] -> Mindmap -> Mindmap
    insRel t ns g = if ti /= length ns -- t is tplt, otherwise like ns
        then error "insRel: Tplt Arity /= number of members Nodes"
        else if any (==False) $ map (flip gelem g) $ (t:ns)
          then error "insRel: One of those Nodes is not in the Mindmap." 
        else f (zip ns [1..ti]) g'
      where Tplt ti ts = fromJust $ lab g t -- can also error:
              -- by finding Str or Rel where expected Tplt
            newNode = head $ newNodes 1 g
            f []     g = g -- TODO ? use fold instead
            f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g
            g' =                insEdge (newNode, t, AsTplt)
                              $ insNode (newNode, Rel ti) g

    insRel' :: (Monad m) => Node -> [Node] -> Mindmap -> m Mindmap
      -- BROKEN; compiles but throws an exception (see test/Spec.hs/tInsRel')
      -- IN PROGRESS : Either|Maybe
      -- TODO : EMULATE Jake's "move" function in tictactoe
    insRel' t ns g = do
      if any (==False) $ map (flip gelem g) $ ns
        then fail "insRel: One of those Nodes is not in the Mindmap." 
        else return ()
      case tplt of
        Tplt ti ts -> do
          if ti /= length ns
            then fail "insRel: Tplt arity /= number of members"
            else return ()
          return $ f (zip ns [1..ti]) g
          where newNode = head $ newNodes 1 g
                f []     g = g -- TODO ? use fold instead
                f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g
                g' =                insEdge (newNode, t, AsTplt)
                                  $ insNode (newNode, Rel ti) g
        _ -> fail "insRel: Tplt Node argument indexes not a Tplt"
        where tplt = fromJust $ lab g t

   -- insRel''
    isIn :: (Monad m) => Mindmap -> Node -> m ()
    isIn g n = if gelem n g then return () else fail "Node not in Mindmap"

    tpltAt :: (Monad m) => Mindmap -> Node -> m Expr -- Expr is always a Tplt
    tpltAt g tn = case lab g tn of Just t@(Tplt a b) -> return $ t
                                   Nothing  -> fail "Node not in Mindmap"
                                   _        -> fail "Node does not index a Tplt"

    nodesMatchTplt :: (Monad m) => [Node] -> Expr -> m ()
    nodesMatchTplt ns e = case e of
      Tplt k _ -> if k /= length ns 
        then fail "Tplt Arity /= number of member Nodes"
        else return ()
      _ -> fail "Expr not a Tplt"

    tpltArity :: (Monad m) => Expr -> m Arity
    tpltArity e = case e of Tplt a _ -> return a
                            _ -> fail "Not a Tplt, therefore has no Arity"

    insRel'' tn ns g = -- TODO !!! Test
      do mapM_ (isIn g) ns
         t <- tpltAt g tn
         nodesMatchTplt ns t
         a <- tpltArity t
         return $ let 
             newNode = head $ newNodes 1 g
             f []     g = g -- TODO ? use fold instead
             f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g
             g' =                insEdge (newNode, tn, AsTplt)
                               $ insNode (newNode, Rel a) g
           in f (zip ns [1..a]) g

  -- edit ("ch" = "change")
    chLNode :: Mindmap -> Node -> Expr -> Mindmap -- TODO: Either|Maybe
    chLNode g n me = let (Just (a,b,c,d),g') = match n g
      in (a,b,me,d) & g'

    -- chMbr :: Role -> Node -> Node -> Mindmap -> Mindmap -- TODO
    -- chMbr role newMbr user g = ...

-- query
    users :: Mindmap ->  Node -> [Node]
    users g n = [m | (m,n,label) <- inn g n]

    specUsers :: Mindmap -> Role -> Arity -> Node -> [Node]
    specUsers g r k n = -- all k-ary Rels using Node n in Role r
      let isKAryRel m = lab g m == (Just $ Rel k)
      in [m | (m,n,r') <- inn g n, r' == r, isKAryRel m]

    matchRel :: Mindmap -> [Maybe Node] -> [Node]
    matchRel g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, RelPos)]
            f (Just n, 0) = specUsers g AsTplt    arity n
            f (Just n, k) = specUsers g (AsPos k) arity n
            listIntersect [] = [] -- silly case
            listIntersect (x:xs) = foldl intersect x xs

-- view
    -- TODO: showExpr should show Template as ":[Node] bla _ bla _ bla"
      -- where [Node] is a single integer, not a list
    showExpr :: Mindmap -> Node -> String -- TODO: Either|Maybe
      -- BEWARE ? infinite loops
        -- if the graph is recursive, this could infinite loop
          -- although such graphs seem unlikely, because recursive statements are
          -- difficult; c.f. Godel's impossibility theorem
        -- a solution: while building, keep list of visited nodes
          -- if visiting one already there, display it as just its number
          -- or number + "already displayed higher in this (node view?)"    
    showExpr g n = case lab g n of
      Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
      Just (Str s) ->     prependNode s
      Just (Tplt k ts) -> prependNode $ "Tplt: " ++ intercalate "_" ts
      Just (Rel _) ->
        let ledges = sortOn edgeLabel $ out g n
            (_,tpltNode,_) = head ledges
              -- head because Tplt sorts first, before Rel, in Ord Expr 
            Just tpltLab = lab g tpltNode :: Maybe Expr
            members = map (\(_,m,_)-> m) $ tail ledges :: [Node]
        in prefixRel tpltNode $ subInTplt tpltLab 
             $ map (bracket . showExpr g) members
      where prependNode s = (show n) ++ ": " ++ s
            prefixRel tn s = show n ++ ":" ++ show tn ++ " " ++ s
            bracket s = "[" ++ s ++ "]"

    view :: Mindmap -> [Node] -> IO ()
    view g ns = mapM_ putStrLn $ map (showExpr g) ns
