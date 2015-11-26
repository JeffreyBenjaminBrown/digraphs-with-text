-- usually folded
  -- TODO
    -- Make another Rel type (called Rel'? RelSpec? RelRequest?)
      -- Rel' = (MmNode, [MmNode]), where data MmNode = MmNode Int | Blank
    -- Add classes for checking arity?
  -- types, vocab, language
    -- Node,Edge: FGL. Expr, Rel: DWT|Mindmap.
    -- how to read edges
      -- in (n,m,lab :: MmLab) :: LEdge MmLab, n is a triplet referring to m
      -- that is, predecessors refer to successors 
        -- (in that kind of relationship they do; maybe there will be others)

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
    type Arity = Int
    data MmExpr = MmString String | Tplt Arity [String] | Rel Arity
      deriving (Show,Read,Eq,Ord)
    data MmEdge = AsTplt | AsPos Arity -- MmEdgeLabel more accurate, but too long
      deriving (Show,Read,Eq,Ord)
    type Mindmap = Gr MmExpr MmEdge

-- build
    insStr :: String -> Mindmap -> Mindmap
    insStr str g = insNode (int, MmString str) g
      where int = head $ newNodes 1 g

    splitTpltStr :: String -> [String]
    splitTpltStr t = map T.unpack $ T.splitOn (T.pack "_") (T.pack t)

    stringToTplt :: String -> MmExpr
    stringToTplt s = Tplt (length ss-1) ss -- even length=0 works
      where ss = splitTpltStr s

    insTplt :: String -> Mindmap -> Mindmap
    insTplt s g = insNode (newNode, stringToTplt s) g
      where newNode = head $ newNodes 1 g

    insRel :: Node -> [Node] -> Mindmap -> Mindmap -- TODO ? return Either
    insRel t ns g = if ti /= length ns -- t is tplt, otherwise like ns
        then error "insRel: Tplt arity /= number of members"
        else f (zip ns [1..ti]) g'
      where Tplt ti ts = fromJust $ lab g t -- TODO: consider case of Nothing?
                                            -- case of Just k, for k not Tplt?
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, AsTplt)
               $ insNode (newNode, Rel ti) g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g

-- query
    users :: Mindmap -> MmEdge -> Int -> Node -> [Node] -- why Int not Arity:
      -- An Arity indicates how many rels; this Int indicates which.
    users g e k n = -- returns all nodes using n in the kth position of e
      let isKAryRel m = lab g m == (Just $ Rel k)
      in [m | (m,n,label) <- inn g n, label == e, isKAryRel m]

    mmRelps :: Mindmap -> [Maybe Node] -> [Node] -- rename match-_
    mmRelps g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, Int)]
            f (Just n, 0) = users g AsTplt    arity n
            f (Just n, k) = users g (AsPos k) arity n
            listIntersect [] = [] -- silly case
            listIntersect (x:xs) = foldl intersect x xs

-- view
    subInTplt :: MmExpr -> [String] -> String
    subInTplt (Tplt k ts) ss = let pairList = zip ts $ ss ++ [""] 
        --append [""] because there are n+1 segments in an n-ary Tplt
      in foldl (\s (a,b) -> s++a++b) "" pairList
    subInTplt _ _ = error "subInTplt: MmExpr not a Tplt"

    showExpr :: Mindmap -> Node -> String -- BEWARE ? infinite loops
     -- if the graph is recursive, this could infinite loop
       -- although such graphs seem unlikely, because recursive statements are
       -- difficult; c.f. Godel's impossibility theorem
     -- a solution: while building, keep list of visited nodes
       -- if visiting one already there, display it as just its number
       -- or number + "already displayed higher in this (node view?)"    
    showExpr g n = case lab g n of
      Nothing -> error $ "showExpr: node " ++ (show n) ++ " not in graph"
      Just (MmString s) -> prefixNode s
      Just (Tplt k ts) -> prefixNode $ "Tplt: "
        ++ intercalate "_" ts
      Just (Rel _) -> 
        let ledges = sortOn (\(_,_,l)->l) $ out g n
            (_,tpltNode,_) = head ledges
              -- head because Tplt sorts first, before Rel, in Ord MmExpr 
            Just tpltLab = lab g tpltNode :: Maybe MmExpr
            members = map (\(_,m,_)-> m) $ tail ledges :: [Node]
        in prefixNode $ subInTplt tpltLab $ map (bracket . showExpr g) members
      where prefixNode s = (show n) ++ ": " ++ s
            bracket s = "[" ++ s ++ "]"

    view :: Mindmap -> [Maybe Node] -> IO ()
    view g mns = mapM_ putStrLn 
                $ map (showExpr g) $ mmRelps g mns
