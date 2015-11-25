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
  -- IN PROGRESS: refactoring Tplt to hold a list of Strings
    type Arity = Int
    data MmExpr =   MmString String | Tplt Arity String    | Rel Arity
      deriving (Show,Read,Eq,Ord)
    data MmExpr' = MmString' String | Tplt' Arity [String] | Rel' Arity
      deriving (Show,Read,Eq,Ord)
    data MmEdge = AsTplt | AsPos Arity -- MmEdgeLabel more accurate, but too long
      deriving (Show,Read,Eq,Ord)
    type Mindmap =  Gr MmExpr  MmEdge
    type Mindmap' = Gr MmExpr' MmEdge

-- build
    insStr :: String -> Mindmap -> Mindmap
    insStr str g = insNode (int, MmString str) g
      where int = head $ newNodes 1 g

    insStr' :: String -> Mindmap' -> Mindmap' -- unchanged but for type
    insStr' str g = insNode (int, MmString' str) g
      where int = head $ newNodes 1 g

    splitTpltStr :: String -> [String]
    splitTpltStr t = map T.unpack $ T.splitOn (T.pack "_") (T.pack t)

    stringToTplt :: String -> MmExpr'
    stringToTplt s = Tplt' (length ss-1) ss -- even length=0 works
      where ss = splitTpltStr s

    insTplt' :: String -> Mindmap' -> Mindmap' -- TODO: use stringToTplt
    insTplt' s g = insNode (newNode, stringToTplt s) g
      where newNode = head $ newNodes 1 g

    insTplt :: String -> Mindmap -> Mindmap
    insTplt s g = insNode (newNode, Tplt (countHoles s) s) g
      where newNode = head $ newNodes 1 g
            countHoles = length . filter (== '_') :: String -> Int

    insRel :: Node -> [Node] -> Mindmap -> Mindmap -- TODO ? return Either Str Mm
    insRel t ns g = if ti /= length ns -- t is tplt, otherwise like ns
        then error "Tplt arity /= number of members"
        else f (zip ns [1..ti]) g'
      where Tplt ti ts = fromJust $ lab g t -- TODO: consider case of Nothing?
                                            -- case of Just k, for k not Tplt?
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, AsTplt)
               $ insNode (newNode, Rel ti) g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g

    insRel' :: Node -> [Node] -> Mindmap' -> Mindmap' -- TODO ? return Either
      -- this function is unchanged from insRel except in its type sig
    insRel' t ns g = if ti /= length ns -- t is tplt, otherwise like ns
        then error "Tplt arity /= number of members"
        else f (zip ns [1..ti]) g'
      where Tplt' ti ts = fromJust $ lab g t -- TODO: consider case of Nothing?
                                            -- case of Just k, for k not Tplt?
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, AsTplt)
               $ insNode (newNode, Rel' ti) g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g

-- query. TODO: Duplicate for MmExpr'
    mmReferents :: Mindmap -> MmEdge -> Arity -> Node -> [Node]
    mmReferents g e k n = -- returns all uses (of a type specified by e & k) of n
      let isKAryRel m = lab g m == (Just $ Rel k)
      in [m | (m,n,label) <- inn g n, label == e, isKAryRel m]

    mmRelps :: Mindmap -> [Maybe Node] -> [Node]
    mmRelps g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, Int)]
            f (Just n, 0) = mmReferents g AsTplt    arity n
            f (Just n, k) = mmReferents g (AsPos k) arity n
            listIntersect [] = [] -- silly case
            listIntersect (x:xs) = foldl intersect x xs

-- view
    subInTplt' :: MmExpr' -> [String] -> String
    subInTplt' (Tplt' k ts) ss = let pairList = zip ts $ ss ++ [""] --append [""] because there are n+1 segments in an n-ary Tplt
      in foldl (\s (a,b) -> s++a++b) "" pairList
    subInTplt' _ _ = error "MmExpr not a Tplt"

    subInTplt :: String -> [String] -> String -- TODO: Tplt be already split
    subInTplt t ss = let tpltAsList = splitTpltStr t
                         pairList = zip tpltAsList $ ss ++ [""] --append [""] because there are n+1 segments in an n-ary Tplt
      in foldl (\s (a,b) -> s++a++b) "" pairList

    showExpr' :: Mindmap' -> Node -> String -- TODO: BUGGY
       -- replacing showExpr; this one is Tplt'-aware
     -- WARNING|TODO
     -- if the graph is recursive, could this infinite loop?
        -- (the answer is yes, but is that kind of graph probable?)
        -- a solution: while building, keep list of visited nodes
            -- if visiting one already there, display it as just its number
            -- or number + "already displayed higher in this (node view?)"    
    showExpr' g n = case lab g n of
      Nothing -> error $ "node " ++ (show n) ++ " not in graph"
      Just (MmString' s) -> prefixNode s
      Just (Tplt' k ts) -> prefixNode $ "Tplt: "
        ++ intercalate "_" ts
      Just (Rel' _) -> let
            ledges = sortOn (\(_,_,l)->l) $ out g n
            (_,tn,_) = head ledges
              -- head because Tplt sorts first, before Rel, in Ord MmExpr 
            Just t = lab g tn :: Maybe MmExpr'
            members = map (\(_,m,_)-> m) $ tail ledges :: [Node]
        in subInTplt' t $ map (\(_,m,_) -> showExpr' g m) ledges
      where prefixNode s = (show n) ++ ": " ++ s

    showExpr :: Mindmap -> Node -> Either String String -- TODO ? cyc-robust
      -- if the graph is recursive, this could infinite loop
        -- although that kind of mindmap is unlikely, difficult
          -- c.f. Godel's impossibility theorem
      -- a solution: while building, keep list of visited nodes
        -- if visiting one already there, display it as just its number
        -- or number + "already displayed higher in this (node view?)"
    showExpr g n = case lab g n of
      Nothing -> Left $ "node " ++ (show n) ++ " not in graph"
      Just (MmString s) -> Right $ prefixNode s
      Just (Tplt k s) -> Right $ prefixNode $ "Tplt: " ++  s
      Just (Rel _) -> Right $ prefixNode $ intercalate ", " 
           $ (\(a,b) -> a++b) $ partitionEithers $ map f
           $ sortOn (\(_,_,l)->l) $ out g n
        where f (n,m,label) = showExpr g m
      where prefixNode s = (show n) ++ ": " ++ s

    view :: Mindmap -> [Maybe Node] -> IO ()
    view g mns = mapM_ putStrLn 
               $ map (eitherString . showExpr g) $ mmRelps g mns
      where eitherString (Left s) = s
            eitherString (Right s) = s
