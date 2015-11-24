-- usually folded
  -- TODO ? Make another Rel type
    -- Rel' = (MmNode, [MmNode]), where data MmNode = MmNode Int | Blank
  -- TODO ? classes for checking arity
  -- TODO ? make "arity" a type
      -- its support is the integers in [1,k]
      -- AsPositions and Rels contain one
        -- or AsPosition contains a number that varies in [1,k]
          -- where k is the arity
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

-- types
    data MmExpr =  MmString String | Tplt Int String | Rel Int
      -- TODO: use Tplt
      deriving (Show,Read,Eq,Ord)

    data MmEdge = AsTplt | AsPos Int -- TODO ? MmEdge -> MmEdgeLab
        -- hide this type from user
      deriving (Show,Read,Eq,Ord) -- Ord: AsTplt < AsPos _ 

    type Mindmap = Gr MmExpr MmEdge

-- build
    insStr :: String -> Mindmap -> Mindmap
    insStr str g = insNode (int, MmString str) g
      where int = head $ newNodes 1 g

    countHoles :: String -> Int
    countHoles ('_':s) = 1 + countHoles s
    countHoles (_:s) = countHoles s
    countHoles "" = 0

    insTplt :: String -> Mindmap -> Mindmap
    insTplt s g = insNode (int, Tplt (countHoles s) s) g
      where int = head $ newNodes 1 g

    insRel :: Node -> [Node] -> Mindmap -> Mindmap
    insRel t ns g = f (zip ns [1..len]) g' -- t is like ns but tplt
      where len = length ns
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, AsTplt)
               $ insNode (newNode, Rel len) g
            f []     g = g -- a fold would be briefer
            f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g

    insRel' :: Node -> [Node] -> Mindmap -> Mindmap -- TODO: Either Str Mm
      -- TODO: throw exception if length ns /= ti
    insRel' t ns g = if ti /= length ns 
        then error "Tplt arity /= number of members"
        else f (zip ns [1..ti]) g' -- t is tplt, otherwise like ns
      where Tplt ti ts = fromJust $ lab g t -- TODO: consider Nothing case? 
            -- consider Just k case, for k not Tplt?
            newNode = head $ newNodes 1 g
            g' = insEdge (newNode, t, AsTplt)
               $ insNode (newNode, Rel ti) g
            f []     g = g
            f (p:ps) g = f ps $ insEdge (newNode, fst p, AsPos $ snd p) g

-- query
    mmReferents :: Mindmap -> MmEdge -> Int -> Node -> [Node]
    mmReferents g e arity n = -- all uses(of a type specd by e & arity) of n
      let isKAryRel m = lab g m == (Just $ Rel arity)
      in [m | (m,n,label) <- inn g n, label == e, isKAryRel m]

    mmRelps :: Mindmap -> [Maybe Node] -> [Node] -- TODO: Exhaust patterns
      -- currently the pattern [Nothing] does not match
      -- TODO: provide the Tplt as a separate Maybe Node, not the first in list
    mmRelps g mns = listIntersect $ map f jns
      where arity = length mns - 1
            jns = filter (isJust . fst) $ zip mns [0..] :: [(Maybe Node, Int)]
            f (Just n, 0) = mmReferents g AsTplt    arity n
            f (Just n, k) = mmReferents g (AsPos k) arity n
            listIntersect (x:xs) = foldl intersect x xs

-- view
    showExpr :: Mindmap -> Node -> Either String String -- WARNING|TODO
      -- if the graph is recursive, could this infinite loop?
        -- yes, but is that kind of graph probable?
    showExpr g n = case lab g n of
      Nothing -> Left $ "node " ++ (show n) ++ " not in graph"
      Just (MmString s) -> Right $ prefixNode s
      Just (Tplt k s) -> Right $ "Tplt: " ++ prefixNode s
      Just (Rel _) -> Right $ prefixNode $ intercalate ", " 
           $ (\(a,b)->a++b) $ partitionEithers $ map f
           $ sortOn (\(_,_,l)->l) $ out g n
        where f (n,m,label) = showExpr g m
      where prefixNode s = (show n) ++ ": " ++ s

    view :: Mindmap -> [Maybe Node] -> IO () -- TODO ? test
    view g mns = mapM_ putStrLn $ map (eitherString . showExpr g) $ mmRelps g mns
      where eitherString (Left s) = s
            eitherString (Right s) = s

-- EOF
