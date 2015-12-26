    import Data.Graph.Inductive

    type MyGraph = Gr Expr Role
    type Expr = Str String | Rel
    data Role = AsTplt | AsPos Int
      deriving (Show,Read,Eq,Ord)

    g1 = mkGraph
      [   (0, Str "dog")
        , (1, Str "_ wants _ for _")
        , (2, Str "yams")
        , (3, Str "dinner")
        , (4, Rel)
      ] [ (4,1,AsTplt), (4,0, AsPos 1), (4,2,AsPos 2), (4,3,AsPos 3)
      ]

    gelemM :: (Monad m) => MyGraph -> Node -> m ()
    gelemM g n = if gelem n g then return () 
                              else fail "Node not in Graph"

    chStringAt :: MyGraph -> Node -> String -> MyGraph
    chStringAt g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'

    chStringAt' :: (Monad m) => MyGraph -> Node -> String -> m MyGraph
    chStringAt' g n s = do
      gelemM g n
      return $ chStringAt g n s
