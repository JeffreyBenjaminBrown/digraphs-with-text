    import Data.Graph.Inductive

    type MyGraph = Gr Expr Role
    type Expr = Word String | Rel
    data Role = AsTplt | AsPos Int
      deriving (Show,Read,Eq,Ord)

    g1 = mkGraph
      [   (0, Word "dog")
        , (1, Word "_ wants _ for _")
        , (2, Word "yams")
        , (3, Word "dinner")
        , (4, Rel)
      ] [ (4,1,AsTplt), (4,0, AsPos 1), (4,2,AsPos 2), (4,3,AsPos 3)
      ]

    gelemMStrErr :: (Monad m) => MyGraph -> Node -> m ()
    gelemMStrErr g n = if gelem n g then return () 
                              else fail "Node not in Graph"

    chStringAt :: MyGraph -> Node -> String -> MyGraph
    chStringAt g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'

    chStringAt' :: (Monad m) => MyGraph -> Node -> String -> m MyGraph
    chStringAt' g n s = do
      gelemMStrErr g n
      return $ chStringAt g n s
