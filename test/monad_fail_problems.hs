    import Data.Graph.Inductive

    type MyGraph = Gr String String

    tinyGraph = mkGraph
      [   (0, "dog")
        , (2, "yams")
      ] [] :: MyGraph

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

    maybeSucceed = chStringAt' tinyGraph 0 "cat" :: Maybe MyGraph
    maybeFail = chStringAt' tinyGraph 1 "cat" :: Maybe MyGraph

    eitherSucceed = chStringAt' tinyGraph 0 "cat" :: Either String MyGraph
    eitherFail = chStringAt' tinyGraph 1 "cat" :: Either String MyGraph
      -- why is eitherFail an Exception, when maybeFail is a Nothing?
