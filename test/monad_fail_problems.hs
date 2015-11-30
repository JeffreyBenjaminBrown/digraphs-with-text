    import Data.Graph.Inductive -- FGL, the Functional Graph Library

    type MyGraph = Gr String String

    tinyGraph = mkGraph [(0, "dog")] [] :: MyGraph

    maybeSucceed = replaceStringAtNodeM tinyGraph 0 "cat" :: Maybe MyGraph
      -- == Just (mkGraph [(0,"cat")] [])
    maybeFail = replaceStringAtNodeM tinyGraph 1 "cat" :: Maybe MyGraph
      -- == Nothing

    eitherSucceed = replaceStringAtNodeM tinyGraph 0 "cat" :: Either String MyGraph
      -- ==  Right (mkGraph [(0,"cat")] [])
    eitherFail = replaceStringAtNodeM tinyGraph 1 "cat" :: Either String MyGraph
      -- *** Exception: Node not in Graph

    gelemM :: (Monad m) => MyGraph -> Node -> m ()
    gelemM g n = if gelem n g       -- FGL's gelem function returns
      then return ()                  -- True if the node is in the graph
      else fail "Node not in Graph"   -- False otherwise

    replaceStringAtNode :: MyGraph -> Node -> String -> MyGraph
    replaceStringAtNode g n e = let (Just (a,b,c,d),g') = match n g
      in (a,b,e,d) & g'

    replaceStringAtNodeM :: (Monad m) => MyGraph -> Node -> String -> m MyGraph
    replaceStringAtNodeM g n s = do
      gelemM g n
      return $ replaceStringAtNode g n s
        -- if evaluated, the pattern match in replaceStringAtNode (Just ...) must succeed,
        -- because gelemM catches the case where n is not in the graph
