    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Wiz where
    import Data.Graph.Inductive
    import Dwt.Graph
    import Dwt.Show

    data Cmd = Id | InsTplt | Quit deriving (Show, Read) --Eq
    type State = RSLT

    wiz :: State -> IO State
    wiz g = do
      putStrLn "Command me!"
      cmd <- (\s->read s::Cmd) <$> getLine
      case cmd of Id -> wiz g
                  Quit -> return g
                  InsTplt -> tryIO ((>0) . length . filter (=='_'))
                                   insTplt g

    tryIO :: (String-> Bool)
          -> (String-> State-> State) -> State -> IO State
    tryIO good change g = do
      s <- getLine
      case good s of False -> do putStrLn "No seriously!"
                                 tryIO good change g
                     True -> wiz $ change s g

    testTryIO = tryIO ((>0) . length . filter (=='_'))
                      insTplt (empty :: RSLT)

    -- need: something that gets user input, tests against "!q", aborts current command if yes, and proceeds otherwise
    -- need: a function of type (String -> Bool) -> (State->IO State) -> State -> IO State, where State = (RSLT, CommandStack), which tests the user input with the first arg, and if Bool, 

    insTpltWiz :: RSLT -> IO RSLT
    insTpltWiz g = do
      putStrLn "Describe a template."
      s <- getLine
      return $ insTplt s g

--it works!
-- > wiz $ (empty :: RSLT)
--Command me!
--Id
--Command me!
--InsTplt 
--_ needs _
--Command me!
--Quit
--mkGraph [(0,Tplt ["","needs",""])] []
-- >
