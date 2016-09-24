    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Wiz where
    import Data.Graph.Inductive
    import Dwt.Graph
    import Dwt.Show

    data Cmd = Id | InsWord | InsTplt | InsRel | Quit deriving (Show, Read) --Eq
    type State = RSLT

    wiz :: State -> IO State
    wiz g = do
      putStrLn "Command me!"
      cmd <- (\s->read s::Cmd) <$> getLine
      case cmd of 
        Id -> wiz g
        Quit -> return g
        InsTplt -> tryIO "Enter a Tplt to add"
                         ((>0) . length . filter (=='_'))
                         insTplt g
        InsWord ->  tryIO "Enter a Str to add"
                          (const True)
                          insWord g
        -- InsRel -> insWordWiz g

    -- specifically State \vart IO
    tryIO :: String                   -> (String-> Bool)
          -> (String-> State-> State) -> State -> IO State
    tryIO askUser good change g = do
      putStrLn askUser
      s <- getLine
      case good s of False -> do putStrLn "No seriously!"
                                 tryIO askUser good change g
                     True -> wiz $ change s g

--    insRelWiz :: RSLT -> IO RSLT
--    insRelWiz g = do
--      putStrLn "Enter the address of a Template for the new Rel."
--      n <- (\s->read s::Node) <$> getLine
--      case gelem n g of False -> do "No, seriously!"; wordWiz g
--                        True -> 
--
    -- tryReadNode :: String -> IO Node
    -- tryReadNode askUser = ...
