    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Wiz where
    import Data.Graph.Inductive
    import Dwt.Graph
    import Dwt.Show
    import qualified Text.Read as R
    import qualified Data.Maybe as Mb

    data Cmd = Id | InsWord | InsTplt | InsRel | Quit deriving (Show, Read) --Eq
    type State = RSLT

    wiz :: State -> IO State
    wiz g = do
      putStrLn "Command me!"
      cmd <- (\s->read s::Cmd) <$> getLine
      case cmd of 
        Id -> wiz g
        Quit -> return g
        InsTplt -> tryStateIO "Enter a Tplt to add"
                         ((>0) . length . filter (=='_'))
                         insTplt g
        InsWord ->  tryStateIO "Enter a Str to add"
                          (const True)
                          insWord g
        InsRel -> do h <- insRelWiz g; wiz h

    tryStateIO :: String                   -> (String-> Bool)
          -> (String-> State-> State) -> State -> IO State
    tryStateIO askUser good change g = do
      putStrLn askUser
      s <- getLine
      case good s of False -> do putStrLn "No seriously!"
                                 tryStateIO askUser good change g
                     True -> wiz $ change s g

    getTpltAddrWiz :: RSLT -> IO Node
    getTpltAddrWiz g = do
      putStrLn "Enter the address of a Template."
      s <- getLine
      case (R.readMaybe s :: Maybe Node) of 
        Nothing -> do putStrLn "Not an address!"; getTpltAddrWiz g
        Just n -> case lab g n of 
          Nothing -> do putStrLn "Address absent."; getTpltAddrWiz g
          Just (Tplt _) -> return n
          Just _ -> do putStrLn "Not a template!"; getTpltAddrWiz g

    getMbrListWiz :: RSLT -> IO [Node]
    getMbrListWiz g = do
      putStrLn "Enter member addresses, separated by space."
      mns <- map (\a -> R.readMaybe a :: Maybe Node) <$> words <$> getLine
      case or $ map Mb.isNothing mns of 
        True -> do putStrLn "Not a list of addresses!"; getMbrListWiz g
        False -> let ns = Mb.catMaybes mns in case and $ map (flip gelem g) ns of
          True -> return ns
          False -> do putStrLn "Those are not all in the graph!"; getMbrListWiz g

    insRelWiz :: State -> IO State
    insRelWiz g = do 
      tn <- getTpltAddrWiz g
      ns <- getMbrListWiz g
      let mbExpr = lab g tn
      case mbExpr of Nothing -> error "By getTpltAddrWiz, this is impossible."
                     Just tplt -> if tpltArity tplt == length ns
                                  then case insRel tn ns g of
                                         Right h -> return h
                                         _ -> error "Impossible, by wizzes."
                                  else do putStrLn "Arity mismatch!"; insRelWiz g
