    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Wiz where
    import Data.Graph.Inductive
    import Dwt.Graph
    import Dwt.Show
    import qualified Data.Maybe as Mb

    import Control.Monad (void)
    import Text.Megaparsec
    import Text.Megaparsec.String -- input stream is of type ‘String’
    import qualified Text.Megaparsec.Lexer as L

    import qualified Text.Read as R -- trying not to need

--
    data Cmd = InsWord | InsTplt | InsRel | Quit deriving (Show, Read)
    type WizState = RSLT

-- Parser utilities
    sc :: Parser ()
    sc = L.space (void spaceChar) lineCmnt blockCmnt
      where lineCmnt  = L.skipLineComment "//"
            blockCmnt = L.skipBlockComment "/*" "*/"
    
    symbol :: String -> Parser String
    symbol = L.symbol sc

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    integer :: Parser Integer
    integer = lexeme L.integer

-- Read a command name.
    commandAliases :: [(String, Cmd)]
    commandAliases = [ ("iw", InsWord)
                     , ("it", InsTplt)
                     , ("ir", InsRel)
                     , ("q",  Quit)
                     ]
    
    parseCmd :: Parser Cmd
    parseCmd = sc *> foldl1 (<|>) parsers <* sc where
      parsers = map f commandAliases where
        f (name,cmd) = try $ symbol name *> pure cmd

-- IO
    retry :: String -> IO a -> IO a
    retry msgToUser f = do putStrLn msgToUser;  f

    wiz :: WizState -> IO WizState
    wiz g = do
      putStrLn "Go!"
      mbCmd <- parseMaybe parseCmd <$> getLine
      maybe (retry "Command not understood." $ wiz g) f mbCmd where
        f cmd = case cmd of 
          Quit -> return g
          InsTplt -> tryWizStateIO "Enter a Tplt to add."
                           ((>0) . length . filter (=='_'))
                           insTplt g
          InsWord -> do putStrLn "Enter a Word (any string, really) to add."
                        s <- getLine
                        wiz $ insWord s g
--          InsWord ->  tryWizStateIO "Enter a Word (any string, really) to add."
--                            (const True)
--                            insWord g
          InsRel -> do h <- insRelWiz g; wiz h

    -- ?? bad
    tryWizStateIO :: String
          -> (String-> Bool) -- is the input good?
          -> (String-> WizState-> WizState) -- how to change the wizstate
          -> WizState -> IO WizState
    tryWizStateIO askUser good change g = do
      putStrLn askUser
      s <- getLine
      case good s of False -> do putStrLn "No seriously!"
                                 tryWizStateIO askUser good change g
                     True -> wiz $ change s g

    getTpltAddrWiz :: RSLT -> IO Node
    getTpltAddrWiz g = do
      putStrLn "Enter the address of a relationship Template."
      s <- fmap fromIntegral . parseMaybe (sc *> L.integer <* sc) <$> getLine
      case s of 
        Nothing -> retry "Address = integer." $ getTpltAddrWiz g
        Just n -> case lab g n of 
          Just (Tplt _) -> return n
          Nothing -> retry "Address absent." $ getTpltAddrWiz g
          Just _ -> retry "Address holds not a Template." $ getTpltAddrWiz g

    getMbrListWiz :: RSLT -> IO [Node]
    getMbrListWiz g = do
      putStrLn "Enter relationship member addresses, separated by space."
      -- mns <- parseMaybe (sc *> many integer)
      mns <- map (\a -> R.readMaybe a :: Maybe Node) <$> words <$> getLine
      case or $ map Mb.isNothing mns of 
        True -> do putStrLn "Not a list of addresses!"; getMbrListWiz g
        False -> let ns = Mb.catMaybes mns in case and $ map (flip gelem g) ns of
          True -> return ns
          False -> do putStrLn "Those are not all in the graph!"; getMbrListWiz g

    insRelWiz :: WizState -> IO WizState
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
