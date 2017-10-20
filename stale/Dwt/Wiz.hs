    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE ViewPatterns #-}

    module Dwt.Wiz where
    import Data.Graph.Inductive
    import Dwt.Edit
    import Dwt.Show.Expr
    import qualified Data.Maybe as Mb

    import Control.Monad (void)
    import Text.Megaparsec
    import Text.Megaparsec.String
    import qualified Text.Megaparsec.Lexer as L

-- data
    data Cmd = Help | View | InsWord | InsTplt | QRel | Quit deriving (Show, Read)
    type WizState = RSLT

-- parser utilities
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

-- read a command name
    commandAliases :: [(String, Cmd)]
    commandAliases = [ ("?", Help)
                     , ("v", View)
                     , ("iw", InsWord)
                     , ("it", InsTplt)
                     , ("ir", QRel)
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
      putStrLn "What next? (Type ? for help.)"
      mbCmd <- parseMaybe parseCmd <$> getLine
      maybe (retry "Command not understood." $ wiz g) f mbCmd where
        f cmd = case cmd of 
          Help -> do putStrLn "Type v to view, iw to insert word, it to insert template, ir to insert relationship."; wiz g
          View -> do view g $ nodes g; wiz g
          InsWord -> do putStrLn "Enter a Word (any string, really) to add."
                        s <- getLine
                        wiz $ insWord s g
          InsTplt -> tryWizStateIO "Enter a Tplt to add."
                           ((>0) . length . filter (=='_'))
                           insTplt g
          QRel -> do h <- insRelWiz g; wiz h
          Quit -> return g

    tryWizStateIO :: -- only used once
          String -- tell the user
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
      mns <- parseMaybe (sc *> many integer) <$> getLine
      case mns of 
        Nothing -> do putStrLn "Error parsing address list."; getMbrListWiz g
        Just ns -> let ints = fromIntegral <$> ns 
          in case and $ map (flip gelem g) $ ints of
            True -> return ints
            False -> do putStrLn "Addresses not all in graph."; getMbrListWiz g

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
