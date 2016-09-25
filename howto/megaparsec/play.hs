    module Play where
    
-- lifted from the tutorial
    import Control.Monad (void)
    import Text.Megaparsec
    import Text.Megaparsec.Expr
    import Text.Megaparsec.String -- input stream is of type ‘String’
    import qualified Text.Megaparsec.Lexer as L
    
    sc :: Parser () -- space consumer
    sc = L.space (void spaceChar) lineCmnt blockCmnt
      where lineCmnt  = L.skipLineComment "//"
            blockCmnt = L.skipBlockComment "/*" "*/"
    
    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc
    
    symbol :: String -> Parser String
    symbol = L.symbol sc
    
    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")
    
    integer :: Parser Integer
    integer = lexeme L.integer

-- >>>
    data Cmd = A | B deriving Show

    parseA = lexeme (string "A") *> pure A
    parseB = lexeme (string "B") *> pure B
    parseCmd = sc *> parseA <|> parseB

    parseInts :: Parser [Int]
    parseInts = sc *> (many $ fromIntegral <$> integer)

    ioA :: IO Cmd
    ioA = do mba <- parseMaybe parseA <$> getLine
             maybe (retry "Parse failure." ioA) return mba

    retry :: String -> IO a -> IO a
    retry error f = do putStrLn $ error ++ " Try again?"; f
