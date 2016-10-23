-- based on (and simpler than) the tutorial at https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

    module Experim where
    
    import Control.Monad (void)
    import Text.Megaparsec
    import Text.Megaparsec.Expr
    import Text.Megaparsec.String -- input stream is of type ‘String’
    import qualified Text.Megaparsec.Lexer as L

    test = map (parseMaybe aExpr) exprsToParse
    exprsToParse = [ "a $ b"             -- works
                   , "a $$ b"            -- fails!
                   , "a $ b $$ c $ d"    -- works
                   , "(a $ b) $ (c $ d)" -- works
                   ]

  -- tiny parsers
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

    parseInts :: Parser [Int]
    parseInts = sc *> (many $ fromIntegral <$> integer)

  -- operators!
    data AExpr = Var String | Pair AExpr AExpr deriving (Show)
   
    aExpr :: Parser AExpr
    aExpr = makeExprParser aTerm aOperators
    
    aTerm :: Parser AExpr
    aTerm = parens aExpr   <|>   Var <$> identifier

    aOperators :: [[Operator Parser AExpr]]
    aOperators =
      [ [ InfixN $ symbol "$" *> pure (Pair) ]
      , [ InfixN $ symbol "$$" *> pure (Pair) ]
      ]

    identifier :: Parser String
    identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar
