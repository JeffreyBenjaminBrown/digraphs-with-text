-- based on (and simpler than) the tutorial at https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

    module Experim where

    import Control.Monad (void)
    import Data.Void
    import Text.Megaparsec
    import Text.Megaparsec.Expr
    import qualified Text.Megaparsec.Char as C
    import qualified Text.Megaparsec.Char.Lexer as L

    type Parser = Parsec Void String

  -- tiny parsers
    sc :: Parser () -- space consumer
    sc = L.space C.space1 lineCmnt blockCmnt
      where lineCmnt  = L.skipLineComment "//"
            blockCmnt = L.skipBlockComment "/*" "*/"

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    symbol :: String -> Parser String
    symbol = L.symbol sc

--    integer :: Parser Integer
--    integer = lexeme L.integer
--
--    parseInts :: Parser [Int]
--    parseInts = sc *> (many $ fromIntegral <$> integer)

    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    identifier :: Parser String
    identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

  -- expressions, operators and recursion
    data AExpr = Var String | Pair AExpr AExpr deriving (Show)

    aExpr :: Parser AExpr
    aExpr = makeExprParser aTerm aOperators

    aTerm :: Parser AExpr
    aTerm = parens aExpr   <|>   Var <$> identifier

    aOperators :: [[Operator Parser AExpr]]
    aOperators = -- the bug is here. see experim.hs for explanation
      [ [ InfixN $ op "#" *> pure (Pair) ]
      , [ InfixN $ op "##" *> pure (Pair) ]
      ] where
      op :: String -> Parser String
      op n = (lexeme . try) (C.string n <* notFollowedBy C.punctuationChar)

  -- test it
    test = map (parseMaybe aExpr) exprsToParse
    exprsToParse = [ "a # b"
                   , "a ## b"
                   , "a # b ## c # d"
                   , "(a # b) # (c # d)"
                   ]
