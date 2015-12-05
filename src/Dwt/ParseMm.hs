-- usually folded
  -- uses some functions by Jake Wheat
    -- https://github.com/JakeWheat/intro_to_parsing
    -- parse2 below is what Jake Wheat called parseWithLeftOver

-- init
    {-# LANGUAGE FlexibleContexts #-}
    module Dwt.ParseMm
      ( module Text.Parsec
      , module Text.Parsec.String
      , module Dwt.ParseMm
      ) where
    import Text.Parsec
    import Text.Parsec.String (Parser)
    import Data.Map

-- simplified parse commands
    parseWithEof :: Parser a -> String -> Either ParseError a
    parseWithEof p = parse (p <* eof) ""

    eParse :: Parser a -> String -> Either ParseError a
    eParse p = parse p ""

    eParse2 :: Parser a -> String -> Either ParseError (a,String)
    eParse2 p = parse ((,) <$> p <*> leftOver) ""
      where leftOver = manyTill anyToken eof

-- parsers
    mmEscapedChar :: Parser Char
    mmEscapedChar = mmLeftAngle <|> mmNewline <|> mmRightAngle 
        <|> mmCaret <|> mmAmpersand <|> mmApostrophe
      where sandwich s = try $ string $ "&" ++ s ++ ";"
            mmLeftAngle = pure '<' <* sandwich "lt"
            mmNewline = pure '\n' <* sandwich "#xa"
            mmRightAngle = pure '>' <* sandwich "gt"
            mmCaret = pure '"' <* sandwich "quot"
            mmAmpersand = pure '&' <* sandwich "amp"
            mmApostrophe = pure '\'' <* sandwich "apos"

    mmNodeText = char '"' *>  -- TODO: readability: use between
     (many $ mmEscapedChar <|> satisfy (/= '"'))
     <* char '"'

    word :: Parser String -- that is, a Word outside of an MmNodeText
    word = many1 $ alphaNum <|> char '_'

    -- see csv2.hs at http://book.realworldhaskell.org/read/using-parsec.html
      -- for how to make a (Parser [a]) from a (Parser a)
    -- or this is even easier:
      -- *Dwt> eParse2 (many $ string "bo") "bobobodo"
      -- Right (["bo","bo","bo"],"do")
    -- tag :: String -> Parser (Map String String)
    -- tag name = 

    -- found this in Text.ParserCombinators.Parsec.Combinator
    comment :: Parser String
    comment  = do string "<!--"
                  manyTill anyChar (try $ string "-->")

    
