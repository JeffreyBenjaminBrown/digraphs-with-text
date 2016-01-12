    module Dwt.Parse
      ( module Dwt.Parse
      ) where

    import Text.Parsec
    import Text.Parsec.String (Parser)

-- Parser a -> String -> _
    parseWithEof :: Parser a -> String -> Either ParseError a
    parseWithEof p = parse (p <* eof) ""

    eParse :: Parser a -> String -> Either ParseError a
    eParse p = parse p ""

    eParse2 :: Parser a -> String -> Either ParseError (a,String)
    eParse2 p = parse ((,) <$> p <*> leftOver) ""
      where leftOver = manyTill anyToken eof
