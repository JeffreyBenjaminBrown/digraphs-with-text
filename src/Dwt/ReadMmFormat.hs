    module Dwt.ReadMmFormat
      ( module Text.Parsec
      , module Text.Parsec.String
      , module Dwt.ReadMmFormat
      ) where
    import Text.Parsec
    import Text.Parsec.String (Parser)

    parseWithEof :: Parser a -> String -> Either ParseError a
    parseWithEof p = parse (p <* eof) ""

    parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
    parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
      where leftOver = manyTill anyToken eof
