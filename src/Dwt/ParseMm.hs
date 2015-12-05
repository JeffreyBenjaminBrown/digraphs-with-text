-- usually folded
  -- uses some functions by Jake Wheat
    -- https://github.com/JakeWheat/intro_to_parsing
    -- parse2 below is what Jake Wheat called parseWithLeftOver
  -- tags to skip -- NEAT: Maybe I did not need to write these
    -- <map_styles>
    -- <stylenode LOCALIZED_TEXT="styles.root_node">
    -- <font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
    -- </stylenode>
    -- <edge STYLE="hide_edge"/>
    -- <cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
    -- <icon BUILTIN="yes"/>
    -- </map_styles>
    -- </hook>
    -- </map>

-- init
    {-# LANGUAGE FlexibleContexts #-}
    module Dwt.ParseMm
      ( module Text.Parsec
      , module Text.Parsec.String
      , module Dwt.ParseMm
      ) where
    import Text.Parsec
    import Text.Parsec.String (Parser)
    import qualified Data.Map as Map

-- types
    data Branch = Branch { mmText :: MmText
                         , scrs :: [Branch]
                         , links :: [MmText] }

    data MmText = MmText { text :: String
                         , mmId :: Int
                         , created :: Int
                         , modified :: Int }

    data MmTag = MmTag { title :: String 
                       , starts :: Bool, ends :: Bool
                       , mmMap :: Map.Map String String
                       } | Comment deriving (Eq, Show)

-- parse
    parseWithEof :: Parser a -> String -> Either ParseError a
    parseWithEof p = parse (p <* eof) ""

    eParse :: Parser a -> String -> Either ParseError a
    eParse p = parse p ""

    eParse2 :: Parser a -> String -> Either ParseError (a,String)
    eParse2 p = parse ((,) <$> p <*> leftOver) ""
      where leftOver = manyTill anyToken eof

-- parsers
    lexeme :: Parser a -> Parser a
    lexeme p = p <* spaces

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

    mmStr = between quot quot 
      $ many $ mmEscapedChar <|> satisfy (/= '"')
      where quot = char '"'

    word :: Parser String -- that is, a Word outside of an MmNodeText
    word = many1 $ alphaNum <|> char '_'

    keyValPair :: Parser (String,String)
    keyValPair = (,) <$> (lexeme word <* lexeme (char '=')) <*> lexeme mmStr

    mmTag :: Parser MmTag -- IS tested but strangely
    mmTag = do starts <- startsItself
               title <- lexeme word
               pairs <- many $ lexeme keyValPair
               ends <- endsItself -- not use lexeme here, rather a level up
               return $ MmTag { title = title
                              , starts = starts
                              , ends = ends
                              , mmMap = Map.fromList pairs
                              }
      where endsItself =     (string "/>" >> return True) 
                         <|> (string ">" >> return False) :: Parser Bool
            startsItself  =  (try $ string "</" >> return False)
                         <|> (string "<" >> return True) :: Parser Bool
      -- TO LEARN
        -- does endsItself need no try while startsItself does
        -- because startsItself can read halfway through before discarding,
        -- while endsItself recognizes inequality at the first character?

    comment :: Parser MmTag -- found in Text.ParserCombinators.Parsec.Combinator
    comment  = do string "<!--"
                  manyTill anyChar (try $ string "-->")
                  return Comment

    strip :: Parser a -> Parser [Char]
    strip p = many $ (skipMany $ try p) >> anyChar

    mmFile :: Parser [MmTag]
    mmFile = optional space *> (sepBy (lexeme $ try mmTag <|> comment)
                                      space)

-- [mmTag] -> _
