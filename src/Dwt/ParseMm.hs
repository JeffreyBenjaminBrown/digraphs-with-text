-- usually folded
  -- CREDITS: uses some functions by Jake Wheat
    -- https://github.com/JakeWheat/intro_to_parsing
    -- parse2 below is what Wheat called parseWithLeftOver
   -- styles|fonts: incomplete
    -- within-node ones, e.g. LOCALIZED_STYLE_REF="styles.topic", this captures
    -- but <font ...> tags outside of a node applicable to it, this does not

-- lang, modules
    {-# LANGUAGE FlexibleContexts #-}
    module Dwt.ParseMm
      ( module Text.Parsec
      , module Text.Parsec.String
      , module Dwt.ParseMm
      ) where
    import Text.Parsec
    import Text.Parsec.String (Parser)
    import Control.Monad.Except
    import qualified Data.Map as Map
    import qualified Data.Time.Clock as Time

-- types
    -- TODO : rep times, which are 1/1000 of a second after the start of 1970
    data MmEdge = MmScr   { from :: Int, to :: Int }
                | MmArrow { from :: Int, to :: Int } deriving (Show, Eq)

    data MmText = MmText { text :: String
                         , mmId :: Int
                         , style :: Maybe String
                         , created :: Int
                         , modified :: Int } deriving (Show, Eq)

    data MlTag = MlTag { title :: String 
                       , isStart :: Bool -- starting < is start; </ is not
                       , isEnd :: Bool   -- ending /> is end; > is not
                       , mlMap :: Map.Map String String
                       } | Comment deriving (Eq, Show)

-- parsing generally
    parseWithEof :: Parser a -> String -> Either ParseError a
    parseWithEof p = parse (p <* eof) ""

    eParse :: Parser a -> String -> Either ParseError a
    eParse p = parse p ""

    eParse2 :: Parser a -> String -> Either ParseError (a,String)
    eParse2 p = parse ((,) <$> p <*> leftOver) ""
      where leftOver = manyTill anyToken eof

-- parsing the .mm format
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

    mlTag :: Parser MlTag -- IS tested but strangely
    mlTag = do isStart <- startsItself
               title <- lexeme word
               pairs <- many $ lexeme keyValPair
               isEnd <- endsItself -- not use lexeme here, rather a level up
               return $ MlTag { title = title
                              , isStart = isStart
                              , isEnd = isEnd
                              , mlMap = Map.fromList pairs
                              }
      where endsItself =     (string "/>" >> return True) 
                         <|> (string ">" >> return False) :: Parser Bool
            startsItself  =  (try $ string "</" >> return False)
                         <|> (string "<" >> return True) :: Parser Bool

    comment :: Parser MlTag -- found in Text.ParserCombinators.Parsec.Combinator
    comment  = do string "<!--"
                  manyTill anyChar (try $ string "-->")
                  return Comment

    strip :: Parser a -> Parser [Char]
    strip p = many $ (skipMany $ try p) >> anyChar

    parseMmFile :: String -> Either ParseError [MlTag]
      -- MYST ? how to unify this two-parse strategy with the usual parser idiom
    parseMmFile f = case eParse (strip comment) f of 
        Right f' ->  eParse (many $ lexeme mlTag) f'
        Left e -> throwError e

-- [mlTag] -> _
  -- TODO ? Work with the Eithers and Nothings rather than fighting them
  -- TODO ? use safe Map lookups
    tagToKeep :: MlTag -> Bool
    tagToKeep t = elem (title t) ["node","arrowlink"]

    parseId :: String -> Either ParseError Int
    parseId s = read <$> eParse (string "ID_" *> many digit) s

    arrowDest :: MlTag -> Either ParseError Int
    arrowDest m = parseId $ mlMap m Map.! "DESTINATION"

    fromRight :: Either a b -> b
    fromRight (Right b) = b
    fromRight (Left _) = error "fromRight: Left"

    mmText :: MlTag -> MmText
    mmText tag = 
      let m = mlMap tag
          text = m Map.! "TEXT"
          mmId = fromRight $ parseId $ m Map.! "ID"
          style = if Map.member "LOCALIZED_STYLE_REF" m
                    then Just $ m Map.! "LOCALIZED_STYLE_REF"
                    else Nothing
          created = read $ m Map.! "CREATED"
          modified = read $ m Map.! "MODIFIED"
      in MmText text mmId style created modified

-- ? LAST
    -- from a list of MmNodes and MmArrows, generate a list of MmNodes and MmEdges
    -- Thread the current ancestry through that calculation
      -- at each descent, add to it; at each rise, pop its most recent
