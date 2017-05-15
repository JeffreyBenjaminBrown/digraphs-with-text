-- now using a fuller tutorial: https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
  -- which built on this one: https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

    module Experim where

    import Control.Monad (void)
    import Text.Megaparsec
    import Text.Megaparsec.Expr
    import Text.Megaparsec.String -- input stream is of type ‘String’
    import qualified Text.Megaparsec.Lexer as L

    import Data.Maybe

  -- tiny parsers
    sc :: Parser () -- space consumer
    sc = L.space (void spaceChar) lineCmnt blockCmnt
      where lineCmnt  = L.skipLineComment "//"
            blockCmnt = L.skipBlockComment "/*" "*/"

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    symbol :: String -> Parser String
    symbol = L.symbol sc

    integer :: Parser Integer
    integer = lexeme L.integer

    integerList :: Parser [Int]
    integerList = sc *> (many $ fromIntegral <$> integer)

    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    identifier :: Parser String
    identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

  -- >>>
    -- easier to start: no parens, no punctuation except $
    -- no abut except for some $ symbols before a word

    data Expr = ExprAtom String
              | Expr { rels :: [Expr]
                     , members :: [Maybe Expr] }

    validExpr :: Expr -> Bool
    validExpr (ExprAtom s) = s /= "" -- to write nothing is to express nothing
    validExpr (Expr rels mbrs) =
      length rels + 1 == length mbrs
      && (and $ map (not . isNothing) middleMbrs)
      where middleMbrs = tail $ reverse $ tail mbrs

    type Level = Int
    type LevelList a = [(Level,a)]

    maxLevel :: LevelList a -> Int
    maxLevel = maximum . map fst

    parseOneLevel :: Eq a => LevelList a -> [(Maybe a, LevelList a)]
        -- the maybe is the rel, the list is the member preceding it
        -- an empty LevelList is only valid in the first position
           -- because there might not have been anything before the first rel word
        -- a Nothing is only valid in the last position
           -- because there might be something after the last rel word
    parseOneLevel l = parseOneLevel' (maxLevel) l where
      parseOneLevel' :: Eq a => Level -> LevelList a -> [(Maybe a, LevelList a)]
      parseOneLevel' _ [] = []
      parseOneLevel' topLevel ((l1,a1):rest) =
        if l1 == topLevel then (Just a1,[]) : parseOneLevel topLevel rest
        else if rest == [] then [(Nothing,[(l1,a1)])]
        else (rel,(l1,a1):mbrs):more
          where (rel,mbrs):more = parseOneLevel topLevel rest
      -- It works!
        -- *Experim> parseOneLevel 3 [(3,x),(2,y),(1,z),(3,x),(1,z)]
        -- [(Just "x",[]),(Just "x",[(2,"y"),(1,"z")]),(Nothing,[(1,"z")])]
        -- *Experim> 

  -- too soon ! tried to start with parentheticals that abut
    -- type Level = Int
    -- data Tokenn = Tokenn String | Punct String -- "Token" already taken
    -- data LevelToken = LevelToken Level Tokenn
    -- newtype FlatExpr = FlatExpr [LevelToken]
    -- data Expr = ExprToken Tokenn
    --           | Expr { rels :: [Expr]
    --                  , members :: [Maybe Expr] }
    -- 
    -- validExpr :: Expr -> Bool
    -- validExpr (Expr rels mbrs) =
    --   length rels + 1 == length mbrs
    --   && (and $ map (not . isNothing) middleMbrs)
    --   where middleMbrs = tail $ reverse $ tail mbrs
