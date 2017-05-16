    module Experim where

    import Control.Monad (void)
    import Text.Megaparsec
    import Text.Megaparsec.Expr
    import Text.Megaparsec.String -- input stream is of type ‘String’
    import qualified Text.Megaparsec.Lexer as L

    import Data.Maybe

-- Goal: transform from a Hash statement (see the-hash-language.md) to an Expr
    data Expr = ExprAtom String
              | Expr {
                  rels :: [Expr] -- rename to relWords
                  , members :: [Maybe Expr] }

    validExpr :: Expr -> Bool
      -- "bob #needs #someday cabbage" is invalid: either a member should be specified between needs and someday, or they should be part of the same rel-word (like "bob #needs-someday cabbage")
    validExpr (ExprAtom s) = s /= "" -- to write nothing is to express nothing
    validExpr (Expr rels mbrs) =
      length rels + 1 == length mbrs
      && (and $ map (not . isNothing) middleMbrs)
      where middleMbrs = tail $ reverse $ tail mbrs

  -- somehow this is an intermediate type
    type LevelList a = [(Level,a)]
    type Level = Int

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

    -- ? If parseOneLevel does it once, how do you recurse across the LevelList?
      -- That is, what is the _ in parseLevels :: Eq a => LevelList a -> _

    -- Am I missing a case: What if they are atoms? That is, top level = only level, and there's only one (Level,a) in the list?
