{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Dwt.Hash.Parse (
  expr

  -- for tests, not interface
  , leaf, at, qVar, leafOrAtOrVar
  , pBranch, pMapBranch, pRoleMap
  , EO(..), Hash(..)
  , hash, rightConcat, leftConcat
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char (satisfy, string, char)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))

import Dwt.Initial.ParseUtils (Parser, lexeme, parens, brackets, sc
                      , integer
                      , anyWord, word, wordChar, phrase)
import Dwt.Initial.Types
import Dwt.Second.MkTplt (mkTplt)
import Dwt.Second.Misc (toRoleMap)
import Data.List (intersperse)
import qualified Data.Map as M


-- == Hash (not parsing)
data EO = EO     -- ^ EO = "expression orderer"
  { open :: Bool -- ^ open = "more expressions can be concatentated into it"
                 -- In b@(QRel (EO x _) _ _), x is true until
                 -- b has been surrounded by parentheses.
  , inLevel :: Level } deriving (Eq)
instance Show EO where
  show (EO x y) = "(EO " ++ show x ++ " " ++ show y ++ ")"
instance Ord EO where -- ^ Open > closed. If those are equal, ## > #, etc.
  EO a b <= EO c d = a <= c && b <= d

data Hash = Hash EO QNode -- ^ QRels go here
  | HashNonRel QNode -- ^ anything else goes here
  deriving (Show, Eq)

getQNode :: Hash -> QNode
getQNode (HashNonRel q) = q
getQNode (Hash _ q) = q

hasInsRel :: Hash -> Bool
hasInsRel (Hash _ _) = True
hasInsRel (HashNonRel _) = False

startRel :: Level -> Joint -> Hash -> Hash -> Hash
startRel l j a b = Hash (EO True l) $ QRel [j] $ map getQNode [a,b]

-- | PITFALL: In "a # b # c # d", you might imagine evaluating the middle #
-- after the others. In that case both sides would be a QRel, and you would
-- want to modify both, rather than make one a member of the other. These
-- concat functions skip that possibility; one of the two QNode arguments is
-- always incorporated into the other. I believe that is safe, because 
-- expressions in serial on the same level will always be parsed left to
-- right, not outside to inside.
rightConcat :: Joint
  -> Hash -- ^ insert this
  -> Hash -- ^ into the right side of this
  -> Hash -- ifdo speed : use a two-sided list of pairs
rightConcat j h (Hash eo (QRel joints mbrs))
  = Hash eo $ QRel (joints ++ [j]) (mbrs ++ [getQNode h])
rightConcat _ _ _ = error "can only rightConcat into a QRel"

leftConcat :: Joint
  -> Hash -- ^ insert this
  -> Hash -- ^ into the right side of this
  -> Hash -- ifdo speed : use a two-sided list of pairs
leftConcat j h (Hash eo (QRel joints mbrs))
  = Hash eo $ QRel (j : joints) (getQNode h : mbrs)
leftConcat _ _ _ = error "can only leftConcat into a QRel"

close :: Hash -> Hash
close (Hash (EO _ a) (QRel b c)) = Hash (EO False a) $ QRel b c
close x@(HashNonRel _) = x

hash :: Level -> Joint -> Hash -> Hash -> Hash
hash l j a@(hasInsRel -> False) b@(hasInsRel -> False)
  = startRel l j a b
hash l j a@(hasInsRel -> False) b@(Hash (EO False _) (QRel _ _))
  = startRel l j a b
hash l j a@(Hash (EO False _) (QRel _ _)) b@(hasInsRel -> False)
  = startRel l j a b
hash l j a@(hasInsRel -> False) b@(Hash (EO True l') (QRel _ _))
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = leftConcat j a b -- I suspect this won't happen either
  | l > l' = startRel l j a b
hash l j a@(Hash (EO True l') (QRel _ _)) b@(hasInsRel -> False)
  | l < l' = error "Higher level should not have been evaluated first."
  | l == l' = rightConcat j b a -- but this will
  | l > l' = startRel l j a b
hash l j a@(Hash ea (QRel _ _)) b@(Hash eb (QRel _ _)) =
  let e = EO True l
      msg = unlines [ "Joint should have been evaluated earlier."
                    , "level: " ++ show l
                    , "joint: " ++ show j
                    , "left: " ++ show a
                    , "right: " ++ show b ]
  in if e <= eb then error msg
     else if e > ea then startRel l j a b
     else if e == ea then rightConcat j b a
     else error msg


-- == parsing QNodes
expr :: Parser QNode
expr = lexeme $ sc >> getQNode <$> _expr

_expr :: Parser Hash
_expr = makeExprParser term
  [ [ InfixL $ try $ pHash n
    , InfixL $ try $ pAnd n
    , InfixL $ try $ pOr n
    ] | n <- [1..8] ]

pQNode = (QLeaf <$> try leaf <|> try at <|> try qVar)
         <|> try pBranch
         <|> try pMapBranch

term :: Parser Hash
term = HashNonRel <$> try pQNode
       <|> close <$> parens _expr
       <|> absent where
  absent :: Parser Hash
  absent = HashNonRel . const Absent <$> f <?> "Intended to \"find\" nothing."
  f = lookAhead $ const () <$> satisfy (== '#') <|> eof
    -- the Absent parser should look for #, but not ), because
    -- parentheses get consumed in pairs in an outer (earlier) context

-- == parsing operators
pHash :: Int -> Parser (Hash -> Hash -> Hash)
pHash n = lexeme $ do pHashUnlabeled n
                      label <- option "" $ anyWord <|> parens phrase
                      return $ hash n $ Joint label
  where pHashUnlabeled :: Int -> Parser ()
        pHashUnlabeled n = const () <$> f
        f = string (replicate n '#') <* notFollowedBy (char '#')

pAnd :: Int -> Parser (Hash -> Hash -> Hash)
pAnd n = lexeme $ do pAndUnlabeled n
                     return $ \a b-> HashNonRel $ QAnd $ map getQNode [a,b]
  -- todo ? prettier would be to test if they are already QAnds,
  -- and if so merge them, rather than creating a new one.
  -- The same could be done to pOr.
  where pAndUnlabeled :: Int -> Parser ()
        pAndUnlabeled n = const () <$> f
        f = string (replicate n '&') <* notFollowedBy (char '&')

pOr :: Int -> Parser (Hash -> Hash -> Hash)
pOr n = lexeme $ do pOrUnlabeled n
                    return $ \a b-> HashNonRel $ QOr $ map getQNode [a,b]
  where pOrUnlabeled :: Int -> Parser ()
        pOrUnlabeled n = const () <$> f
        f = string (replicate n '|') <* notFollowedBy (char '|')

-- == QBranch
-- | less typing, but less flexible
pBranch :: Parser QNode -- TODO ? handle a Left in a Parser
pBranch = lexeme $ do
  word "/b"
  eitherDir <- toRoleMap <$> parens expr
  origin <- expr
  case eitherDir of Left e -> error $ "pBranch parser encountered a Left: "
                                      ++ show e
                    Right dir -> return $ QBranch dir origin

-- | a little harder to type,  but more flexible
pMapBranch :: Parser QNode
pMapBranch = lexeme $ do word "/mb"
                         dir <- parens pRoleMap
                         origin <- expr
                         return $ QBranch dir origin

pRoleMap :: Parser RoleMap
pRoleMap = M.fromList <$> pair `sepBy` (lexeme $ char ',') where
  pair :: Parser (RelRole, QNode)
  pair = (,) <$> pRelRole <*> expr
  pRelRole :: Parser RelRole
  pRelRole = lexeme $ const TpltRole <$> word "t"
                      <|> Mbr <$> integer

-- == the simplest kinds of leaf
leafOrAtOrVar :: Parser QNode
leafOrAtOrVar = QLeaf <$> try leaf <|> try at <|> try qVar

leaf :: Parser Expr
leaf = do
  let blank = lexeme $ string "_" <* notFollowedBy (wordChar <|> char '_')
      f = concat . intersperse " " 
  p <- some $ blank <|> anyWord
  return $ case elem "_" p of True ->  mkTplt . f $ p
                              False -> Word   . f $ p

at :: Parser QNode
at = At <$> (char '/' >> integer)

qVar :: Parser QNode
qVar = QVar <$> (string "/" >> it <|> any <|> from <|> to) where
  it, any, from, to :: Parser SearchVar
  it = const It <$> word "it"
  any = const Any <$> word "any"
  from = const From <$> word "from"
  to = const To <$> word "to"

-- | unused
hasBlanks :: Parser Bool
hasBlanks = (>0) . length . concat <$> (sc *> (many $ blank <|> other))
  where blank, other :: Parser String  -- order across the <|> matters
        blank = try $ word "_"
        other = const "" <$> anyWord
