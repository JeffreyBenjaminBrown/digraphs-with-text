{-# LANGUAGE FlexibleContexts #-}

module Dwt.Leaf (
  _splitStringForTplt, mkTplt
  , subInTplt, padTpltStrings, subInTpltWithHashes
  , tpltArity, mbrListMatchesTpltArityDeprecatoryName, mbrListMatchesTpltAritySum
  , insLeaf
    , insWord, insTplt, insFl -- TODO ? deprec, insLeaf generalizes them
  
  , isWord, isWordMDeprecatoryName, isWordMSum
  , isTplt, isTpltMDeprecatoryName, isTpltMSum
  , isFl, isFlMDeprecatoryName, isFlMSum
  , isRel, isRelMDeprecatoryName, isRelMSum
  , isColl, isCollMDeprecatoryName, isCollMSum
  , isLeaf, areLikeExprs
  ) where

import Dwt.Types
import Data.Graph.Inductive (Node, Graph, lab, newNodes, insNode)
import Control.Monad.Except (MonadError, throwError, catchError)
import Data.Text (pack, unpack, strip, splitOn)
import Control.Lens

-- == Tplt
_splitStringForTplt :: String -> [String]
_splitStringForTplt t = map unpack $ splitOn (pack "_") (pack t)

mkTplt :: String -> Expr
mkTplt = Tplt
  . map (unpack . strip . pack)
  . _splitStringForTplt

subInTpltWithHashes :: Expr      -- must be a Tplt
                     -> [String] -- members for the Tplt
                     -> Int      -- relationship level = number of #s
                     -> String
  -- todo ? test length (should match arity), use Either
  -- todo ? test each tplt-string; if has space, wrap in parens
subInTpltWithHashes (Tplt ts) ss prefixCount =
  let ts' = padTpltStrings (Tplt ts)
          $ replicate (2^prefixCount) '#'
      pairList = zip ts' $ ss ++ [""]
       -- append "" because there are n+1 segments in an n-ary Tplt; 
         -- zipper ends early otherwise
  in foldl (\s (a,b) -> s++a++b) "" pairList
subInTpltWithHashes _ _ _ = error "subInTplt: not a Tplt" -- todo ? omit

subInTplt :: Expr -> [String] -> String
subInTplt (Tplt ts) ss = subInTpltWithHashes (Tplt ts) ss 0

padTpltStrings :: Expr -> String -> [String]
padTpltStrings (Tplt ss) prefix =
  let a = head ss
      z = last ss
      middle = reverse $ tail $ reverse $ tail ss
      f s = if elem ' ' s then '(' : (s ++ ")") else s
      doToMiddle s = " " ++ prefix ++ f s ++ " "
      doToFirst s = case s of "" -> ""
                              _ -> prefix ++ f s ++ " "
      doToLast  s = case s of "" -> ""
                              _ -> " " ++ prefix ++ f s
  in [doToFirst a] ++ map doToMiddle middle ++ [doToLast z]

tpltArity :: Expr -> Arity
tpltArity e = case e of Tplt ss -> length ss - 1
                        _       -> error "tpltArity: Expr not a Tplt."

mbrListMatchesTpltArityDeprecatoryName :: (MonadError DwtErrDeprecatoryName m) => [Node] -> Expr -> m ()
mbrListMatchesTpltArityDeprecatoryName ns e = case e of
  Tplt _ -> if (tpltArity e) == length ns
    then return ()
    else throwError (ArityMismatch, mExpr .~ Just e $ noErrOpts, funcName)
  _ -> throwError (NotTplt,         mExpr .~ Just e $ noErrOpts, funcName)
  where funcName = "mbrListMatchesTpltArityDeprecatoryName."

mbrListMatchesTpltAritySum :: (MonadError DwtErrSum m) => [Node] -> Expr -> m ()
mbrListMatchesTpltAritySum ns e = case e of
  Tplt _ -> if (tpltArity e) == length ns
    then return ()
    else throwError (ArityMismatch, [ErrExpr e], funcName)
  _ -> throwError (NotTplt,         [ErrExpr e], funcName)
  where funcName = "mbrListMatchesTpltArityDeprecatoryName."

-- == Insert
insLeaf :: Expr -> RSLT -> RSLT
  -- TODO : use this to avoid duplicate ways to delete, replace, ...
insLeaf e g = case isLeaf e of
  True -> insNode (newAddr, e) g where [newAddr] = newNodes 1 g
  False -> error $ "insLeaf: " ++ show e ++ "is not a leaf."

insWord :: String -> RSLT -> RSLT
insWord str = insLeaf (Word str)

insTplt :: String -> RSLT -> RSLT
insTplt s = insLeaf $ mkTplt s

insFl :: Float -> RSLT -> RSLT
insFl f = insLeaf $ Fl f

-- == Expr tests
_isExprMConstructorStrErr :: (MonadError String m, Graph gr) => (a -> Bool) ->
  gr a b -> Node -> m () -- constructs an isExprM function (Expr a variable)
  -- todo ? catch these erorrs, append strings
    -- otherwise the distinction bewteen absence and inequality is lost
_isExprMConstructorStrErr pred g n = case mExpr of 
    Nothing -> throwError $ "Node " ++ show n ++ " absent."
    Just expr ->  case pred expr of True -> return ()
                                    False -> throwError $ "is not"
  where mExpr = lab g n

_isExprMConstructorDeprecatoryName -- constructs an is_M function (_ is a variable)
  :: (Graph gr) => (a -> Bool) -> gr a b -> Node -> Either DwtErrDeprecatoryName ()
  -- todo ? catch these erorrs, append strings
    -- otherwise the distinction bewteen absence and inequality is lost
_isExprMConstructorDeprecatoryName pred g n = case lab g n of 
  Just expr -> case pred expr of True -> return ()
                                 False -> Left $ _1 .~ FoundWrongKind $ err
  Nothing -> Left err
  where err = (FoundNo, mNode .~ Just n $ noErrOpts, "_isExprMConstructorDeprecatoryName.")

_isExprMConstructorSum -- constructs an is_M function (_ is a variable)
  :: (Graph gr) => (a -> Bool) -> gr a b -> Node -> Either DwtErrSum ()
  -- todo ? catch these erorrs, append strings
    -- otherwise the distinction bewteen absence and inequality is lost
_isExprMConstructorSum pred g n = case lab g n of 
  Just expr -> case pred expr of True -> return ()
                                 False -> Left $ _1 .~ FoundWrongKind $ err
  Nothing -> Left err
  where err = (FoundNo, [], "_isExprMConstructorDeprecatoryName.")

isWord :: Expr -> Bool
isWord x = case x of Word _ -> True; _ -> False

isWordMDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName ()
isWordMDeprecatoryName = _isExprMConstructorDeprecatoryName isWord

isWordMSum :: RSLT -> Node -> Either DwtErrSum ()
isWordMSum = _isExprMConstructorSum isWord

isTplt :: Expr -> Bool
isTplt x = case x of Tplt _ -> True; _ -> False

isTpltMDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName ()
isTpltMDeprecatoryName = _isExprMConstructorDeprecatoryName isTplt

isTpltMSum :: RSLT -> Node -> Either DwtErrSum ()
isTpltMSum = _isExprMConstructorSum isTplt

isFl :: Expr -> Bool
isFl x = case x of Fl _ -> True; _ -> False

isFlMDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName ()
isFlMDeprecatoryName = _isExprMConstructorDeprecatoryName isFl

isFlMSum :: RSLT -> Node -> Either DwtErrSum ()
isFlMSum = _isExprMConstructorSum isFl

isRel :: Expr -> Bool
isRel x = case x of Rel -> True; _ -> False

isRelMDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName ()
isRelMDeprecatoryName = _isExprMConstructorDeprecatoryName isRel

isRelMSum :: RSLT -> Node -> Either DwtErrSum ()
isRelMSum = _isExprMConstructorSum isRel

isColl :: Expr -> Bool
isColl x = case x of Coll -> True; _ -> False

isCollMDeprecatoryName :: RSLT -> Node -> Either DwtErrDeprecatoryName ()
isCollMDeprecatoryName = _isExprMConstructorDeprecatoryName isColl

isCollMSum :: RSLT -> Node -> Either DwtErrSum ()
isCollMSum = _isExprMConstructorSum isColl

isLeaf :: Expr -> Bool -- todo ? make Leaf an Expr constructor
isLeaf (Word _) = True
isLeaf (Fl _) = True
isLeaf (Tplt _) = True
isLeaf _ = False

areLikeExprs :: Expr -> Expr -> Bool
areLikeExprs e f = case e of
  Word _  ->  case f of Word  _ -> True;  _ -> False
  Tplt _ ->  case f of Tplt _ -> True;  _ -> False
  Rel    ->  case f of Rel    -> True;  _ -> False
  Coll   ->  case f of Coll   -> True;  _ -> False
  RelSpecExpr _ ->  case f of RelSpecExpr _ -> True;  _ -> False
