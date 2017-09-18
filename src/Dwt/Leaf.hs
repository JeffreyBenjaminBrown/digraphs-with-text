{-# LANGUAGE FlexibleContexts #-}

module Dwt.Leaf (
  mbrListMatchesTpltArity
  , extractTplt
  , tpltArity

  -- Expr tests
  , isWord, isWordM
  , isTplt, isTpltM
  , isFl, isFlM
  , isRel, isRelM
  , isColl, isCollM
  , isLeaf, areLikeExprs
  ) where

import Dwt.Types
import Data.Graph.Inductive (Node, Graph, lab, newNodes, insNode)
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Lens
import Data.Maybe (fromJust, isJust)

extractTplt :: QNode -> Either DwtErr Expr
extractTplt (QRel js as) = Right $ Tplt $ ja ++ map (\(Joint s) -> s) js ++ jz
  where (ja,jz) = (f $ head as, f $ last as)
        f Absent = []
        f _ = [""]
extractTplt q = Left (ConstructorMistmatch, [ErrQNode q], "extractTplt.")

tpltArity :: Expr -> Arity
tpltArity e = case e of Tplt ss -> length ss - 1
                        _       -> error "tpltArity: Expr not a Tplt."

mbrListMatchesTpltArity :: (MonadError DwtErr m) => [Node] -> Expr -> m ()
mbrListMatchesTpltArity ns e = case e of
  Tplt _ -> if (tpltArity e) == length ns
    then return ()
    else throwError (ArityMismatch, [ErrExpr e], funcName)
  _ -> throwError (NotTplt,         [ErrExpr e], funcName)
  where funcName = "mbrListMatchesTpltArity."


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

_isExprMConstructor -- constructs an is_M function (_ is a variable)
  :: (Graph gr) => (a -> Bool) -> gr a b -> Node -> Either DwtErr ()
  -- todo ? catch these erorrs, append strings
    -- otherwise the distinction bewteen absence and inequality is lost
_isExprMConstructor pred g n = case lab g n of 
  Just expr -> case pred expr of True -> return ()
                                 False -> Left $ _1 .~ FoundWrongKind $ err
  Nothing -> Left err
  where err = (FoundNo, [], "_isExprMConstructor.")

isWord :: Expr -> Bool
isWord x = case x of Word _ -> True; _ -> False

isWordM :: RSLT -> Node -> Either DwtErr ()
isWordM = _isExprMConstructor isWord

isTplt :: Expr -> Bool
isTplt x = case x of Tplt _ -> True; _ -> False

isTpltM :: RSLT -> Node -> Either DwtErr ()
isTpltM = _isExprMConstructor isTplt

isFl :: Expr -> Bool
isFl x = case x of Fl _ -> True; _ -> False

isFlM :: RSLT -> Node -> Either DwtErr ()
isFlM = _isExprMConstructor isFl

isRel :: Expr -> Bool
isRel x = case x of Rel -> True; _ -> False

isRelM :: RSLT -> Node -> Either DwtErr ()
isRelM = _isExprMConstructor isRel

isColl :: Expr -> Bool
isColl x = case x of Coll -> True; _ -> False

isCollM :: RSLT -> Node -> Either DwtErr ()
isCollM = _isExprMConstructor isColl

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
