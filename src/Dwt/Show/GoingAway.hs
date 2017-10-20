{-# LANGUAGE FlexibleContexts #-}

module Dwt.Show.GoingAway where

import Dwt.Initial.Types
import Dwt.Initial.Measure (tpltArity)
import Dwt.Second.MkTplt (subInTplt, subInTpltWithHashes)
import Dwt.Show.Expr
import Dwt.Second.Graph (users,mbrs,exprDepth)

import Data.Graph.Inductive
import Data.List (sortOn, intercalate)
import qualified Data.Map as Map

-- | == things using _showExpr
prefixer = Prefixer {_str=f, _tplt=f, _coll=f, _rel = const f}
  where f = const ""

prefixerVerbose = Prefixer { _str = colWordFunc
                    , _tplt = \n -> ":" ++ show n ++ " "
                    , _rel = \n tn -> show n ++ ":" ++ show tn ++ " "
                    , _coll = colWordFunc } where
  colWordFunc = \n -> show n ++ ": "

showExpr :: RSLT -> Node -> String
showExpr g n = _showExpr vp g d (Just n)
  where d = exprDepth g n
        vp = ViewProg { vpPrefixer = prefixer
                      , vpShowFiats = Map.empty }

showExprVerbose :: RSLT -> Node -> String
showExprVerbose g n = _showExpr vp g d (Just n)
  where d = exprDepth g n
        vp = ViewProg { vpPrefixer = prefixerVerbose
                      , vpShowFiats = Map.empty }

view :: RSLT -> [Node] -> Either DwtErr [String]
view g ns = do mapM f ns
  where f n = do x <- length <$> users g n
                 return $ show (n, x, showExpr g n)

viewVerbose :: RSLT -> [Node] -> Either DwtErr [String]
viewVerbose g ns = do mapM f ns -- ^ verbose: shows inner addresses
  where f n = do x <- length <$> users g n
                 return $ show (n, x, showExprVerbose g n)

-- | == IO
_v :: (RSLT -> [Node] -> Either DwtErr [String]) -> RSLT -> [Node] -> IO ()
_v viewer g ns = do putStrLn "(node, users, content)"
                    either (putStrLn . show) (mapM_ putStrLn) $ viewer g ns

v, vv :: RSLT -> [Node] -> IO ()
v  = _v view
vv = _v viewVerbose
va, vva :: RSLT -> IO ()
va g = v g $ nodes g
vva g = vv g $ nodes g

-- | == mostly unused
bracket :: String -> String -- unused, but a useful pair of characters
bracket s = "\171" ++ s ++ "\187" -- = «s»
