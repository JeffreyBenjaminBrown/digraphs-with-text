-- | Sample use:
-- Dwt.prettyPrint $ fr $ parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
-- let Right (_,g) = runStateT (mapM (addExpr . fr . parse expr "" ) ["a # b", "# c", "## d #"]) empty

module Dwt.Parse.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Types
import Dwt.Graph
import Dwt.Search.Local
import Dwt.Util (fr, maxNode, prependCaller, gelemM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.List (mapAccumL)
import qualified Data.Sequence as S
import Control.Lens ((.~))

-- | (At n) represents something already extant in the graph.
-- Leaf and RelX represent something that *might* already exist; it will
-- be searched for. If found, it becomes an At; if not, it is created, and
-- becomes an At.

isAt, isAbsent :: Insertion -> Bool
isAbsent Absent = True
isAbsent _ = False
isAt (At _) = True
isAt _ = False

isValid :: Insertion -> Bool
isValid (RelX _ [_] [Absent,Absent]) = False
isValid (RelX _ [_] [_,_]) = True
isValid (RelX _ js  ms) = (not $ any isAbsent $ middle)
                           && all isValid ms
                           && length js + 1 == length ms
  where middle = tail . reverse . tail $ ms
isValid _ = True

extractTplt :: Insertion -> Expr
extractTplt (RelX _ js as) = Tplt $ ja ++ map (\(JointX s) -> s) js ++ jz
  where (ja,jz) = (f $ head as, f $ last as)
        f Absent = []
        f _ = [""]

prettyPrint :: Insertion -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Level -> Insertion -> IO ()
  it indent (RelX _ js (m:ms)) = do
    putStrLn $ space indent ++ "Insertion: "
    it (indent+1) m
    let f (j,m) = do putStrLn $ (space $ indent+1) ++ show j
                     it (indent+1) m
    mapM_ f $ zip js ms
  it indent l = putStrLn $ space indent ++ show l

addExpr :: Insertion -> StateT RSLT (Either DwtErr) Node
addExpr (At n) = get >>= lift . flip gelemM n >> return n
addExpr Absent = lift $ Left (Impossible
  , [ErrInsertion Absent], "execInsertion.")
addExpr (LeafX e) = qPutSt $ QLeaf e
addExpr q@(RelX _ js as) = do
  ms <- mapM addExpr $ filter (not . isAbsent) as
  t <- qPutSt $ QLeaf $ extractTplt q
  qPutSt $ QRel (QAt t) (map QAt ms)
