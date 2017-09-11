module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Types
import Dwt.Graph
import Dwt.Search
import Dwt.Util (fr, maxNode, prependCallerDeprecatoryName, gelemMDeprecatoryName, gelemMSum)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.List (mapAccumL)
import qualified Data.Sequence as S
import Control.Lens ((.~))

-- | AddX was (maybe) optimized for correctness when parsing text from usersDeprecatoryName.
-- AddX is optimized for ease of loading new data into the graph.

-- | (At n) represents something already extant in the graph.
-- Leaf and RelX represent something that *might* already exist; it will
-- be searched for. If found, it becomes an At; if not, it is created, and
-- becomes an At.

isAt, isAbsent :: AddX -> Bool
isAbsent Absent = True
isAbsent _ = False
isAt (At _) = True
isAt _ = False

isValid :: AddX -> Bool
isValid (RelX _ [_] [Absent,Absent]) = False
isValid (RelX _ [_] [_,_]) = True
isValid (RelX _ js  ms) = (not $ any isAbsent $ middle)
                           && all isValid ms
                           && length js + 1 == length ms
  where middle = tail . reverse . tail $ ms
isValid _ = True

extractTplt :: AddX -> Expr
extractTplt (RelX _ js as) = Tplt $ ja ++ map (\(JointX s) -> s) js ++ jz
  where (ja,jz) = (f $ head as, f $ last as)
        f Absent = []
        f _ = [""]

-- Dwt.prettyPrint $ fr $ parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
prettyPrint :: AddX -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Int -> AddX -> IO () -- Int = indentation level
  it k (RelX _ js (m:ms)) = do
    putStrLn $ space k ++ "AddX: "
    it (k+1) m
    let f (j,m) = do putStrLn $ (space $ k+1) ++ show j
                     it (k+1) m
    mapM_ f $ zip js ms
  it k l = putStrLn $ space k ++ show l

addExprDeprecatoryName :: AddX -> StateT RSLT (Either DwtErrDeprecatoryName) Node
addExprDeprecatoryName (At n) = get >>= lift . flip gelemMDeprecatoryName n >> return n
addExprDeprecatoryName Absent = lift $ Left (Impossible
  , mAddX .~ Just Absent $ noErrOpts, "execAddX.")
addExprDeprecatoryName (LeafX e) = qPutStDeprecatoryName $ QLeaf e
addExprDeprecatoryName q@(RelX _ js as) = do
  ms <- mapM addExprDeprecatoryName $ filter (not . isAbsent) as
  t <- qPutStDeprecatoryName $ QLeaf $ extractTplt q
  qPutStDeprecatoryName $ QRel (QAt t) (map QAt ms)

addExprSum :: AddX -> StateT RSLT (Either DwtErrSum) Node
addExprSum (At n) = get >>= lift . flip gelemMSum n >> return n
addExprSum Absent = lift $ Left (Impossible
  , [ErrAddX Absent], "execAddX.")
addExprSum (LeafX e) = qPutStSum $ QLeaf e
addExprSum q@(RelX _ js as) = do
  ms <- mapM addExprSum $ filter (not . isAbsent) as
  t <- qPutStSum $ QLeaf $ extractTplt q
  qPutStSum $ QRel (QAt t) (map QAt ms)

-- let Right (_,g) = runStateT (mapM (addExprDeprecatoryName . fr . parse expr "" ) ["a # b", "# c", "## d #"]) empty
