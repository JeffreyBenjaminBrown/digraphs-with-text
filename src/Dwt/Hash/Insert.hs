-- | Sample use:
-- Dwt.prettyPrint $ fr $ parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
-- let Right (_,g) = runStateT (mapM (addExpr . fr . parse expr "" ) ["a # b", "# c", "## d #"]) empty

module Dwt.Hash.Insert where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Types
import Dwt.Search.Node
import Dwt.Util (fr, maxNode, prependCaller, gelemM)
import Dwt.Leaf (extractTplt)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.List (mapAccumL)
import qualified Data.Sequence as S
import Control.Lens ((.~))

-- | (At n) represents something already extant in the graph.
-- Leaf and InsRel represent something that *might* already exist; it will
-- be searched for. If found, it becomes an At; if not, it is created, and
-- becomes an At.

isValid :: Insertion -> Bool
isValid (InsRel _ [_] [Absent,Absent]) = False
isValid (InsRel _ [_] [_,_]) = True
isValid (InsRel _ js  ms) = (not $ any isAbsent $ middle)
                           && all isValid ms
                           && length js + 1 == length ms
  where middle = tail . reverse . tail $ ms
isValid _ = True

prettyPrint :: Insertion -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Level -> Insertion -> IO ()
  it indent (InsRel _ js (m:ms)) = do
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
addExpr (InsLeaf e) = qPutSt $ InsLeaf e
addExpr q@(InsRel _ js as) = do
  ns <- mapM addExpr $ filter (not . isAbsent) as
  qPutSt $ InsRel blankEo js $ reshuffle ns as
  where reshuffle :: [Node] -> [Insertion] -> [Insertion]
        reshuffle [] ms = ms
        reshuffle ns (Absent:is) = Absent : reshuffle ns is
        reshuffle (n:ns) (_:is) = At n : reshuffle ns is
