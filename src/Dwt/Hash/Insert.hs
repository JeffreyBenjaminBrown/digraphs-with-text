-- | Sample use:
-- Dwt.prettyPrint $ fr $ parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
-- let Right (_,g) = runStateT (mapM (addExpr . fr . parse expr "" ) ["a # b", "# c", "## d #"]) empty

module Dwt.Hash.Insert (prettyPrint, addExpr) where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Initial.Types
import Dwt.Query.Main
import Dwt.Initial.Measure (isAbsent)
import Dwt.Initial.Util (gelemM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)


-- | (At n) represents something already extant in the graph.
-- Leaf and QRel represent something that *might* already exist; it will
-- be searched for. If found, it becomes an At; if not, it is created, and
-- becomes an At.

prettyPrint :: QNode -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Level -> QNode -> IO ()
  it indent (QRel _ js (m:ms)) = do
    putStrLn $ space indent ++ "QNode: "
    it (indent+1) m
    let f (j,m) = do putStrLn $ (space $ indent+1) ++ show j
                     it (indent+1) m
    mapM_ f $ zip js ms
  it indent l = putStrLn $ space indent ++ show l

addExpr :: QNode -> StateT RSLT (Either DwtErr) Node
addExpr (At n) = get >>= lift . flip gelemM n >> return n
addExpr (QLeaf e) = qPutSt $ QLeaf e
addExpr (QRel isTop js as) = do
  ns <- mapM addExpr $ filter (not . isAbsent) as
  qPutSt $ QRel isTop js $ reshuffle ns as
  where reshuffle :: [Node] -> [QNode] -> [QNode]
        reshuffle [] ms = ms
        reshuffle ns (Absent:is) = Absent : reshuffle ns is
        reshuffle (n:ns) (_:is) = At n : reshuffle ns is
        -- ns is shorter or equal, so the case reshuffle _ [] is unnecessary
addExpr q = lift $ Left (Impossible, [ErrQNode q], "addExpr.")
