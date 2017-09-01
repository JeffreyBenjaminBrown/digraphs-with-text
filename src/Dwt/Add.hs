module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Graph
import Dwt.Search
import Dwt.Parse (AddX(..), Level, JointX(..), EO)
import Dwt.Util (fr, maxNode)
import Data.List (mapAccumL)
import qualified Data.Sequence as S

-- | AddX was (maybe) optimized for correctness when parsing text from users.
-- Adder is optimized for ease of loading new data into the graph.

-- | (At n) represents something already extant in the graph.
-- Leaf and RelX represent something that *might* already exist; it will
-- be searched for. If found, it becomes an At; if not, it is created, and
-- becomes an At.

type Adder = AddX

isAt, isAbsent :: Adder -> Bool
isAbsent Absent = True
isAbsent _ = False
isAt (At _) = True
isAt _ = False

isValid :: Adder -> Bool
isValid (RelX _ [_] [Absent,Absent]) = False
isValid (RelX _ [_] _) = True -- TODO: check for no internal Absent
isValid (RelX _ js  ms) = (not $ any isAbsent $ middle)
                           && all isValid ms
                           && length js + 1 == length ms
  where middle = tail . reverse . tail $ ms
isValid _ = True

extractTplt :: Adder -> Expr
extractTplt (RelX _ js as) = Tplt $ ja ++ map (\(JointX s) -> s) js ++ jz
  where (ja,jz) = (f $ head as, f $ last as)
        f Absent = []
        f _ = [""]

-- Dwt.prettyPrint $ fr $ parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
prettyPrint :: Adder -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Int -> Adder -> IO () -- Int = indentation level
  it k (RelX _ js (m:ms)) = do
    putStrLn $ space k ++ "Adder: "
    it (k+1) m
    let f (j,m) = do putStrLn $ (space $ k+1) ++ show j
                     it (k+1) m
    mapM_ f $ zip js ms
  it k l = putStrLn $ space k ++ show l

mapac :: RSLT -> Adder -> (RSLT, Adder)
mapac g (At n) = (g, At n)
mapac g Absent = (g, Absent)
mapac g (LeafX s) = either left right $ qPut g $ QLeaf s where
  left s = error $ "mapac: " ++ s
  right (g',n) = (g', At n)
mapac g a@(RelX _ js as) = (g2, At n) where
  (g1, as1) = mapAccumL mapac g as
  mbrQueries = map (QAt . \(At n) -> n) as1
  tpltQuery = QLeaf $ extractTplt a
  (g2, n) = fr $ qPut g1 $ QRel tpltQuery mbrQueries
  -- TODO: fr is not safe here, because tplQuery might not find a tplt
    -- question
      -- How to lift a fold|map|both into the Either monad? - Stack Overflow 
      -- https://stackoverflow.com/questions/45991542/how-to-lift-a-foldmapboth-into-the-either-monad
    -- answers, maybe
      -- https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Accum.html
      -- traverse, foldM
