module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Graph
import Dwt.Search
import Dwt.Parse (AddX(..), Level, JointX(..), EO)
import Dwt.Util (maxNode)
import Data.List (mapAccumL)
import qualified Data.Sequence as S

-- | AddX was (maybe) optimized for correctness when parsing text from users.
-- Adder is optimized for ease of loading new data into the graph.

-- | (At n) represents something already extant in the graph.
-- Leaf and RelAdder represent something that *might* already exist; it will
-- be searched for. If found, it becomes an At; if not, it is created, and
-- becomes an At.

data Adder = Absent
           | Leaf String
           | RelAdder [JointX] [Adder]
           | At Node deriving (Show)

isAt, isAbsent :: Adder -> Bool
isAbsent Absent = True
isAbsent _ = False
isAt (At _) = True
isAt _ = False

isValid :: Adder -> Bool
isValid (RelAdder [_] [Absent,Absent]) = False
isValid (RelAdder [_] _) = True
isValid (RelAdder js  ms) = (not $ any isAbsent $ middle)
                           && all isValid ms
                           && length js + 1 == length ms
  where middle = tail . reverse . tail $ ms
isValid _ = True

extractTplt :: Adder -> Expr
extractTplt (RelAdder js as) = Tplt $ ja ++ map (\(JointX s) -> s) js ++ jz
  where (ja,jz) = (f $ head as, f $ last as)
        f Absent = []
        f _ = [""]

--  let pairs = zip (map adderString as) (map jointString js)
--      adderString Absent = "" :: String
--      adderString _ = " _ "
--      jointString (JointX s) = " " ++ s ++ " " :: String
--      splitPair (a,b) = [a,b] :: [String]
--  in concat (adderString a : concatMap splitPair pairs)
--    -- use mkTplt

-- Dwt.prettyPrint $ fr $ adder <$> parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
prettyPrint :: Adder -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Int -> Adder -> IO () -- Int = indentation level
  it k (RelAdder js (m:ms)) = do
    putStrLn $ space k ++ "Adder: "
    it (k+1) m
    let f (j,m) = do putStrLn $ (space $ k+1) ++ show j
                     it (k+1) m
    mapM_ f $ zip js ms
  it k l = putStrLn $ space k ++ show l

adder :: AddX -> Adder
adder (LeafX "") = Absent
adder (LeafX s) = Leaf s
adder a@(RelX _ js as) = let a = RelAdder js $ map adder as
  in case isValid a of True -> a
                       False -> error $ "adder: invalid rel: " ++ show a

mapac :: RSLT -> Adder -> (RSLT, Adder)
mapac g (At n) = (g, At n)
mapac g Absent = (g, Absent)
mapac g (Leaf s) = either left right $ qPut g $ QLeaf $ Word s where
  left s = error $ "mapac: " ++ s
  right (g',n) = (g', At n)
--mapac g (RelAdder js as) = _ where
--  (g1, as1) = mapAccumL mapac g as
--  (g2, _) = 
