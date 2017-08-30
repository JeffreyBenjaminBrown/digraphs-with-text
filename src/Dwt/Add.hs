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

isAbsent :: Adder -> Bool
isAbsent Absent = True
isAbsent _ = False

isValid :: Adder -> Bool
isValid (RelAdder [_] [Absent,Absent]) = False
isValid (RelAdder [_] _) = True
isValid (RelAdder js  ms) = (not $ any isAbsent $ middle)
                           && all isValid ms
                           && length js + 1 == length ms
  where middle = tail . reverse . tail $ ms
isValid _ = True

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
adder (RelX _ a j pairs b) = RelAdder (j : joints)
                             $ map adder $ [a] ++ members ++ [b]
  where (members,joints) = unzip pairs

-- mapac :: RSLT -> Adder -> (RSLT, Adder)
-- will use qPut
