module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Graph
import Dwt.Parse (AddX(..), Level, JointX(..), EO)

-- Whereas AddX is optimized for correctness when parsing human-entered data,
-- Adder is optimized for ease of loading new data into the graph.
data Adder = Absent
           | Leaf String
           | RelAdder [JointX] [Adder]
           | At Node deriving (Show)

isAbsent :: Adder -> Bool
isAbsent Absent = True
isAbsent _ = False

isValid :: Adder -> Bool
isValid (RelAdder [j] [Absent,Absent]) = False
isValid (RelAdder [j] _) = True
isValid (RelAdder _  ms) = not $ any isAbsent $ middle
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

add :: Adder -> RSLT -> Either String (Adder, RSLT)
add Absent    _ = Left "Attempt to add Absent to graph."
add (Leaf s)  g = let g' = insLeaf (Word s) g
                      (_,n) = nodeRange g'
  in Right (At n, g')
-- add (Adder _) = _
add (At n)    g = Right (At n, g)
