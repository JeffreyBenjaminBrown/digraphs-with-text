module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Graph
import Dwt.Parse (AddX(..), Level, JointX(..), EO)

-- import Data.Sequence hiding (replicate)

-- Whereas AddX is optimized for correctness when parsing human-entered data,
-- Adder is optimized for ease of loading new data into the graph.
data Adder = Adder [Adder]
            | Joint String
            | Leaf String
            | At Node
            | Absent deriving (Show)

-- pp $ fr $ loader <$> parse expr "" "a # b ##z # (d # e) # e ## f ## g # h"
prettyPrint :: Adder -> IO ()
prettyPrint = it 0 where
  space :: Int -> String
  space k = replicate (4*k) ' '
  it :: Int -> Adder -> IO ()
  it k (Adder ls) = do
    putStrLn $ space k ++ "Adder"
    mapM_ (it $ k+1) ls
  it k l = putStrLn $ space k ++ show l

loader :: AddX -> Adder
loader (LeafX "") = Absent
loader (LeafX s) = Leaf s
loader (RelX _ a j more b) = Adder $
  concatMap f ((a,j) : more) ++ [loader b]
  where f (a, JointX s) = [loader a, Joint s]

