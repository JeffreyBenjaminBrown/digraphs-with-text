module Dwt.Add where

import Data.Graph.Inductive hiding (empty)
import Dwt.Graph
import Dwt.Parse (AddX(..), Level, JointX(..), EO)

import Data.Sequence

-- whereas AddX is optimized for correctness when parsing human-entered data,
-- Loader is optimized for ease of loading new data into the graph
data Loader = Loader [Loader]
            | Joint String
            | Leaf String
            | At Node
            | Absent deriving (Show)

loader :: AddX -> Loader
loader (LeafX "") = Absent
loader (LeafX s) = Leaf s
loader (RelX _ a j more b) = Loader $
  concatMap f ((a,j) : more) ++ [loader b]
  where f (a, JointX s) = [loader a, Joint s]
