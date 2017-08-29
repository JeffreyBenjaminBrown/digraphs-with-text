module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Graph
import Dwt.Parse (AddX(..), Level, JointX(..), EO)
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

--add :: Adder -> RSLT -> Either String (Adder, RSLT)
--add Absent    _ = Left "Attempt to add Absent to graph."
--add (Leaf s)  g = let g' = insLeaf (Word s) g
--                      (_,n) = nodeRange g' -- TODO: change for speed
--                  in Right (At n, g')
--add (At n)    g = Right (At n, g)
--add a@(RelAdder _ _) g = if isValid a -- recursive test, so only run once
--  then addRel (S.empty) a g
--  else Left $ "attempt to add invalid Adder: " ++ show a
--  where -- we'll transfer Adders from the RelAdder to the Seq
--    addRel :: S.Seq Node -> Adder -> RSLT -> Either String (Adder, RSLT)
--    addRel (found S.|> n) (RelAdder js as) g
--    addRel found (RelAdder js (At n : as)) g =
--    addRel found (RelAdder js []) g = 

-- testing mapAccumL
-- https://www.reddit.com/r/haskell/comments/6wmrwh/how_to_process_a_tree_of_orderdependent_insertion/

type Acc = Int
data Test = Done Test | TLeaf Int | TBranch [Test] deriving Show

doLeaf :: Acc -> Test -> (Acc, Test)
doLeaf acc (TLeaf i) = (acc+i, Done $ TLeaf $ acc+i)

doBranch :: Acc -> Test -> (Acc, Test)
doBranch acc (TBranch ts) =
  let branchFree, allDone :: [Test]
      (a2, branchFree) = mapAccumL doBranch acc ts
      (a3, allDone) = mapAccumL doLeaf acc branchFree
  in (a3, Done $ TBranch $ allDone)
doBranch acc t = (acc, t)

--test = mapAccumL _ [0] [TLeaf 1, TLeaf 2
--                        , TBranch [TLeaf 3, TLeaf 4]
--                        , TLeaf 5]
