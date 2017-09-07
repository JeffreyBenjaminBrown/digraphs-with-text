module Dwt.Add where

import Data.Graph.Inductive hiding (empty, prettyPrint)
import Dwt.Types
import Dwt.Graph
import Dwt.Search
import Dwt.Util (fr, maxNode, prependCaller, gelemMDe)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.List (mapAccumL)
import qualified Data.Sequence as S
import Control.Lens ((.~))

-- | AddX was (maybe) optimized for correctness when parsing text from users.
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

execAddX' :: RSLT -> AddX -> Either DwtErr (Node, RSLT)
execAddX' g (At n) = do gelemMDe g n
                        return (n,g)
execAddX' g Absent = Left (Impossible
                          , mAddX .~ Just Absent $ noErrOpts, "execAddX.")
execAddX' g (LeafX e) = runStateT (qPutDeSt $ QLeaf e) g
execAddX' g r@(RelX _ js as) = do
  let qt = QLeaf $ extractTplt r
  Left $ (Legacy, noErrOpts,
          "Chaining qPutDeSt operations will be easier in the State monad.")

--execAddX'' :: RSLT -> AddX -> Either DwtErr (Node, RSLT)

execAddX :: RSLT -> AddX -> RSLT
execAddX g a = fst $ mapac g a

mapac :: RSLT -> AddX -> (RSLT, AddX)
mapac g (At n) = (g, At n)
mapac g Absent = (g, Absent)
mapac g (LeafX s) = either left right $ qPut g $ QLeaf s where
  left s = error $ "mapac: " ++ s
  right (g',n) = (g', At n)
mapac g a@(RelX _ js as) =
  let (g1, as1) = mapAccumL mapac g as
  in case qPutDe g (QLeaf $ extractTplt a)
  of Left e -> error $ "mapac: " ++ show e
     Right (g2, tn) -> case qPutDe g2 $ QRel (QAt tn)
                                      $ map (QAt . \(At n) -> n) as1
                       of Right (g3,n) -> (g3, At n)
                          Left e -> error $ "mapac: " ++ show e

mapacSt :: AddX -> State RSLT (Either DwtErr AddX)
mapacSt a@(RelX _ js as) = get >>= \g -> do-- TODO: prependCaller "mapacSt: "..
  -- TODO: complete
  let (as1, g1) = flip runState g $ mapM mapacSt as
  return $ Right a
mapacSt a@(At _) = return $ Right a -- | pitfall ! assumes existence of At values
mapacSt Absent = return $ Right Absent
mapacSt (LeafX x) =  get >>= \g -> case qPutDe g $ QLeaf x of
  Left e  -> return $ prependCaller "mapac': " $ Left e
  Right (g',n) -> put g' >> return (Right $ At n)

mapac' :: RSLT -> AddX -> (RSLT, Either DwtErr AddX)
mapac' g (At n) = (g, Right $ At n)
  -- TODO ?(slow) test that it's in the graph
    -- better: perform such tests only when the At is created
mapac' g Absent = (g, Right Absent)
mapac' g (LeafX s) = case qPutDe g $ QLeaf s of
  Left e  -> (g, prependCaller "mapac': " $ Left e)
  Right (g',n) -> (g', Right $ At n)
mapac' g a@(RelX _ js as) =
  let (g1, as1) = mapAccumL mapac g as
  in case qPutDe g (QLeaf $ extractTplt a)
  of Left e -> error $ "mapac: " ++ show e

--     Right (g2, tn) -> case qPutDe g2 $ QRel (QAt tn)
--                                      $ map (QAt . \(At n) -> n) as1
--                       of Right (g3,n) -> (g3, At n)
--                          Left e -> error $ "mapac: " ++ show e

  -- TODO: fr is not safe here, because tplQuery might not find a tplt
    -- question
      -- How to lift a fold|map|both into the Either monad? - Stack Overflow 
      -- https://stackoverflow.com/questions/45991542/how-to-lift-a-foldmapboth-into-the-either-monad
    -- answers, maybe
      -- https://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Accum.html
      -- traverse, foldM
