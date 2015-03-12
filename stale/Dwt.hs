{-# LANGUAGE NoImplicitPrelude, QuasiQuotes, DataKinds #-}
module Dwt where

  import BasePrelude
  import Record
  import Record.Lens

-- imports
  import qualified Data.List as List
  import qualified Data.Map as Map
  import qualified Data.Sequence as Seq
  import Data.Maybe
  import Data.Functor
  
  import Reln
  
-- miscellaneous utilities
  prependIfAbsent elt list
    | elem elt list = list
    | otherwise = elt : list

-- Gnode: A node in a graph
  -- Gnodes refer to each other through GnodeIdxs
  newtype GnodeIdx = GnodeIdx { gnodeIdx :: Int }
    -- Ord because they are keys for [a Map in a Graph']
    deriving (Show, Ord, Eq)

  data Gnode = Gnode { gIdx :: GnodeIdx
                     , tx :: String
                     , conns :: Map.Map Reln [GnodeIdx] }
         deriving (Show, Eq)

  -- TODO, brevity: lenses, record update syntax
  gnodeConn :: Gnode -> Reln -> Gnode -> Gnode
  gnodeConn connectingAs reln connectingTo =
    connectingTo { conns = Map.insert reln relvs' $ conns connectingTo }
    where relvs = Map.lookup reln $ conns connectingTo
          relvs' = fromMaybe
            [gIdx connectingAs] -- fromMaybe returns this if relvs = Nothing
            $ (prependIfAbsent $ gIdx connectingAs) 
            <$> relvs

  gnodeDisc :: Gnode -> Reln -> Gnode -> Gnode
  gnodeDisc removing reln removingFrom
    -- I am using "undefined" to mean "should not happen".
    | isJust relvs =
      removingFrom { conns = 
        Map.filter (/= [])
        $ Map.insert reln relvs' 
        $ conns removingFrom 
        }
    | otherwise = undefined
    where relvs = Map.lookup reln $ conns removingFrom
          relvs' = (List.delete $ gIdx removing)
            $ fromMaybe undefined relvs

-- Graph
  data Graph = Graph {  dirdTxs :: Map.Map DirdTxIdx DirdTx
                     , gnodes :: Map.Map GnodeIdx Gnode }
    deriving Show

  {- Want:
    conn :: Graph ->
    conn connectingAs reln connectingTo graph =
    -}

-- EOF

