{-# LANGUAGE NoImplicitPrelude, QuasiQuotes, DataKinds #-}
module Reln where

  import BasePrelude
  import Record
  import Record.Lens
  
-- Reversible.
  class Rev a where
    rev :: a -> a
  
-- Direction in a graph. Unused except by the Reln type.
  data Dirn = Pred | Succ
    deriving (Eq, Show)
  
  instance Ord Dirn where -- so that Reln can be a key in a Map
    (<=) Succ Pred = False
    (<=) _ _ = True
  
  instance Rev Dirn where
    rev Pred = Succ
    rev Succ = Pred
  
-- DirdTx: Directed text, to pair with a Dirn
  data DirdTx = DirdTx { predTx :: String
                       , succTx :: String }
    deriving (Eq, Show, Ord)
  
  -- an index into a DirdTx array
  newtype DirdTxIdx = DirdTxIdx { dirdTxIdx :: Int }
    deriving (Eq, Show, Ord)

-- Reln: A relationship between graph nodes.
  data Reln = Reln { dirn :: Dirn
                   , dtIdx :: DirdTxIdx }
    deriving (Eq, Show, Ord)

  instance Rev Reln where
    -- TODO, brevity: use lenses
    rev r = Reln { dirn = rev $ dirn r
                 , dtIdx = dtIdx r }

-- EOF

