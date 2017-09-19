{-# LANGUAGE FlexibleContexts #-}

module Dwt
  ( module Data.Graph.Inductive
  , module Dwt
  , module Dwt.Edit
  , module Dwt.Measure
  , module Dwt.Hash.Insert
  , module Dwt.Hash.Parse
  , module Dwt.ParseUtils
  , module Dwt.Search.Branch
  , module Dwt.Search.QNode
  , module Dwt.Search.Parse
  , module Dwt.Show
  , module Dwt.MkTplt
  , module Dwt.Types
  , module Dwt.UI
  , module Dwt.Util
  ) where

import Data.Graph.Inductive (Node,Edge,LNode,LEdge,empty)

import Dwt.Measure
import Dwt.Edit
import Dwt.Hash.Insert
import Dwt.Hash.Parse
import Dwt.ParseUtils
import Dwt.Search.Branch
import Dwt.Search.QNode
import Dwt.Search.Parse
import Dwt.Show
import Dwt.MkTplt
import Dwt.Types
import Dwt.UI
import Dwt.Util

import qualified Data.Maybe as Mb


-- ==== shorthand (can't go in Util.hs; depends on more)
-- mindmap things
ns = QNodeSpec

p :: (Applicative f) => a -> f a
p = pure

-- monad things
(n, j) = (Mb.Nothing, Mb.Just)
type EM = Either String
