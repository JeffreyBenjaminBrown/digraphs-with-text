{-# LANGUAGE FlexibleContexts #-}

module Dwt
  ( module Data.Graph.Inductive
  , module Dwt
  , module Dwt.Edit
  , module Dwt.Initial.Measure
  , module Dwt.Hash.Insert
  , module Dwt.Hash.Parse
  , module Dwt.Initial.ParseUtils
  , module Dwt.Second.Graph
  , module Dwt.UI.Parse
  , module Dwt.Query.Main
  , module Dwt.Query.Misfits
  , module Dwt.Show.Expr
  , module Dwt.Second.QNode
  , module Dwt.Second.MkTplt
  , module Dwt.Initial.Types
  , module Dwt.UI.Terminal
  , module Dwt.Initial.Util
  ) where

import Data.Graph.Inductive (Node,Edge,LNode,LEdge,empty)

import Dwt.Initial.Measure
import Dwt.Edit
import Dwt.Hash.Insert
import Dwt.Hash.Parse
import Dwt.Initial.ParseUtils
import Dwt.Second.Graph
import Dwt.UI.Parse
import Dwt.Query.Main
import Dwt.Query.Misfits
import Dwt.Show.Expr
import Dwt.Second.QNode
import Dwt.Second.MkTplt
import Dwt.Initial.Types
import Dwt.UI.Terminal
import Dwt.Initial.Util

import qualified Data.Maybe as Mb


-- ==== shorthand (can't go in Util.hs; depends on more)
-- mindmap things
ns = QNodeSpec

p :: (Applicative f) => a -> f a
p = pure

-- monad things
(n, j) = (Mb.Nothing, Mb.Just)
type EM = Either String
