{-# LANGUAGE FlexibleContexts #-}

module Dwt
  ( module Data.Graph.Inductive
  , module Dwt
  , module Dwt.Graph
  , module Dwt.Leaf
  , module Dwt.Hash.Insert
  , module Dwt.Hash.ParseEo
  , module Dwt.ParseUtils
  , module Dwt.Search.Branch
  , module Dwt.Search.Node
  , module Dwt.Search.Parse
  , module Dwt.Show
  , module Dwt.Tplt
  , module Dwt.Types
  , module Dwt.UI
  , module Dwt.Util
  ) where

import Data.Graph.Inductive (Node,Edge,LNode,LEdge,empty)

import Dwt.Leaf
import Dwt.Graph
import Dwt.Hash.Insert
import Dwt.Hash.ParseEo
import Dwt.ParseUtils
import Dwt.Search.Branch
import Dwt.Search.Node
import Dwt.Search.Parse
import Dwt.Show
import Dwt.Tplt
import Dwt.Types
import Dwt.UI
import Dwt.Util

import Control.Monad (void)
import qualified Text.Megaparsec as Mp

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Read as R
import qualified Data.Text as T

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Brick.Main as B

-- ==== shorthand (can't go in Util.hs; depends on more)
-- mindmap things
ns = NodeSpec

p :: (Applicative f) => a -> f a
p = pure

-- monad things
(n, j) = (Mb.Nothing, Mb.Just)
type EM = Either String
