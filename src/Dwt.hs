    {-# LANGUAGE FlexibleContexts #-}

    module Dwt
      ( module Data.Graph.Inductive

      , module Dwt

      , module Dwt.Util
      , module Dwt.Graph
      , module Dwt.Search
      , module Dwt.Show
      , module Dwt.FileIO
      , module Dwt.Parse
      , module Dwt.MmParse
      , module M
      , module L
      ) where

    import Data.Graph.Inductive

    import Dwt.Util
    import Dwt.Graph
    import Dwt.Search
    import Dwt.Show
    import Dwt.FileIO
    import Dwt.Parse
    import Dwt.MmParse

    import qualified Data.Text as T
    import qualified Data.List as L
    import qualified Data.Map as M
    import qualified Data.Maybe as Mb

-- shorthand
  -- mindmap things
    ns = NodeSpec
    (qn, qs) = (QNode, QStr)
    qt = QTplt . map (T.unpack . T.strip . T.pack) . _splitStringForTplt
      -- is nearly mkTplt, but swaps QTplt for Tplt

    p :: (Applicative f) => a -> f a
    p = pure

  -- monad things
    (n, j) = (Mb.Nothing, Mb.Just)
    -- p = pure -- todo ? GHC will not compile, but GHCI accepts
    fr = fromRight
    type EM = Either String
