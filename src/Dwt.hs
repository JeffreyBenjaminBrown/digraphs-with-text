    {-# LANGUAGE FlexibleContexts #-}

    module Dwt
      ( module Data.Graph.Inductive

      , module Dwt
      , module Dwt.Util
      , module Dwt.Graph
      , module Dwt.Search
      , module Dwt.Show
      , module Dwt.FileIO
      , module Dwt.Brick

      , module P
      , module R
      , module T
      , module L
      , module M
      , module Mb
      , module B
      ) where

    import Data.Graph.Inductive

    import Dwt.Util
    import Dwt.Graph
    import Dwt.Search
    import Dwt.Show
    import Dwt.FileIO
    import qualified Dwt.ParsecUtils as P
    import Dwt.Brick

    import Control.Monad (void)
    import Text.Megaparsec as Mp

    import qualified Text.Parsec as P
    import qualified Text.Parsec.String as P
    import qualified Text.Read as R
    import qualified Data.Text as T

    import qualified Data.List as L
    import qualified Data.Map as M
    import qualified Data.Maybe as Mb
    import qualified Brick.Main as B


-- shorthand (can't go in Util.hs; depends on more)
  -- mindmap things
    ns = NodeSpec
    (qn, qs) = (QNode, QWord)
    qt = QTplt . map (T.unpack . T.strip . T.pack) . _splitStringForTplt
      -- is nearly mkTplt, but subs QTplt for Tplt

    p :: (Applicative f) => a -> f a
    p = pure

  -- monad things
    (n, j) = (Mb.Nothing, Mb.Just)
    fr = fromRight
    type EM = Either String
