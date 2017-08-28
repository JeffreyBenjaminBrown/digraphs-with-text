    {-# LANGUAGE FlexibleContexts #-}

    module Dwt
      ( module  Dwt
      , module Dwt.Add
      , module Dwt.FileIO
      , module Dwt.Graph
      , module Dwt.Parse 
      , module Dwt.Search
      , module Dwt.Show
      , module Dwt.UI
      , module Dwt.Util
      ) where

    import Data.Graph.Inductive

    import Dwt.Add
    import Dwt.FileIO
    import Dwt.Graph
    import Dwt.Parse
    import Dwt.Search
    import Dwt.Show
    import Dwt.UI
    import Dwt.Util

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
