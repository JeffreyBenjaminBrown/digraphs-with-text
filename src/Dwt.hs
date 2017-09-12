    {-# LANGUAGE FlexibleContexts #-}

    module Dwt
      ( module Dwt
      , module Dwt.Add
      , module Dwt.Graph
      , module Dwt.Leaf
      , module Dwt.Parse 
      , module Dwt.Search.Local
      , module Dwt.Search.Recursive
      , module Dwt.Show
      , module Dwt.Types
      , module Dwt.UI
      , module Dwt.Util
      ) where

    import Data.Graph.Inductive

    import Dwt.Add
    import Dwt.Graph
    import Dwt.Leaf
    import Dwt.Parse
    import Dwt.Search.Local
    import Dwt.Search.Recursive
    import Dwt.Show
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

-- shorthand (can't go in Util.hs; depends on more)
  -- mindmap things
    ns = NodeSpecQ

    p :: (Applicative f) => a -> f a
    p = pure

  -- monad things
    (n, j) = (Mb.Nothing, Mb.Just)
    type EM = Either String
