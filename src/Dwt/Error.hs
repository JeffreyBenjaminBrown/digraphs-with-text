module Dwt.Error where
-- TODO: This commented out stuff would form an import cycle.
-- Separate Dwt.Types from the rest (inc. Dwt.Graph)

import Data.Graph.Inductive (Node)
-- import Dwt.Graph
-- import Dwt.Search 

type DwtErr = (BaseErr, ErrOpts, String)

data ErrOpts = ErrOpts -- { _mNode :: Maybe Node
                       -- , _mExpr :: Maybe Expr
                       -- , _mQNode :: Maybe QNode)

noErrOpts :: ErrOpts
noErrOpts = ErrOpts

data BaseErr = Legacy -- | for when the String has all the info
             | FoundNo | FoundMany | NonTplt | ArityMismatch | Impossible
  deriving Show
