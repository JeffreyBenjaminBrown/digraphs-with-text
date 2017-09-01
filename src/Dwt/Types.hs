module Dwt.Types (
  MbrPos, Arity
  , RSLT, Expr(..), RSLTEdge(..), RelRole(..), CollRole(..)
  , Mbrship(..), AddressOrVar(..), RelVarSpec, RelNodeSpec, RelSpec
  , QNode(..), DwtErr(..), noErrOpts, BaseErr(..)
  ) where

import Data.Graph.Inductive
import Data.Map as Map

type Arity = Int
type MbrPos = Int -- k members of k-ary Rel, MbrPos values [1..k]

type RSLT = Gr Expr RSLTEdge -- reflective set of labeled tuples
data Expr = Word String | Fl Float -- these two are similarly atomic
          | Rel
          | Tplt [String]
          | Coll -- each uses a CollPrinciple like "and" or "or"
          | RelSpecExpr RelVarSpec
            -- The RelVarSpec specifies the variable members.
            -- Edges specify the concrete (addressed) members.
          deriving(Show,Read,Eq,Ord)

data RSLTEdge = RelEdge RelRole | CollEdge CollRole
              deriving(Show,Read,Eq,Ord)
  -- only Rels and Colls emit edges, have subexpressions
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
  -- a k-ary Rel emits one TpltRole and k RelMbrs
data CollRole = CollPrinciple | CollMbr deriving(Show,Read,Eq,Ord)
  -- a Col emits one CollPrinciple, any number of CollMbrs
-- TODO: A CollPrinciple currently can point to anything. It would be
  -- cleaner, and closer to truth, to pointonly to transitive Tplts.
  -- Exceptions: "some of," "no more than," "exactly" would use unary Tplts.
    -- As in "some of {Ghandi, Einstein, Peter Pan} existed".

-- == for RelSpec
data Mbrship = It | Any | Up | Down
  deriving (Show,Read,Eq,Ord)
data AddressOrVar -- might be addressed; else is Mbrship variable
  = VarSpec Mbrship | NodeSpec Node deriving(Show,Read,Eq,Ord)

-- at the TpltRole key is always a concrete NodeSpec
type RelVarSpec = Map.Map RelRole Mbrship
type RelNodeSpec = Map.Map RelRole Node -- set-complement of RelVarSpec
type RelSpec =     Map.Map RelRole AddressOrVar -- if well-formed, keys
  -- include a single Tplt, and MbrPos k for all k in [1, Tplt arity]

-- == for Queries
data QNode = QAt Node -- when you already know the Node
           | QLeaf Expr -- when you don't but you know its contents
           | QRel QNode [QNode]
  deriving (Show, Eq)

-- == for Errors
type DwtErr = (BaseErr, ErrOpts, String)

data ErrOpts = ErrOpts { _mNode :: Maybe Node
                       , _mExpr :: Maybe Expr
                       , _mQNode :: Maybe QNode }

noErrOpts :: ErrOpts
noErrOpts = ErrOpts Nothing Nothing Nothing

data BaseErr = Legacy -- | for when the String has all the info
             | FoundNo | FoundMany | NonTplt | ArityMismatch | Impossible
  deriving Show
