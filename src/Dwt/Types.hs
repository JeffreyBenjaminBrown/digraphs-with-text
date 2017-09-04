{-# LANGUAGE TemplateHaskell #-}

module Dwt.Types (
  MbrPos, Arity
  , RSLT, Expr(..), RSLTEdge(..), RelRole(..), CollRole(..)
  , Mbrship(..), AddressOrVar(..), RelVarSpec, RelNodeSpec, RelSpec
  , QNode(..)
  , DwtErr(..), mExpr, mNode, mQNode
  , errBase, errOpts, errString
  , ErrOpts(..), noErrOpts, ErrBase(..)
  ) where

import Data.Graph.Inductive
import Data.Map as Map
import Control.Lens

-- == Fundamental
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


-- == Specifying kinds of relationships
data Mbrship = It | Any | Up | Down
  deriving (Show,Read,Eq,Ord)
data AddressOrVar -- might be addressed; else is Mbrship variable
  = VarSpec Mbrship | NodeSpec Node deriving(Show,Read,Eq,Ord)

-- at the TpltRole key is always a concrete NodeSpec
type RelVarSpec =  Map.Map RelRole Mbrship
type RelNodeSpec = Map.Map RelRole Node -- set-complement of RelVarSpec
type RelSpec =     Map.Map RelRole AddressOrVar -- if well-formed, keys
  -- include a single Tplt, and MbrPos k for all k in [1, Tplt arity]


-- == Queries
data QNode = QAt Node -- when you already know the Node
           | QLeaf Expr -- when you don't but you know its contents
           | QRel QNode [QNode]
  deriving (Show, Eq)


-- == Errors
data ErrBase = Legacy -- | for when the String has all the info
             | FoundNo | FoundMany
             | ArityMismatch | ConstructorMistmatch
             | NotRelSpecExpr | NotTplt | NotColl | NotLeaf
             | Impossible
  deriving Show

type DwtErr = (ErrBase, ErrOpts, String)
errBase :: Lens' DwtErr ErrBase
errBase = _1
errOpts :: Lens' DwtErr ErrOpts
errOpts = _2
errString :: Lens' DwtErr String
errString = _3

data ErrOpts = ErrOpts { _mNode :: Maybe Node
                       , _mExpr :: Maybe Expr
                       , _mQNode :: Maybe QNode } deriving Show
-- | adjust it like "noErrOpts L.& mNode L..~ (Just 2)"

makeLenses ''ErrOpts

noErrOpts :: ErrOpts
noErrOpts = ErrOpts Nothing Nothing Nothing
