{-# LANGUAGE TemplateHaskell #-}

module Dwt.Types (
  MbrPos, Arity
  , RSLT, Expr(..), RSLTEdge(..), RelRole(..), CollRole(..)
  , Mbrship(..), RelVarSpec, RelNodeSpec
  , NodeOrVar(..), RelSpec
  , QNodeOrVar(..), QRelSpec
  , QNode(..), Level, Joint(..), EO(..)
  , DwtErr, ErrBase(..), ErrOpt(..)
  , errBase, errOpts, errString -- lenses
  ) where

import Data.Graph.Inductive
import Data.Map as Map
import Control.Lens hiding (Level)
import Data.String (IsString, fromString)

-- == Fundamental
type Arity = Int
type MbrPos = Int -- k members of k-ary Rel, MbrPos(ition) values [1..k]

type RSLT = Gr Expr RSLTEdge -- reflective set of labeled tuples
data Expr = Word String | Fl Float -- these two are similarly atomic
  | Rel | Tplt [String]
  | Coll -- each uses a CollPrinciple like "and" or "or"
  | RelSpecExpr RelVarSpec deriving(Show,Read,Eq,Ord)
    -- The RelVarSpec specifies the variable members.
    -- Edges specify the concrete (addressed) members.

data RSLTEdge = RelEdge RelRole | CollEdge CollRole deriving(Show,Read,Eq,Ord)
  -- | only Rels and Colls emit edges, have subexpressions
  -- "RSLTEdgeLabel" would be a more accurate name.
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
  -- | a k-ary Rel emits one TpltRole and k RelMbrs
data CollRole = CollPrinciple | CollMbr deriving(Show,Read,Eq,Ord)
  -- | a Col emits one CollPrinciple, any number of CollMbrs
-- TODO: A CollPrinciple currently can point to anything. It would be
  -- cleaner, and closer to truth, to pointonly to transitive Tplts.
  -- Exceptions: "some of," "no more than," "exactly" would use unary Tplts.
    -- As in "some of {Ghandi, Einstein, Peter Pan} existed".

-- == Specifying kinds of relationships
data Mbrship = It | Any | Up | Down deriving (Show,Read,Eq,Ord)
data QNodeOrVar = QNodeSpec QNode | QVarSpec Mbrship deriving (Show,Eq)
data NodeOrVar = NodeSpec Node | VarSpec Mbrship
  deriving (Show,Read,Eq)
type RelSpec =  Map.Map RelRole NodeOrVar
type QRelSpec = Map.Map RelRole QNodeOrVar -- if well-formed, includes
  -- one Tplt t, one MbrPos k for all k in [1, arity t], and nothing else

-- at the TpltRole key is always a concrete QNodeSpec
type RelVarSpec =  Map.Map RelRole Mbrship
type RelNodeSpec = Map.Map RelRole Node -- set-complement of RelVarSpec

-- == Parsing Hash expressions
data QNode = At Node -- for when you know the expression's node
  | Absent | QLeaf Expr | QRel [Joint] [QNode] deriving (Show, Eq)
  -- Every rel has at least one joint, and potentially members on either side
  -- If there are more, the list of pairs stores them.
type Level = Int -- in "cats like you because you like them", the "because"
  -- relationship is level 2, and the "like" relationships are level 1

data Joint = Joint String deriving (Show, Eq)
  -- in "you #like peaches #at noon", "like" and "at" are joints
instance IsString Joint where fromString = Joint

data EO = EO     -- EO = "expression orderer"
  { open :: Bool -- open = "more expressions can be concatentated into it"
                 -- In b@(QRel (EO x _) _ _), x is true until
                 -- b has been surrounded by parentheses.
  , inLevel :: Level } deriving (Eq)
instance Show EO where
  show (EO x y) = "(EO " ++ show x ++ " " ++ show y ++ ")"
instance Ord EO where -- Open > closed. If those are equal, ## > #, etc.
  EO a b <= EO c d = a <= c && b <= d

-- == Errors
type DwtErr = (ErrBase, [ErrOpt], String)

data ErrBase = Legacy -- | for when the String has all the info
             | FoundNo | FoundMany | FoundWrongKind
             | ArityMismatch | ConstructorMistmatch
             | NotRelSpecExpr | NotTplt | NotColl | NotLeaf
             | Invalid -- | (MbrPos 0), for instance, is ill-formed
             | Impossible
  deriving (Show, Eq)

data ErrOpt =  -- ^ New error style: sum type
  ErrNode Node | ErrEdge Edge | ErrExpr Expr
  | ErrEdgeLab RSLTEdge | ErrRelRole RelRole | ErrMbrship Mbrship
  | ErrRelSpec QRelSpec | ErrQNode QNode
  deriving (Show, Eq)

errBase :: Lens' DwtErr ErrBase
errBase = _1
errOpts :: Lens' DwtErr [ErrOpt]
errOpts = _2
errString :: Lens' DwtErr String
errString = _3
