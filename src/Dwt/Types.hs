{-# LANGUAGE TemplateHaskell #-}

module Dwt.Types (
  MbrPos, Arity
  , RSLT, Expr(..), RSLTEdge(..), RelRole(..), CollRole(..)
  , Mbrship(..), AddressOrVar(..), RelVarSpec, RelNodeSpec, RelSpec
  , QNode(..)
  , AddX(..), Level, JointX(..), EO(..)
  , DwtErr(..), mExpr, mNode, mEdge, mEdgeLab
    , mQNode, mRelRole, mRelSpec, mAddX
  , errBase, errOpts, errString
  , ErrOpt(..), noErrOpts, ErrBase(..)
  , DwtErrSum(..), ErrOptSum(..)
  , errBaseSum, errOptsSum, errStringSum
  ) where

import Data.Graph.Inductive
import Data.Map as Map
import Control.Lens hiding (Level)
import Data.String (IsString, fromString)

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
data Mbrship = It | Any | Up | Down
  deriving (Show,Read,Eq,Ord)
data AddressOrVar -- might be addressed; else is Mbrship variable
  = VarSpec Mbrship | NodeSpec Node deriving(Show,Read,Eq,Ord)

-- at the TpltRole key is always a concrete NodeSpec
type RelVarSpec =  Map.Map RelRole Mbrship
type RelNodeSpec = Map.Map RelRole Node -- set-complement of RelVarSpec
type RelSpec =     Map.Map RelRole AddressOrVar -- if well-formed, includes
  -- one Tplt t, one MbrPos k for all k in [1, arity t], and nothing else


-- == Queries
data QNode = QAt Node -- when you already know the Node
           | QLeaf Expr -- when you don't but you know its contents
           | QRel QNode [QNode]
  deriving (Show, Eq)


-- == Parsing Hash expressions
data AddX = At Node -- for when you know the expression's node. TODO: parse
          | Absent
          | LeafX Expr
          | RelX EO [JointX] [AddX]
  -- Every rel has at least one jointX, and potentially members on either side
  -- If there are more, the list of pairs stores them.
          deriving (Show, Eq)
type Level = Int -- in "cats like you because you like them", the "because"
  -- relationship is level 2, and the "like" relationships are level 1
data JointX = JointX String deriving (Show, Eq)
  -- in "you #like peaches #at noon", "like" and "at" are jointXs
instance IsString JointX where fromString = JointX
data EO = EO     -- EO = "expression orderer"
  { open :: Bool -- open = "more expressions can be concatentated into it"
                 -- In b@(RelX (EO x _) _ _), x is true until
                 -- b has been surrounded by parentheses.
  , inLevel :: Level } deriving (Eq)

instance Show EO where
  show (EO x y) = "(EO " ++ show x ++ " " ++ show y ++ ")"
instance Ord EO where -- Open > closed. If those are equal, ## > #, etc.
  EO a b <= EO c d = a <= c && b <= d


-- == Errors
data ErrBase = Legacy -- | for when the String has all the info
             | FoundNo | FoundMany | FoundWrongKind
             | ArityMismatch | ConstructorMistmatch
             | NotRelSpecExpr | NotTplt | NotColl | NotLeaf
             | Invalid -- | (MbrPos 0), for instance, is ill-formed
             | Impossible
  deriving (Show, Eq)

type DwtErr = (ErrBase, ErrOpt, String)
errBase :: Lens' DwtErr ErrBase
errBase = _1
errOpts :: Lens' DwtErr ErrOpt
errOpts = _2
errString :: Lens' DwtErr String
errString = _3

-- | TODO ? Use a list of a sum type, to avoid lots of Nothing
data ErrOpt = ErrOpt { _mNode :: Maybe Node
                       , _mEdge :: Maybe Edge
                       , _mEdgeLab :: Maybe RSLTEdge
                       , _mExpr :: Maybe Expr
                       , _mAddX :: Maybe AddX
                       , _mRelRole :: Maybe RelRole
                       , _mRelSpec :: Maybe RelSpec
                       , _mQNode :: Maybe QNode } deriving (Show, Eq)
-- | adjust it like "noErrOpts L.& mNode L..~ (Just 2)"

makeLenses ''ErrOpt

noErrOpts :: ErrOpt
noErrOpts = ErrOpt n n n n n n n n where n = Nothing

-- | TODO: Convert all the ErrOpt to this
type DwtErrSum = (ErrBase, [ErrOptSum], String)
errBaseSum :: Lens' DwtErrSum ErrBase
errBaseSum = _1
errOptsSum :: Lens' DwtErrSum [ErrOptSum]
errOptsSum = _2
errStringSum :: Lens' DwtErrSum String
errStringSum = _3

data ErrOptSum = ErrNode Node | ErrEdge Edge -- | New error style: sum type
               | ErrExpr Expr | ErrEdgeLab RSLTEdge | ErrRelRole RelRole
               | ErrAddX AddX | ErrRelSpec RelSpec | ErrQNode QNode
