{-# LANGUAGE TemplateHaskell #-}

module Dwt.Initial.Types (
  MbrPos, Arity, Level
  , RSLT, Expr(..), RSLTEdge(..), RelRole(..), CollRole(..)
  , PathInExpr
  , QNode(..), Joint(..), SearchVar(..), RoleMap
  , ReadNodes, Command(..)
  , DwtErr, ErrBase(..), ErrOpt(..)
  , errBase, errOpts, errString -- lenses

  -- | deprecated
  , NodeOrVar(..), QNodeOrVar(..)
  , Relspec, QRelspec
  , RelVarSpec, RelNodeSpec
  ) where

import Data.Graph.Inductive (Gr, Node, Edge)
import Data.Map as Map
import Control.Lens hiding (Level)
import Control.Monad.Trans.Reader
import Data.String (IsString, fromString)


-- == Fundamental
type Arity = Int
type MbrPos = Int -- k members of k-ary Rel, MbrPos(ition) values [1..k]
type Level = Int -- ^ in "cats like you because you like them", the "because"
  -- relationship is level 2, and the "like" relationships are level 1

type RSLT = Gr Expr RSLTEdge -- ^ reflective set of labeled tuples
data Expr = Word String | Fl Float -- ^ Fl and Word are similarly atomic
  | Rel | Tplt [String]
  | Coll -- ^ each uses a CollPrinciple like "and" or "or"
  | RelspecExpr RelVarSpec deriving(Show,Read,Eq,Ord)
    -- ^ The RelVarSpec specifies the variable members.
    -- Edges specify the concrete (addressed) members.
data RSLTEdge = RelEdge RelRole | CollEdge CollRole deriving(Show,Read,Eq,Ord)
  -- | only Rels and Colls emit edges, have subexpressions
  -- "RSLTEdgeLabel" would be a more accurate name.
data RelRole = TpltRole | Mbr MbrPos deriving(Show,Read,Eq,Ord)
  -- ^ a k-ary Rel emits one TpltRole and k RelMbrs
data CollRole = CollPrinciple | CollMbr deriving(Show,Read,Eq,Ord)
  -- ^ a Col emits one CollPrinciple, any number of CollMbrs

type PathInExpr = [RelRole]

-- == Hash: writing to and querying the RSLT
-- TODO: A CollPrinciple currently can point to anything. It would be
  -- cleaner, and closer to truth, to point only to transitive Tplts.
  -- Exceptions: "some of," "no more than," "exactly" would use unary Tplts.
    -- As in "some of {Ghandi, Einstein, Peter Pan} existed".
data SearchVar = It | Any | From | To deriving (Show,Read,Eq,Ord)
data QNode = Absent 
  | At Node -- ^ for when you know the expression's node
  | QLeaf Expr
  | QAnd [QNode] | QOr [QNode] | QDiff QNode QNode
  -- | QMap RoleMap -- ? todo, for leaving Tplt or Mbrs unspecified
  | QBranch RoleMap QNode
  | QRel {qRelTop :: Bool
         , qRelJoints :: [Joint]
         , qRelQNodes :: [QNode] }
  | QVar SearchVar -- ^ these cannot be queried for
                   -- they are for use only within other QNodes
  deriving (Show, Eq)
type RoleMap = Map.Map RelRole QNode

-- | Every rel has at least one joint, and potentially members on either side
  -- If there are more, the list of pairs stores them.
data Joint = Joint String deriving (Show, Eq)
  -- ^ in "you #like peaches #at noon", "like" and "at" are joints
instance IsString Joint where fromString = Joint

type ReadNodes = Reader RSLT (Either DwtErr [Node])

data Command = CommandQNode QNode
               | CommandUsers Node
               | CommandAllNodes
               | CommandShowQueries deriving (Show, Eq)

-- == Errors
type DwtErr = (ErrBase, [ErrOpt], String)

data ErrBase = Legacy -- ^ for when the errString has all the info
  | FoundNo | FoundMany | FoundWrongKind
  | ArityMismatch | ConstructorMistmatch
  | NothingSpecified -- ^ for a search consisting entirely of variables
  | NotRelspecExpr | NotTplt | NotColl | NotLeaf
  | Invalid -- ^ (MbrPos 0), for instance, is ill-formed
  | Impossible deriving (Show, Eq)

data ErrOpt = ErrNode Node | ErrEdge Edge | ErrExpr Expr
  | ErrEdgeLab RSLTEdge | ErrRelRole RelRole | ErrSearchVar SearchVar
  | ErrQRelspec QRelspec | ErrRelspec Relspec | ErrRoleMap RoleMap
  | ErrPathInExpr PathInExpr
  | ErrQNode QNode
  | ErrParse String
  | ErrCommand Command deriving (Show, Eq)

errBase :: Lens' DwtErr ErrBase
errBase = _1
errOpts :: Lens' DwtErr [ErrOpt]
errOpts = _2
errString :: Lens' DwtErr String
errString = _3

-- == Deprecated
data NodeOrVar  = NodeSpec Node | VarSpec SearchVar deriving (Show,Read,Eq)
data QNodeOrVar = QNodeSpec QNode | QVarSpec SearchVar deriving (Show,Eq)
type Relspec  = Map.Map RelRole NodeOrVar
type QRelspec = Map.Map RelRole QNodeOrVar -- ^ if well-formed, includes
  -- one Tplt t, one MbrPos k for all k in [1, arity t], and nothing else,
  -- and at the TpltRole key is a QNodeSpec, not a QVarSpec
type RelVarSpec  = Map.Map RelRole SearchVar
type RelNodeSpec = Map.Map RelRole Node
  -- ^ not quite the set-complement of RelVarSpec
