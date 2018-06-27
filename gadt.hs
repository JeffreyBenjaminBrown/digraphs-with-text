{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
-- based on these:
-- https://www.reddit.com/r/haskell/comments/8tnily/dependent_types_in_constructors_eg_data_x_y_z_y/e192mpj/
-- https://gist.github.com/Tayacan/d3e63e5cbc42d67e5bbf31450cb0a91b

class AsInt a where
  asInt :: a -> Int

-- = simple types
type Label = String
type Arity = SNat
type Flavor n = (Arity n, Label)

-- = Nat
data Nat = Z | S Nat deriving (Eq, Ord)

instance AsInt Nat where
  asInt Z = 0
  asInt (S n) = 1 + asInt n

instance Show Nat where
  show Z = "Nat 0"
  show (S n) = "Nat " ++ show (1 + asInt n)

-- = SNat
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

instance Show (SNat n) where
  show SZ = "SNat 1"
  show (SS n) = "SNat " ++ show n

one = SZ
two = SS one
three = SS two

-- = Vect
data Vect (n :: Nat) a where
  VNil  :: Vect Z a
  VCons :: a -> Vect n a -> Vect (S n) a

infixr 4 .:
(.:) = VCons

-- = HypergraphNode
data HypergraphNode where
  Atom :: Label -> HypergraphNode
  RelationshipFlavor :: Flavor n -> HypergraphNode
  Relationship :: Flavor n -> Vect (S n) HypergraphNode -> HypergraphNode

foo :: HypergraphNode
foo = Relationship
    (two, "Foo")
    (Atom "a" .: Atom "b" .: VNil)

--anError :: HypergraphNode -- (Good). Arity must match the Vect length.
--anError = Relationship
--    (three, "Foo")
--    (Atom "a" .: Atom "b" .: VNil)
