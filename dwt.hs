import qualified Data.Map as M

class Rev a where
  rev :: a -> a

-- Direction in a graph. Unused except by the Reln type.
data Dirn = Pred | Succ
  deriving (Eq, Show)

instance Ord Dirn where -- because Dirn is used as a key in a Map
  (<=) Succ Pred = False
  (<=) _ _ = True

instance Rev Dirn where
  rev Pred = Succ
  rev Succ = Pred

-- Orientable text, to be paired with a Dirn.
data DirnTx =
  Symm { string :: String }
  | Asymm { predTx :: String
          , succTx :: String }
  deriving (Eq, Show)

  {- sample usage
    let z = Asymm {succTx = "forward", predTx = "backward"}
    z
  -}

-- A relationship between graph nodes.
data Reln =
  Reln {
    dirn :: Dirn,
    dirnTx :: DirnTx }
  deriving (Eq, Show)

instance Rev Reln where
  rev r = Reln {
    dirn = rev $ dirn r,
    dirnTx = dirnTx r
  }

  {- sample usage
  let x = Reln {dirn = Succ, dirnTx = Asymm { predTx = "to", succTx = "fro"}}
  rev x
  -}

-- EOF

