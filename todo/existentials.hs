-- Francesco Ariis gave me the idea, 
  -- in the thread: Manual type-checking in graphs: Avoidable?

    {-# LANGUAGE ExistentialQuantification #-}
    {-#LANGUAGE GADTs #-}
--    {-#LANGUAGE DatatypeContexts #-}

    import Data.Graph.Inductive
    import Data.Maybe as Maybe

-- Matt Parsons solves the problem described in the next section

    data Some f where
       Some :: f a -> Some f
    
    type G = Gr (Some Box') String
    
    -- Ordinarily you lose the information about the `a`, but when you have
    -- a GADT, that allows you to recover type information. So you can match on:
    
    f :: Some Box' -> String
    f (Some (Bi i)) = show (i + 1)
    f (Some (Bs s)) = s
    
-- problem: Gr :: * -> * -> *, not * -> (* -> *) -> *
    data Box' a where
      Bi :: Int -> Box' Int
      Bs :: String -> Box' String
    instance Show (Box' a) where
      show (Bi i) = "Box " ++ show i
      show (Bs s) = "Box " ++ show s

    -- type G = Gr Box' String -- will not compile
    -- type G = Gr (Box' Int) String -- compiles but not polymorphic

-- this lets you build a graph, but you can't work with its items
    data Box = forall s. Show s => Box s
    type ExQuantGraph = Gr Box String
    instance Show Box where show (Box x) = "Box: " ++ show x
    g = insNode (0, Box 1) 
        $ insNode (1, Box 'a') 
        $ empty :: ExQuantGraph

    getBox :: ExQuantGraph -> Node -> Box
    getBox g n = Maybe.fromJust $ lab g n

  -- this won't compile (which is reasonable)
    --    getInt :: Box -> Int
    --    getInt (Box i) = i
  
    --    getInt :: ExQuantGraph -> Node -> Int
    --    getInt g n = i
    --      where (Box i) = Maybe.fromJust $ lab g n
