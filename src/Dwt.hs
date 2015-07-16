--init
    {-# LANGUAGE TemplateHaskell #-}
    module Dwt where
    import qualified Dwt.Util as Util
    import Control.Lens (makeLenses
                        , view, (^.), (^?)
                        , over, (%~)
                        , set, (.~) )
    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import qualified Data.Maybe as Maybe
    import Control.Exception (assert)
--types
    data StmtIdx = StmtIdx Int deriving (Show, Eq, Ord)
    data RelIdx  = RelIdx  Int deriving (Show, Eq, Ord)
    data NodeIdx = StmtNodeIdx StmtIdx | RelNodeIdx  RelIdx 
                 -- | NullNodeIdx -- might use for half-filled-in Rels
                 deriving (Show, Eq, Ord)
    data Stmt = Stmt String deriving (Show, Eq, Ord)
    data Rel = Rel { _tplt :: StmtIdx   , _mbrs :: [NodeIdx] }
              deriving (Show, Eq, Ord)
    data Node = StmtNode { _stmt    :: Stmt , _relIdxs :: [RelIdx] }
              | RelNode  { _rel     :: Rel  , _relIdxs :: [RelIdx] }
                deriving (Show, Eq, Ord)
    data Graph = Graph { _nodeMap :: Map.Map NodeIdx Node            
                       , _maxStmtIdx :: Int, _maxRelIdx  :: Int } 
                          deriving (Show, Eq)
    --PITFALL: _stmt and _rel not defined for the other constructor,
      --which means stmt and rel lenses can fail. If I try to use Lens.(^.), I get an error stating that I can't unless maybe I add a Monoid instance to handle failure. Instead of that, I am using (^?), "reusing the Maybe monoid", per the suggestion here: http://stackoverflow.com/questions/17518301/indexing-list-with-control-lens-requires-monoid-constraint
  --makeLenses
    makeLenses ''Rel
    makeLenses ''Node
    makeLenses ''Graph
--test
  --test Rel ~ NodeIdx, or test Rel in Graph
    isMbrOfRel :: Rel -> NodeIdx -> Bool
    isMbrOfRel r ni = (elem ni $ r ^. mbrs)
    isTpltOfRel :: Rel -> NodeIdx -> Bool
    isTpltOfRel r ni = case ni of
      RelNodeIdx _ -> error "RelNodeIdx cannot be a Rel's tplt."
      StmtNodeIdx si -> view tplt r == si  
    relUsesNodeIdx :: Rel -> NodeIdx -> Bool
    relUsesNodeIdx r ni = isMbrOfRel r ni || isTpltOfRel r ni
    graphSupportsRelIdxInNode :: Graph -> NodeIdx -> RelIdx -> Bool
    graphSupportsRelIdxInNode g ni ri = --Is Node n of ni in Rel r of ri?
      let rn = nodeIdxToNode g $ RelNodeIdx ri
      in case rn of
        RelNode _ _ -> relUsesNodeIdx (relFromRelNode rn) ni 
        StmtNode _ _ -> error "Lookup up a RelIdx, got a StmtNode."
    graphSupportsRel :: Graph -> Rel -> Bool --DO test
    graphSupportsRel g r = --true if g's keys > tplt and mbrs of r
      Set.intersection involved keys == Set.empty
      where involved = Set.insert (StmtNodeIdx $ view tplt r)
                       $ Set.fromList $ view mbrs r
            keys = Set.fromList $ Map.keys $ view nodeMap g
  --test Node (in Graph)
    hasValidRelIdxs :: Graph -> NodeIdx -> Bool
    hasValidRelIdxs g ni =
      and $ map (graphSupportsRelIdxInNode g ni) 
          $ view relIdxs $ nodeIdxToNode g ni
--lookups
    nodeIdxToNode :: Graph -> NodeIdx -> Node
    nodeIdxToNode g ni = let nMb = Map.lookup ni $ view nodeMap g
                             n = Maybe.fromJust nMb
                         in if (Maybe.isJust nMb) 
                            then n 
                            else error "NodeIdx not a key in Graph"
    relFromRelNode :: Node -> Rel
    relFromRelNode rn = Maybe.fromJust $ rn ^? rel
    nodeIdxsRelRefersTo :: Rel -> [NodeIdx]
    nodeIdxsRelRefersTo r = --DO test 
      (StmtNodeIdx $ view tplt r) : view mbrs r
--build
  --Node
    addRelIdxToNode :: RelIdx -> Node -> Node
    addRelIdxToNode ri n = set relIdxs (ri : view relIdxs n) n
  --Graph
    emptyGraph :: Graph
    emptyGraph = Graph { _nodeMap = Map.empty
                       , _maxStmtIdx = 0
                       , _maxRelIdx  = 0 }
    addStmtAtIdxToGraph :: Stmt -> StmtIdx -> Graph -> Graph
    addStmtAtIdxToGraph s (StmtIdx i) g = 
      let nm = view nodeMap g
          si = StmtIdx i
          sni = StmtNodeIdx si
          sn = StmtNode s []
      in case (elem sni $ Map.keys nm) of
        True -> error "Cannot add Stmt at occupied StmtIdx"
        False -> let newMax = max i $ view maxStmtIdx g 
                   in (set maxStmtIdx newMax)
                      . (set nodeMap $ Map.insert sni sn $ nm) $ g
    addStmtToGraph :: Stmt -> Graph -> Graph
    addStmtToGraph s g = addStmtAtIdxToGraph s nextIdx g 
      where nextIdx = StmtIdx $ (+1) $ view maxStmtIdx g
    addRelAtIdxToGraph :: Rel -> RelIdx -> Graph -> Graph 
    addRelAtIdxToGraph r (RelIdx i) g =
      let nm = view nodeMap g -- nm, ri, rni, rn
          ri = RelIdx i
          rni = RelNodeIdx ri
          rn = RelNode r []
      in case (elem rni $ Map.keys nm) of
        True -> error "Cannot add Rel at occupied RelIdx"
        False -> let newMax = max i $ view maxRelIdx g 
                 in _forNewRelAtIdxUpdateGraph r ri -- informs the others
                    . (set maxRelIdx newMax)
                    . (set nodeMap $ Map.insert rni rn $ nm) $ g
    addRelToGraph :: Rel -> Graph -> Graph
    addRelToGraph r g = addRelAtIdxToGraph r nextIdx g 
      where nextIdx = RelIdx $ (+1) $ view maxRelIdx g
    _forNewRelAtIdxUpdateGraph :: Rel -> RelIdx -> Graph -> Graph
    _forNewRelAtIdxUpdateGraph r ri g =
      let nis = nodeIdxsRelRefersTo r
          f = \g ni -> let n' = addRelIdxToNode ri $ nodeIdxToNode g ni
                       in replaceNodeAtIdxInGraph n' ni g
      in foldl f g nis
    _forGoneRelIdxUpdateNode :: RelIdx -> Node -> Node -- relIdxs lose ri
    _forGoneRelIdxUpdateNode ri n = set relIdxs ris' n
      where ris' = filter (/= ri) $ view relIdxs n
    _forGoneRelUpdateGraph :: Rel -> RelIdx -> Graph -> Graph
    _forGoneRelUpdateGraph r ri g = 
      let nis = nodeIdxsRelRefersTo r
          f = \g ni -> let n' = _forGoneRelIdxUpdateNode ri
                              $ nodeIdxToNode g ni
                       in replaceNodeAtIdxInGraph n' ni g
      in foldl f g nis
-- not ?yet necessary
  -- ? delete a Node: what happens to others?
    -- _forGoneNodeIdxUpdateRelTplt :: NodeIdx -> Rel -> Rel
    -- _forGoneNodeIdxUpdateRelTplt ni r =
      -- let replaceIfNi = \x -> case x of ni -> NullNodeIdx
      --                                   _ -> x
      -- in case ni of StmtNodeIdx si -> over tplt replaceIfNi r
      --               RelNodeIdx _ -> r --a Rel is never another's tplt
    -- _forGoneNodeIdxUpdateGraph :: NodeIdx ->Graph -> Graph -- DOING
    -- _forGoneNodeIdxUpdateGraph ni g =
      -- English: for ri in relIdxs n~ni, change r~ri by ni -> NullNode
      -- let nullThatIdx = \ni' -> case ni' of ni -> NullNodeIdx
      --                                       _ -> ni'
      --     processNode = \n -> let ris' = view relIdxs n 
      --       (set relIdxs 
      -- in map ???
  -- ugly: add|replace arbitrary Node to Graph
    replaceNodeAtIdxInGraph :: Node -> NodeIdx -> Graph -> Graph
    replaceNodeAtIdxInGraph n ni g = --UNSAFE
      -- does not update Node/relIdxs or Graph/maxRelIdx
      let nm' = Map.insert ni n $ view nodeMap g
      in (set nodeMap nm') g
    addToGraph :: Node -> Graph -> Graph --DO test
    addToGraph n g = case n of
      --tried, could not avoid duplicating code
        --see benched.hs/addToGraph' for the try
      StmtNode _ _ -> let nextId = (+1) $ view maxStmtIdx g
                          m = view nodeMap g
        in (set maxStmtIdx nextId)
           . (set nodeMap
             $ Map.insert (StmtNodeIdx $ StmtIdx nextId
                          ) n m )
           $ g
      RelNode _ _ -> let nextId = (+1) $ view maxRelIdx g
                         m = view nodeMap g
        in (set maxRelIdx nextId)
           . (set nodeMap
             $ Map.insert (RelNodeIdx $ RelIdx nextId
                          ) n m )
           $ g
    --addToGraph' :: ? -> Node -> Graph -> Graph --MORE ELEG:HARD
      --Keep this to remember why improving addToGraph is hard.
        --these are problematic
          --because these are not the same type sig
            -- *Dwt> :t StmtNodeIdx
            -- StmtNodeIdx :: StmtIdx -> NodeIdx
            -- *Dwt> :t RelNodeIdx
            -- RelNodeIdx :: RelIdx -> NodeIdx
          --nor these
            -- *Dwt> :t StmtIdx
            -- StmtIdx :: Int -> StmtIdx
            -- *Dwt> :t RelIdx
            -- RelIdx :: Int -> RelIdx
        --whereas these are okay
          -- *Dwt> :t maxStmtIdx    
          -- maxStmtIdx :: Functor f => (Int -> f Int) -> Graph -> f Graph
          -- *Dwt> :t maxRelIdx
          -- maxRelIdx :: Functor f => (Int -> f Int) -> Graph -> f Graph
          -- *Dwt> :t RelNodeIdx
          -- RelNodeIdx :: RelIdx -> NodeIdx
      --addToGraph' nodeCstrctr relOrStmtIdxCstrctr maxIdxFunc n g = 
        -- let nextId = (+1) $ view maxIdxFunc g
        --     m = view nodeMap g
        -- in (set maxIdxFunc nextId)
        --   . (set nodeMap
        --     $ Map.insert (nodeCstrctr $ relOrStmtIdxCstrctr nextId
        --                  ) n m )
        --   $ g
    addToGraphAtIdx :: Node -> NodeIdx -> Graph -> Graph
    addToGraphAtIdx n ni g = let nm = view nodeMap g
      in case n of
      StmtNode _ _ -> case ni of
        RelNodeIdx _-> error "Cannot add StmtNode at RelNodeIdx."
        StmtNodeIdx (StmtIdx i) -> case (elem ni $ Map.keys nm) of
          True -> error "Cannot add StmtNode at occupied StmtIdx."
          False -> let newMax = max i $ view maxStmtIdx g 
                   in (set maxStmtIdx newMax)
                      . (set nodeMap $ Map.insert ni n $ nm )
                      $ g
      RelNode _ _ -> case ni of
        StmtNodeIdx _ -> error "Cannot insert RelNode at StmtNodeIdx."
        RelNodeIdx (RelIdx i) -> case (elem ni $ Map.keys nm) of
          True -> error "Cannot add RelNode at occupied StmtIdx."
          False -> let newMax = max i $ view maxRelIdx g 
                   in (set maxRelIdx newMax)
                      . (set nodeMap $ Map.insert ni n $ nm )
  -- Graph keyset
    --DO merge :: Graph -> Graph -> Graph
      --add the max of one's indices to all the other's for both types
    --DO makeDense :: Graph -> Graph 
      --in output, indices run from 0 to n, adjacent
--eof
                        
