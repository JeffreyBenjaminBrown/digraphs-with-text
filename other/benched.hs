-- lib
  -- tried to write addToGraph with no dup
    addToGraph' n g =
      let f = \maxIdxFunc nodeIdxConstructor idxConstructor ->
                let nextId = (+1) $ view maxIdxFunc g    
                    m = view nodeMap g
                in (set maxIdxFunc nextId)          
                   . (set nodeMap $ Map.insert (nodeIdxConstructor 
                                               $ idxConstructor nextId
                                               ) n m )
                   $ g
      in case n of StmtNode _ _ -> f (maxStmtIdx, StmtNodeIdx, StmtIdx)
                   RelNode  _ _ -> f (maxRelIdx,  RelNodeIdx,  RelIdx)

  -- maps from Ord to something
     nextKey :: (Ord k) => Map.Map k a -> k
     nextKey g = if (Map.null g)
                   then toNodeIdx 0
                   else toNodeIdx (1 + i)
                     where i = fromNodeIdx $ fst $ Map.findMax g
     
     append :: NodeIdx k => a -> Map.Map k a -> Map.Map k a
     append a m = Map.insert (nextKey m) a m
    
     newRel sm si sis =
       let s = Map.lookup si sm
           s' = if Maybe.isJust s
             then Maybe.fromJust s 
             else error "newRel: key not found"
       in assert ( elem (countHoles (_htx s') ) [1, length sis] )
         -- Template should have n holes, where the [StmtIdx] is length n,
         -- or else should have only one _.
         -- re. assert idiom, see "smart constructors", Haskell wiki.
         $ Rel { _template = si, _members = sis }
    
  -- testing a NodeIdx's type
    isRelIdx :: Graph -> NodeIdx -> Bool
    isRelIdx g ni = 
      let nMaybe = Map.lookup ni g
          n = if Maybe.isJust nMaybe
                  then Maybe.fromJust nMaybe
                  else error "NodeIdx is not a key in Graph."
      in case n of StmtNode _ _ -> True
                   RelNode _ _ -> False
    isStmtIdx :: Graph -> NodeIdx -> Bool
    isStmtIdx g ni = not $ isRelIdx g ni

-- tests
  tGraphTypes = TestCase $ do -- NodeIdx vals here are gibberish
    let ni = RelIdx 3
        s = Stmt "hi"
        r = Rel {_template = ni, _members = map RelIdx [1,2,3] }
        rn = RelNode {_nodeRel = r, _rels = map RelIdx [3,4,5] }
        sn = StmtNode {_nodeStmt = s, _rels = map RelIdx [11,12] }
    assertBool "true" True
  
  tAppend = TestCase $ do -- TODO: reinstate
    let m = Map.fromList [ (StmtIdx 1,"a"), (StmtIdx 22,"b") ]
    assertBool "append x to Map Int x" ( append "c" m 
      == Map.fromList [ (StmtIdx 1,"a")
                      , (StmtIdx 22,"b")
                      , (StmtIdx 23,"c") ] )
  
  tMaxKey = TestCase $ do -- TODO: reinstate
    let sm = Map.fromList [ (StmtIdx 1, newStmt "_ is for _ when _") ]
        rm = Map.fromList [ ( StmtIdx 11
                              , newRel sm (StmtIdx 1) 
                              $ map StmtIdx [5,6,7,8] ) ]
          -- the two 1s correspond; the other numbers are random
    assertBool "nextKey" (nextKey rm == StmtIdx 12) -- bc 12 comes after 11

  -- newRel: does the assert fail when it should?
    -- yes:
      -- *Dwt> let sm = Map.fromList [ (StmtIdx 1, newStmt "_ is for _ when _") ]
      -- *Dwt> newRel sm (StmtIdx 1) $ map StmtIdx [1,2,3]
      -- Rel {_template = StmtIdx 1, _members = [StmtIdx 1,StmtIdx 2,StmtIdx 3]}
      -- *Dwt> newRel sm (StmtIdx 1) $ map StmtIdx [1,2]
      -- *** Exception: lib/Dwt.hs:69:10-15: Assertion failed
      -- *Dwt>
    -- but how to auto-test?

