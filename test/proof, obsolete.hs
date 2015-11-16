-- proving that multi-edges work (try evaluating "lsuc (test) 1")
test = 
  insStringEdge "another conn" 1 0
  $ insStringEdge "one conn" 1 0
  $ insString "second" $ insString "first" $ myEmpty
