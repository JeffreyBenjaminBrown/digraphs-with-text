*Main> g
mkGraph [(0,MmString "dog")] []
*Main> g <- pure $ insStr "cat" g
*Main> g
mkGraph [(0,MmString "dog"),(1,MmString "cat")] []
*Main> 
