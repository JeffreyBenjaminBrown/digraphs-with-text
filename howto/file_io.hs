*Main> :l test/Spec.hs
[1 of 2] Compiling Dwt              ( /home/jeff/work/computer/dwt/git_hask/src/Dwt.hs, interpreted )
[2 of 2] Compiling Main             ( test/Spec.hs, interpreted )
Ok, modules loaded: Dwt, Main.
*Main> writeFile "temp/g1.txt" (show g1)
*Main> x <- readFile "temp/g1.txt"
*Main> let xg = read x :: Mindmap
*Main> xg
mkGraph [(0,MmString "dog"),(1,MmString "_ wants _"),(2,MmString "_ needs _"),(3,MmString "water"),(4,MmString "brandy"),(5,RelExpr 2),(6,RelExpr 2),(7,MmString "_ needs _ for _"),(8,RelExpr 3),(9,MmString "statement _ is _"),(10,MmString "dubious"),(11,RelExpr 2)] [(5,0,RelPos 1),(5,1,RelTplt),(5,4,RelPos 2),(6,0,RelPos 1),(6,2,RelTplt),(6,3,RelPos 2),(8,0,RelPos 1),(8,3,RelPos 2),(8,4,RelPos 3),(8,7,RelTplt),(11,5,RelPos 1),(11,9,RelTplt),(11,10,RelPos 2)]
*Main> 
