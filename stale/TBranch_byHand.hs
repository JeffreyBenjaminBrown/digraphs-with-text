Right s@(QBranch m a) = Mp.parse pBranch "" "@branch (t _ is _, 1 @from, 2 @to) a"

Right s2@(QBranch m2 a2) = Mp.parse pBranch "" "@branch (t _ is _, 1 @to, 2 @from) a"

g = mkGraph [(0,Word "a"),(1,Word "b"),(2,Tplt ["","is",""]),(3,Rel),(4,Word "c"),(5,Rel),(6,Word "d"),(7,Rel)] [(3,0,RelEdge (Mbr 1)),(3,1,RelEdge (Mbr 2)),(3,2,RelEdge TpltRole),(5,0,RelEdge (Mbr 1)),(5,2,RelEdge TpltRole),(5,4,RelEdge (Mbr 2)),(7,2,RelEdge TpltRole),(7,4,RelEdge (Mbr 1)),(7,6,RelEdge (Mbr 2))] :: RSLT

Right (dogWaterChoco,g2) = flip runStateT g1 ( qPutSt $ QRel ["needs","for"] [ QLeaf $ Word "Laura", QLeaf $ Word "water", QLeaf $ Word "chocolate"] )
