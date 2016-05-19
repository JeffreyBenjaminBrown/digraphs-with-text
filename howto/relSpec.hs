I saw this template:
  ":15 _ #kind/ _"
So I made a direction:
  *Main M> let r = M.fromList [(RelTplt,NodeSpec 15),
    (Mbr 1,VarSpec Up),
    (Mbr 2,VarSpec Down)]
And added it:
  g <- p $ fr $ (insRelSpec r g :: Either String SOLRT)
