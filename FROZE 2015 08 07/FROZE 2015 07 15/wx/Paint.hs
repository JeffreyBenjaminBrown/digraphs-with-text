-- https://github.com/jodonoghue/wxHaskell/tree/master/samples/wx

module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  glob <- varCreate 0
  f  <- frame [text := "Paint demo" 
              , fullRepaintOnResize := False ]
  sw <- scrolledWindow f [on paint := onpaint glob
                         , virtualSize := sz 500 500
                         , scrollRate := sz 10 10
                         , fullRepaintOnResize := False ]
  set f [clientSize := sz 150 150
       , layout := minsize (sz 300 200) $ fill $ widget sw]
  return ()
  where
    onpaint glob dc viewArea = do
      n <- varGet glob
      putStr $ show n 
      circle dc (pt 200 200) 20 [penKind := PenDash DashDot]
=      arc dc (pt 100 100) 20 90 230           
        [color := red, penWidth :~ (+1), penKind := PenSolid]
      ellipticArc dc (rect  (pt 20  20) (sz 60 30)) 90 230 
        [color := blue, penWidth :~ (*2)]
      c <- get dc color
      set dc [fontFace := "Sans Serif", 
             fontSize := 16
             , fontWeight := WeightBold ]
      drawText dc (show c) (pt 50 50) []
      rotatedText dc "rotated text" (pt 80 160) 45 
        [textColor := green]

