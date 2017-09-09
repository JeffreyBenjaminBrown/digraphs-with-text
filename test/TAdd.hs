module TAdd where

import Dwt hiding (fromRight)
import Data.Graph.Inductive
import Test.HUnit hiding (Node)
import Text.Megaparsec (parse)
import Control.Monad.Trans.State (runStateT)

tAdd = TestList [ TestLabel "tExecAddSt" tExecAddSt ]

tExecAddSt = TestCase $ do
  let g = snd . fr $ runStateT (addExprs $ fr $ parse expr "" "a #is b") empty
  assertBool "1" $ g == mkGraph [ (0,Word "a")
                                , (1,Word "b")
                                , (2,Tplt ["","is",""])
                                , (3,Rel)
                                ]
                                [ (3,0,RelEdge (Mbr 1))
                                , (3,1,RelEdge (Mbr 2))
                                , (3,2,RelEdge TpltRole)
                                ]

