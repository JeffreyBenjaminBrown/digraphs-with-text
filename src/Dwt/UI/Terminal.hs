-- | Terminal in the category theory sense of terminal object:
-- every module in Dwt.UI will be imported into this one.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Dwt.UI.Terminal where

import Dwt.Initial.Types
import Dwt.Show (view)
import Dwt.Hash.Insert (addExpr)
import Dwt.Hash.Parse (expr)
import Dwt.UI.Parse (Command(..), ReadNodes, pCommand)
import Dwt.UI.State
import Dwt.Initial.Util (fr)

import Brick.Widgets.Core ( (<+>), (<=>), hLimit, vLimit, str)
import Brick.Util (on)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Graphics.Vty as V
import qualified Data.Text.Zipper as Z

import Lens.Micro
import Lens.Micro.TH
import Text.Megaparsec (parse)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.Reader (runReader)
import Data.Maybe (fromJust)

-- ==== Controls
appHandleEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appHandleEvent st (T.VtyEvent ev) =
  let focus = F.focusGetCurrent (st^.focusRing)
  in case ev of
  V.EvKey V.KEsc [] -> M.halt st
  V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusPrev
  V.EvKey V.KEnter [V.MMeta] -> case fromJust focus of
    -- MMeta is the only working modifier (Kubuntu, Konsole, GHCI)
    InsertWindow -> let st' = addToRSLT st in M.continue $
      case st ^. commands of ("/all":_) -> updateView st'
                             _ -> st'
    CommandWindow -> M.continue $ changeView st
  _ -> M.continue =<< case fromJust focus of
    InsertWindow -> T.handleEventLensed st insertWindow E.handleEditorEvent ev
    CommandWindow -> T.handleEventLensed st commandWindow E.handleEditorEvent ev
appHandleEvent st _ = M.continue st

appChooseCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appChooseCursor = F.focusRingCursor (^.focusRing)


-- ==== Draw
appDraw :: St -> [T.Widget Name]
appDraw st = [ui] where
  -- v = str $ view g $ st ^. nodesInView
  e1 = F.withFocusRing
         (st^.focusRing)
         (  -- :: Bool -> a -> b
              -- in this case, Bool -> Editor t n -> Widget n
           E.renderEditor
             -- :: ([t] -> Widget n) -> Bool -> Editor t n -> Widget n
           $ str . unlines)
         (st^.insertWindow)
  e2 = F.withFocusRing (st^.focusRing)
       (E.renderEditor $ str . unlines)
       (st^.commandWindow)
  ui = C.center
    $ (str "Add data here " <+> e1)
    <=> str " "
    <=> (str "Issue queries here " <+> e2)
    <=>  str " "
    <=> (vLimit 15 $ str (unlines $ st ^. outputWindow))
    <=> str " "

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr [ (E.editAttr       , V.white `on` V.blue)
                                 , (E.editFocusedAttr, V.black `on` V.yellow)
                                 ]

theApp :: M.App St e Name
theApp = M.App { M.appDraw = appDraw
               , M.appChooseCursor = appChooseCursor
               , M.appHandleEvent = appHandleEvent
               , M.appStartEvent = return
               , M.appAttrMap = const appAttrMap
               }

-- ==== Main
ui :: RSLT -> IO (RSLT, [String])
ui g = do st <- M.defaultMain theApp $ initialState g
          return (st ^. rslt
                 , st ^. commands)
