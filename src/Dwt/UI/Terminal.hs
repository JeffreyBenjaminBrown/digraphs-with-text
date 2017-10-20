-- | Terminal in the category theory sense of terminal object:
-- every module in Dwt.UI will be imported into this one.
{-# LANGUAGE ViewPatterns #-}
module Dwt.UI.Terminal where

import Dwt.Initial.Types
import Dwt.Initial.Util (printDwtErr)
import Dwt.UI.State

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
import Text.Megaparsec (parse, parseErrorPretty)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.Reader (runReader)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

-- ==== Control
appHandleEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appHandleEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appHandleEvent st (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
  M.continue $ st & focusRing %~ F.focusPrev
appHandleEvent st@(fromJust . F.focusGetCurrent . flip (^.) focusRing
                  -> InsertWindow)
               (T.VtyEvent ev) = case ev of
  V.EvKey (V.KChar 'a') [V.MMeta] -> M.continue
    $ addToRSLTAndMaybeRefresh . deleteErrors $ st
  V.EvKey (V.KChar 'v') [V.MMeta] -> M.continue
    $ changeView . deleteErrors $ st
  ev -> M.continue
    =<< T.handleEventLensed st inputWindow E.handleEditorEvent ev
appHandleEvent st@(fromJust . F.focusGetCurrent . flip (^.) focusRing
                  -> FakeAltKeyWindow)
               (T.VtyEvent ev) = case ev of
  V.EvKey (V.KChar 'a') [] -> M.continue
    $ addToRSLTAndMaybeRefresh . deleteErrors $ st
  V.EvKey (V.KChar 'v') [] -> M.continue
    $ changeView . deleteErrors $ st
  ev -> M.continue st
  
appChooseCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appChooseCursor = F.focusRingCursor (^.focusRing)


-- ==== Draw
appDraw :: St -> [T.Widget Name]
appDraw st = [ui] where
  e1 = F.withFocusRing
         (st^.focusRing)
         (  -- :: Bool -> a -> b
              -- in this case, Bool -> Editor t n -> Widget n
           E.renderEditor
             -- :: ([t] -> Widget n) -> Bool -> Editor t n -> Widget n
           $ str . unlines)
         (st^.inputWindow)
  e2 = F.withFocusRing (st^.focusRing)
       (E.renderEditor $ str . unlines)
       (st^.fakeAltKeyWindow)
  maybeAddParseErrors ui = if null $ st ^. parseErrors then ui
    else str (unlines $ map parseErrorPretty $ st ^. parseErrors) <=> ui
  maybeAddDwtErrors ui = if null $ st ^. dwtErrors then ui
    else str (unlines $ map printDwtErr $ st ^. dwtErrors) <=> ui
  ui = C.center 
    $ maybeAddParseErrors . maybeAddDwtErrors
    $ e1 <=> str " "
    <=> e2 <=>  str " "
    <=> (vLimit 15 $ str (unlines $ st ^. outputWindow)) <=> str " "

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr [ (E.editAttr       , V.white `on` V.blue)
                                 , (E.editFocusedAttr, V.black `on` V.yellow)
                                 ]


-- ==== Main
theApp :: M.App St e Name
theApp = M.App { M.appDraw = appDraw
               , M.appChooseCursor = appChooseCursor
               , M.appHandleEvent = appHandleEvent
               , M.appStartEvent = return
               , M.appAttrMap = const appAttrMap
               }

ui :: RSLT -> IO RSLT
ui g = do st <- M.defaultMain theApp $ initialState g
          return $ st ^. rslt

uist :: St -> IO (St)
uist st = M.defaultMain theApp st >>= return
