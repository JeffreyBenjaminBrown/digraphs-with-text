{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Dwt.UI where

import Data.Graph.Inductive (empty, nodes)
import Dwt.Graph (RSLT(..), insWord, insTplt)
import Dwt.Show (view)
import Dwt.Parse

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V
import qualified Data.Text.Zipper as Z

import Data.List (partition)

data Name = Edit1
          | Edit2
          deriving (Ord, Show, Eq)

data St =
    St { _rslt :: RSLT
       ,_focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _edit2 :: E.Editor String Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui] where
  g = st ^. rslt
  v = str $ view g $ nodes g
  e1 = F.withFocusRing
         (st^.focusRing)
         (  -- :: Bool -> a -> b
              -- in this case, Bool -> Editor t n -> Widget n
           E.renderEditor
             -- :: ([t] -> Widget n) -> Bool -> Editor t n -> Widget n
           $ str . unlines)
         (st^.edit1)
  e2 = F.withFocusRing (st^.focusRing)
       (E.renderEditor $ str . unlines)
       (st^.edit2)
  ui = C.center $
       (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 e1)) <=>
       str " " <=>
       (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 e2)) <=>
       str " " <=>
       (str "The RSLT: " <+> v) <=>
       str " " <=>
       str "Press Tab to switch between editors, Esc to quit."

appHandleEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appHandleEvent st (T.VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> M.halt st
  V.EvKey V.KIns [] -> addToRSLT st
  V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
  V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
  _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
    Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
    Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
    Nothing -> return st
appHandleEvent st _ = M.continue st

addToRSLT :: St -> T.EventM Name (T.Next St)
addToRSLT st = do
    let strings = st ^. edit1 & E.getEditContents
        (templates,words) =
          partition (either (const False) id . hasBlanks) strings
          -- (const False) should work if it happens but shouldn't happen
        f1 = edit1 %~ E.applyEdit Z.clearZipper
        f2 = rslt %~ (\g -> foldl (flip insWord) g $ words)
        f3 = rslt %~ (\g -> foldl (flip insTplt) g $ templates)
    M.continue $ st & f3 . f2 . f1

initialState :: RSLT -> St
initialState g = St g
                    (F.focusRing [Edit1, Edit2])
                    (E.editor Edit1 Nothing "")
                    (E.editor Edit2 (Just 2) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appHandleEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

ui :: RSLT -> IO (RSLT)
ui g = do
    st <- M.defaultMain theApp $ initialState g
    return $ st ^. rslt
