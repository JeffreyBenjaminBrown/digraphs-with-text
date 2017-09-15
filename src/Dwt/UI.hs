{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Dwt.UI where

import Data.Graph.Inductive (empty, nodes, Node)
import Dwt.Types
import Dwt.Show (view)
import Dwt.Hash.Insert (addExpr)
import Dwt.Hash.Parse
import Dwt.Search.Parse
import Dwt.Util (fr)
import Text.Megaparsec (parse, (<|>))
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.Reader (runReader)

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
       , _nodesInView :: [Node]
       ,_focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _edit2 :: E.Editor String Name
       }

makeLenses ''St

appDraw :: St -> [T.Widget Name]
appDraw st = [ui] where
  g = st ^. rslt
  v = str $ view g $ st ^. nodesInView
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
  ui = C.center
    $ (str "Input 1: " <+> e1)
    <=> str " "
    <=> (str "Input 2: " <+> e2)
    <=>  str " "
    <=> (str "The RSLT: " <+> v)
    <=> str " "
    <=> str "Press Tab to switch between editors, Esc to quit."

appHandleEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appHandleEvent st (T.VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> M.halt st
  V.EvKey V.KIns [] -> addToRSLT st
  V.EvKey V.KHome [] -> viewRSLT st
  V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
  V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
  _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
    Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
    Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
    Nothing -> return st
appHandleEvent st _ = M.continue st

viewRSLT :: St -> T.EventM Name (T.Next St)
viewRSLT st = do
    let (request:_) = st ^. edit2 & E.getEditContents
          -- TODO: find a more elegant way to take only one string
        theReader = fr $ parse (pUsers <|> pAll) "" request -- TODO: nix fr
        newNodesInView = runReader theReader $ st ^. rslt
        f1 = nodesInView .~ newNodesInView
        f2 = edit2 %~ E.applyEdit Z.clearZipper
    M.continue $ st & f2 . f1

addToRSLT :: St -> T.EventM Name (T.Next St)
addToRSLT st = do
    let strings = st ^. edit1 & E.getEditContents
        graphUpdater = mapM (addExpr . fr . parse expr "" ) strings
          -- TODO: nix the fr
        g = st ^. rslt
        e = execStateT graphUpdater g
    let f1 = edit1 %~ E.applyEdit Z.clearZipper
        f2 = case e of Left _ -> id -- TODO: display the error
                       Right g' -> rslt .~ g'
    M.continue $ st & f2 . f1

initialState :: RSLT -> St
initialState g = St g (nodes g) (F.focusRing [Edit1, Edit2])
  (E.editor Edit1 Nothing "") (E.editor Edit2 (Just 2) "")

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appChooseCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appChooseCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = appDraw
          , M.appChooseCursor = appChooseCursor
          , M.appHandleEvent = appHandleEvent
          , M.appStartEvent = return
          , M.appAttrMap = const appAttrMap
          }

ui :: RSLT -> IO (RSLT)
ui g = do
    st <- M.defaultMain theApp $ initialState g
    return $ st ^. rslt
