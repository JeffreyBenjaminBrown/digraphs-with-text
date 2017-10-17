{-# LANGUAGE TemplateHaskell #-}
module Dwt.UI.State where

import Dwt.Initial.Types
import Dwt.Show (view)
import Dwt.Hash.Insert (addExpr)
import Dwt.Hash.Parse (expr)
import Dwt.UI.Parse (pCommand, commandToReadNodes)
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

data Name = InsertWindow | FakeAltKeyWindow deriving (Ord, Show, Eq)

data St = St { _rslt :: RSLT
             , _commands :: [String]
             ,_focusRing :: F.FocusRing Name
             , _inputWindow, _fakeAltKeyWindow :: E.Editor String Name
             , _outputWindow :: [String]
             }

makeLenses ''St

initialState :: RSLT -> St
initialState g = recalculateView $ St
  { _rslt = g
  , _commands = ["/all"]
  , _focusRing = F.focusRing [InsertWindow, FakeAltKeyWindow]
  , _inputWindow = E.editor InsertWindow Nothing ""
  , _fakeAltKeyWindow = E.editor FakeAltKeyWindow (Just 1) "Visit this window to simulate pressing the Alt key."
  , _outputWindow = []
  }

-- ==== Change state
-- | if the previous view was /all, refresh view; otherwise don't
addToRSLTAndMaybeRefresh :: St -> St
addToRSLTAndMaybeRefresh st = let st' = addToRSLT st in
  case st ^. commands of ("/all":_) -> recalculateView st'
                         _ -> st'

addToRSLT :: St -> St
addToRSLT st = st & f2 . f1 where
  strings = filter (not . null) $ st ^. inputWindow & E.getEditContents
  graphUpdater = mapM (addExpr . fr . parse expr "") strings
    -- TODO: nix the fr
  g = st ^. rslt
  e = execStateT graphUpdater g
  f1 = inputWindow %~ E.applyEdit Z.clearZipper
  f2 = case e of Left _ -> id -- TODO: display the error
                 Right g' -> rslt .~ g'

changeView :: St -> St
changeView st =
  let (commandString:_) = filter (not . null)
                          $ st^.inputWindow & E.getEditContents
        -- TODO: take first string without discarding rest
      st' = inputWindow %~ E.applyEdit Z.clearZipper
            $ commands %~ (commandString :)
            $ st
  in updateView commandString st

recalculateView :: St -> St
recalculateView st = updateView (head $ st ^. commands) st
  -- head is safe b/c _commands begins nonempty and never shrinks

-- | like changeView, but without reading a new query
updateView :: String -> St -> St
updateView cmdString st = let command = fr $ parse pCommand "" cmdString in
                    -- TODO: nix the fr
  case command of CommandShowQueries -> showQueries st
                  r -> viewRSLT (commandToReadNodes r) st

viewRSLT :: ReadNodes -> St -> St
viewRSLT reader st = do
  let nodesToView = runReader reader $ st^.rslt
  case nodesToView of
    Left dwtErr -> error $ "viewRSLT" ++ show dwtErr
    Right ns -> st & outputWindow .~ reverse (fr $ view  (st^.rslt)  ns)

showQueries :: St -> St
showQueries st = st & outputWindow .~ st^.commands
