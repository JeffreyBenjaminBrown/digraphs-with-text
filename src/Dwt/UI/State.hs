{-# LANGUAGE TemplateHaskell #-}
module Dwt.UI.State where

import Dwt.Initial.Types
import Dwt.Show (view)
import Dwt.Hash.Insert (addExpr)
import Dwt.Hash.Parse (expr)
import Dwt.UI.Parse (pCommand, commandToReadNodes)

import Brick.Widgets.Core ( (<+>), (<=>), hLimit, vLimit, str)
import Brick.Util (on)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as Ed
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Graphics.Vty as V
import qualified Data.Either as E
import qualified Data.Text.Zipper as Z

import Lens.Micro
import Lens.Micro.TH
import Text.Megaparsec (parse, ParseError, Token)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.Reader (runReader)
import Data.Void (Void)

data Name = InsertWindow | FakeAltKeyWindow deriving (Ord, Show, Eq)

data St = St { _rslt :: RSLT
             , _commands :: [String]
             , _parseErrors :: [ParseError (Token String) Void]
             , _dwtErrors :: [DwtErr]
             ,_focusRing :: F.FocusRing Name
             , _inputWindow, _fakeAltKeyWindow :: Ed.Editor String Name
             , _outputWindow :: [String]
             }

makeLenses ''St

instance Show St where
  show s = "St {rslt: " ++ show (s^.rslt)
    ++ ", commands: " ++ show (s^.commands)
    ++ ", parseErrors: " ++ show (s^.parseErrors)
    ++ ", dwtErrors: " ++ show (s^.dwtErrors)
    ++ ", outputWindow: " ++ show (s^.outputWindow)
    ++ ", plus a focus ring and two editors}"

initialState :: RSLT -> St
initialState g = refreshView $ St
  { _rslt = g
  , _commands = ["/all"]
  , _parseErrors = []
  , _dwtErrors = []
  , _focusRing = F.focusRing [InsertWindow, FakeAltKeyWindow]
  , _inputWindow = Ed.editor InsertWindow Nothing ""
  , _fakeAltKeyWindow = Ed.editor FakeAltKeyWindow (Just 1) "Visit this window to simulate pressing Alt."
  , _outputWindow = []
  }

-- ==== Change state
-- | if the previous view was /all, refresh view; otherwise don't
addToRSLTAndMaybeRefresh :: St -> St
addToRSLTAndMaybeRefresh st = let st' = addToRSLT st in
  case st ^. commands of ("/all":_) -> refreshView st'
                         _ -> st'

deleteErrors :: St -> St
deleteErrors st = dwtErrors .~ [] $ parseErrors .~ [] $ st

addToRSLT :: St -> St
addToRSLT st = st & f3 . f2 . f1 where
  strings = filter (not . null) $ st ^. inputWindow & Ed.getEditContents
  parseEithers = map (parse expr "") strings
    :: [Either (ParseError (Token String) Void) QNode]
  qnodes = E.rights parseEithers
  graphUpdater = mapM addExpr qnodes
  g = st ^. rslt
  e = execStateT graphUpdater g
  f1 = inputWindow %~ Ed.applyEdit Z.clearZipper
  f2 = case e of Left de -> dwtErrors .~ [de]
                 Right g' -> rslt .~ g'
  f3 = parseErrors .~ E.lefts parseEithers

changeView :: St -> St
changeView st =
  let (commandString:_) = filter (not . null)
                          $ st^.inputWindow & Ed.getEditContents
        -- TODO: take first string without discarding rest
      st' = inputWindow %~ Ed.applyEdit Z.clearZipper
            $ commands %~ (commandString :)
            $ st
  in updateView commandString st

refreshView :: St -> St
refreshView st = updateView (head $ st ^. commands) st
  -- head is safe b/c _commands begins nonempty and never shrinks

updateView :: String -> St -> St
updateView cmdString st = case parse pCommand "" cmdString of
  Left pe -> st & parseErrors .~ [pe]
  Right cmd -> case cmd of CommandShowQueries -> showQueries st
                           r -> viewRSLT (commandToReadNodes r) st

viewRSLT :: ReadNodes -> St -> St
viewRSLT reader st = do
  let x = do ns <- runReader reader $ st^.rslt
             view (st^.rslt) ns :: Either DwtErr [String]
  case x of Left de -> st & dwtErrors .~ [de]
            Right lines -> st & outputWindow .~ reverse lines

showQueries :: St -> St
showQueries st = st & outputWindow .~ st^.commands
