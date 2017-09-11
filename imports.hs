:module -Dwt.ParsecUtils 

import Text.Megaparsec
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Dwt.ParsecUtils as P

import qualified Text.Read as R
import qualified Data.Text as T

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Brick.Main as B

:set prompt ">>> "
:show imports
