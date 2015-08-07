set prompt "> "

import Control.Lens (makeLenses, view, (^.), (^?), over, (%~), set, (.~) )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import Control.Exception (assert)

import Data.Graph.Inductive        
import Data.Graph.Inductive.Graph  
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.Basic  

