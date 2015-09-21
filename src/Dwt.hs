module Dwt
  ( -- exports:
  module Data.Graph.Inductive -- export for testing, not production
  , module Dwt -- exports everything in this file
    -- could be more selective
  -- , module Dwt.Graph -- etc. Will need to import below to match.
  ) where                   
import Data.Graph.Inductive

myEmpty = empty :: Gr String String
                           
insString s g = insNode (i,s) g
  where i = head $ newNodes 1 g


