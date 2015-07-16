module Dwt.Util where

import qualified Data.List as List

insertAfterPos :: Int -> a -> [a] -> [a] -- first arg out of range, 
  -- either direction, adds onto that end.
insertAfterPos n x xs = let (starts,ends) = List.splitAt n xs in starts ++ x:ends
  -- TODO: write auto-tests
    -- is tested, good 
    -- was by hand, but all cases
                        
countHoles :: String -> Int
countHoles = length . Prelude.filter (== '_')

-- eof

