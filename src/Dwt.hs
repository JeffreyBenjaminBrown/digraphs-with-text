module Dwt
    ( -- these are what get exported
    someFunc
    -- , module Dwt -- this is accepted, but does it make sense?
                    -- is it to export everything in this file?
    -- , module Dwt.Graph
    ) where
-- import Dwt.Graph

someFunc :: IO ()
someFunc = putStrLn "someFunc"

