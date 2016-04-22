    module Dwt.Explore (
      module Dwt.Explore
    ) where

    import Data.Graph.Inductive
    import Dwt.Graph
    import Dwt.Show

    import System.IO ( BufferMode(NoBuffering)
                     , hSetBuffering, hSetEcho
                     , hGetBuffering, hGetEcho
                     , stdin, stdout, getChar
                     )
    
    silently :: IO a -> IO a -- act on but don't echo to screen user input
    silently f = do -- CREDIT to Gary Fixler: http://github.com/gfixler/continou
        inB <- hGetBuffering stdin
        outB <- hGetBuffering stdout
        inE <- hGetEcho stdin
        outE <- hGetEcho stdout
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdout True -- HACK: required for clean exit
        hSetEcho stdin False
        hSetEcho stdout False
        r <- f
        hSetBuffering stdin inB
        hSetBuffering stdout outB
        hSetEcho stdin inE
        hSetEcho stdout outE
        return r

    -- CREDIT to Gary Fixler: http://github.com/gfixler/continou
    -- silently $ trap (== 'q') "Press q to exit this trap."
    trap :: (Char -> Bool) -> String -> IO Char
    trap p s = putStrLn s >> f
        where f = do c' <- getChar
                     if p c' then return c' else f

-- loop
    loop :: Mindmap -> IO Int
    loop g = do
      putStrLn "type an integer"
      line <- getLine
      let num = read line :: Node
      putStr "-- it --"
      v g [num]
      putStrLn "-- its users (predecessors) --"
      v g $ pre g num
      loop g

    -- seemed handy, but maybe not
    --    import Dwt.Util (fromRight)
    --    import Dwt.Parse (eParse)
    --    import Text.Parsec
    --    import Text.Parsec.Char (digit)
