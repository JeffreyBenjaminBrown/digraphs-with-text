    module Dwt.Explore (
      module Dwt.Explore
    ) where

    import Dwt.Graph

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

--    loop :: Mindmap -> IO ()
--    loop g = do
--      putStrLn "type a number"
--      line <- getLine
--      let coord = maybeRead line

    -- CREDIT to Gary Fixler: http://github.com/gfixler/continou
    -- silently $ trap (== 'q') "Press q to exit this trap."
    trap :: (Char -> Bool) -> String -> IO Char
    trap p s = putStrLn s >> f
        where f = do c' <- getChar
                     if p c' then return c' else f
