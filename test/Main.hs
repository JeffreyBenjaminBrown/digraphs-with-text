-- setup
    {-# LANGUAGE FlexibleContexts #-}
    module Main where

    import Test.HUnit

    import Dwt
    import TData
    import TParse
    import TGraph
    import TSearch
--    import TShow
--    import TMmParse

-- main
    main = runTestTT $ TestList
      [
--        TestLabel "tParse"   tParse
            TestLabel "tGraph"   tGraph
          , TestLabel "tSearch" tSearch
--        , TestLabel "tView"    tView
--        , TestLabel "tMmParse" tMmParse
      ]
