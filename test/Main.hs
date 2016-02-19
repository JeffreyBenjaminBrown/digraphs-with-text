-- setup
    {-# LANGUAGE FlexibleContexts #-}
    module Main where

    import Dwt
    import TData
    import TGraph
    import TSearch
    import TView
    import TMmParse

    import Test.HUnit

-- main
    main = runTestTT $ TestList
      [   TestLabel "tGraph"   tGraph
        , TestLabel "tView"    tView
        , TestLabel "tMmParse" tMmParse
        , TestLabel "tSearch" tSearch
      ]
