-- setup
    {-# LANGUAGE FlexibleContexts #-}
    module Main where

    import Dwt
    import TData
    import TGraph
    import TMmParse

    import Test.HUnit

-- main
    main = runTestTT $ TestList
      [   TestLabel "tGraph"   tGraph
        , TestLabel "tMmParse" tMmParse
      ]
