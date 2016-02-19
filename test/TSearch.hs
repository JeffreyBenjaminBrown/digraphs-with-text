    module TSearch where

    import Dwt
    import TData
    import Test.HUnit

    import Data.Either

    tSearch = TestList [ TestLabel "tQGet" tQGet
                       ]

    tQGet = TestCase $ do
      assertBool "one (Right)" $ qGet (QStr "brandy") g1 == Right 4
      assertBool "none (Left)" $ isLeft $ qGet (QStr "brandy") g1_0
      assertBool "two (Left)" $ isLeft $ qGet (QStr "brandy") g1_2
      where g1_0 = delNode 4 g1
            g1_2 = insStr "brandy" g1
