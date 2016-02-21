    module TSearch where

    import Dwt
    import TData
    import Test.HUnit

    import Data.Either

    tSearch = TestList [ TestLabel "tQGet" tQGet
                       , TestLabel "tQInsRel" tQInsRel
                       ]

    tQGet = TestCase $ do
      assertBool "one (Right)" $ qGet g1 (QStr "brandy") == Right 4
      assertBool "none (Left)" $ isLeft $ qGet g1_0 (QStr "brandy")
      assertBool "two (Left)" $ isLeft $ qGet g1_2 (QStr "brandy")
      where g1_0 = delNode 4 g1
            g1_2 = insStr "brandy" g1

    tQInsRel = TestCase $ do
      let g1' = fromRight $ qInsRel (qt "_ wants _") [qn 4, qs "dog"] g1
      assertBool "brandy wants dog" $ 
        (pre g1' $ fromRight $ qGet g1' $ qs "dog") == [5,6,8,14]
