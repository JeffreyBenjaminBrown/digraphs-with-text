    module TMmParse where

    import Dwt
    import Test.HUnit
    import qualified Data.Map as Map

    tMmParse = TestList [ TestLabel "tParseMmXml" tParseMmXml
                        , TestLabel "tParseMmTags" tParseMmTags
                        ]

-- parse .mm(the xml format)
    tParseMmXml = TestList [ TestLabel "tMmStr" tMmStr
                           , TestLabel "tWord" tWord
                           , TestLabel "tComment" tComment
                           , TestLabel "tKeyValPair" tKeyValPair
                           , TestLabel "tStrip" tStrip
                           , TestLabel "tMlTag" tMlTag]

    tMmStr = TestCase $ do
      assertBool "mmStr" $ eParse2 mmStr "\"aygaw\"bbbb"
        == Right ("aygaw","bbbb")
      assertBool "the escape characters"
        $ eParse2 mmStr "\"&lt;&amp;&gt;  &apos;&quot;&#xa;\"111"
        == Right ("<&>  '\"\n","111")

    tWord = TestCase $ do
      assertBool "tWord"
        $ eParse2 (many $ word <* spaces) "bird thug_a\nMAZ3 \n 13;;;"
        == Right (["bird","thug_a","MAZ3","13"],";;;")

    tComment = TestCase $ do
      assertBool "tComment" $ eParse2 comment "<!--xxx-->yyy"
        == Right (Comment,"yyy")

    tKeyValPair = TestCase $ do
      assertBool "tKeyValPair" $ eParse2 keyValPair "word=\"nacho\""
        == Right( ("word","nacho"), "")
      assertBool "list of key-value pairs; lexme"
        $ eParse2 (many $ lexeme keyValPair) "a=\"1\" b=\"2\""
        == Right( [("a","1"), ("b","2")], "")

    tStrip = TestCase $ do
      assertBool "strip -- symbols" 
        $ eParse (strip $ string "--") "-a--b-c--dd---"
        == Right                       "-ab-cdd-"

    tMlTag = TestCase $ do
      assertBool "parse mlTag" $ eParse mlTag "<hi a=\"1\" bb =\"22\" >"
        == Right ( MlTag "hi" True False -- WHY can't I dollar these parens?
                         ( Map.fromList [("a","1"), ("bb","22")] )
                 )
      assertBool "parse mlTag" $ eParse mlTag "</hi a=\"1\" bb =\"22\" />"
        == Right ( MlTag "hi" False True -- WHY can't I dollar these parens?
                         ( Map.fromList [("a","1"), ("bb","22")] )
                 )

  -- manip mmTags
    tParseMmTags = TestList [ TestLabel "tParseId" tParseId
                            , TestLabel "tMmNLab" tMmNLab ]

    tParseId = TestCase $ do
      assertBool "parse ID strings" $ parseId "ID_123" == Right 123

    tMmNLab = TestCase $ do
      assertBool "parse an xml TEXT tag into an TextTag"
        $ (readMmNLabUsf $ MlTag { 
          title = "node"
          , isStart = True
          , isEnd = True
          , mlMap = Map.fromList [
              ("CREATED","1449389483215")
            , ("ID","ID_1033943189")
            , ("LOCALIZED_STYLE_REF","AutomaticLayout.level,2")
            , ("MODIFIED","1449389512135")
            , ("TEXT","c3, gold")]})
        == MmNLab "c3, gold" 1033943189 (Just "AutomaticLayout.level,2")
             (read "2015-12-06 08:11:23 UTC") (read "2015-12-06 08:11:52 UTC")

-- testing by hand
    tFrame = do 
      x <- mmToMlTags "data/root+22ish.mm" -- root+22ish because it needs styles
      let y = fromRight $ dwtSpec $ fromRight x
        in return (frame $ frameOrphanStyles y :: Either String DwtFrame)

    tLoadNodes = do 
      mls <- mmToMlTags "data/root+22ish.mm" -- again, needs styles
      let spec = fromRight $ dwtSpec $ fromRight mls
          fr = frame $ frameOrphanStyles spec :: Either String DwtFrame
        in return $ (loadNodes (spec, fromRight fr) :: Either String Mindmap)
