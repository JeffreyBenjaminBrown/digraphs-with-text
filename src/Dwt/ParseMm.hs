-- usually folded
  -- TODO
    -- Make those magic numbers (e.g. 11, 23) into functions, ala edgeNode
      -- defined right next to the data definitions that imply them
    -- (Stale, premature): Now that the parsing's done, what was the hangup in it?
      -- Either ParseError _ not reconcilable with Either String _
        -- surely an elegant solution exists?
      -- more?
  -- CREDITS: uses some functions by Jake Wheat
    -- https://github.com/JakeWheat/intro_to_parsing
    -- parse2 below is what Wheat called parseWithLeftOver
  -- styles|fonts: incomplete, ignoring
    -- within-node ones, e.g. LOCALIZED_STYLE_REF="styles.topic", this captures
    -- but <font ...> tags outside of a node applicable to it, this does not

-- lang, modules
    {-# LANGUAGE FlexibleContexts #-}
    module Dwt.ParseMm
      ( module Text.Parsec
      , module Text.Parsec.String
      , module Dwt.ParseMm
      ) where
    import Text.Parsec
    import Text.Parsec.String (Parser)
    import Control.Monad (foldM)
    import Control.Monad.Except
    import qualified Data.Map as Map
    import qualified Data.Maybe as Mb
    import qualified Data.Time as T
    import qualified Data.List as L

    import Dwt.Graph

-- types
    type MmNode = Int

    data MlTag = MlTag { title :: String 
      , isStart :: Bool -- leading < indicates start; </ indicates continuation.
      , isEnd :: Bool   -- trailing /> indicates end; > indicates continuation.
      , mlMap :: Map.Map String String
      } | Comment deriving (Eq, Show)

    data MmNLab = MmNLab { text :: String -- inaccurate type name: hold both data
                         , mmId :: MmNode -- about the node label and other lnodes
                         , style :: Maybe String -- it will be connected to
                         , created :: T.UTCTime
                         , modified :: T.UTCTime } deriving (Eq, Show)

    data MmELab = TreeEdge | ArrowEdge deriving (Eq, Show)

    data MmObj = MmText MmNLab | MmArrow {dest ::  MmNode}
      deriving (Eq, Show)
      -- the xml is an interleaved nested list of nodes and arrows
        -- in which the nesting matters; it lets succesion be implicit
      -- to process such a list, I need a type that unifies those two things

    type DwtSpec = ( [MmNLab], [(MmNode,MmNode,MmELab)] ) -- (nodes,edges)
    type DwtFrame = (Mindmap, Map.Map String Int)

-- constructors, helpers
    mmNLabDummy :: MmNLab
    mmNLabDummy = MmNLab "hi" 0 Nothing t t
      where t = T.UTCTime (T.fromGregorian 1989 11 30) 0

    -- TODO: This could use a higher-kinded function, lke eitherToMe, for Maybes
      -- should in that case take also a String to show if Nothing happens
    mapLookupMe :: (Ord k, Show k, MonadError String me) -- TODO ? BAD
      => k -> Map.Map k a -> me a
    mapLookupMe k m = case Map.lookup k m of
      Just a -> return a
      Nothing -> throwError $ "mapLookupMe: " ++ show k ++ " not in map."   

    eitherToMe :: (Show e, MonadError String me)
      => (a -> Either e t) -> a -> me t
    eitherToMe f x = case f x of Right y -> return y
                                 Left e -> throwError $ show e

-- parsing
  -- Parser a -> String -> _
    parseWithEof :: Parser a -> String -> Either ParseError a
    parseWithEof p = parse (p <* eof) ""

    eParse :: Parser a -> String -> Either ParseError a
    eParse p = parse p ""

    eParse2 :: Parser a -> String -> Either ParseError (a,String)
    eParse2 p = parse ((,) <$> p <*> leftOver) ""
      where leftOver = manyTill anyToken eof

  -- parsing the .mm format
   -- elements of the mlTag parser
    lexeme :: Parser a -> Parser a
    lexeme p = p <* spaces

    mmEscapedChar :: Parser Char
    mmEscapedChar = mmLeftAngle <|> mmNewline <|> mmRightAngle 
        <|> mmCaret <|> mmAmpersand <|> mmApostrophe
      where sandwich s = try $ string $ "&" ++ s ++ ";"
            mmLeftAngle = pure '<' <* sandwich "lt"
            mmNewline = pure '\n' <* sandwich "#xa"
            mmRightAngle = pure '>' <* sandwich "gt"
            mmCaret = pure '"' <* sandwich "quot"
            mmAmpersand = pure '&' <* sandwich "amp"
            mmApostrophe = pure '\'' <* sandwich "apos"

    mmStr = between quot quot 
      $ many $ mmEscapedChar <|> satisfy (/= '"')
      where quot = char '"'

    word :: Parser String -- that is, a Word outside of an MmNLab
    word = many1 $ alphaNum <|> char '_'

    keyValPair :: Parser (String,String)
    keyValPair = (,) <$> (lexeme word <* lexeme (char '=')) <*> lexeme mmStr

   -- parsing tags and comments
    mlTag :: Parser MlTag -- IS tested but strangely
    mlTag = do isStart <- startsItself
               title <- lexeme word
               pairs <- many $ lexeme keyValPair
               isEnd <- endsItself -- not use lexeme here, rather a level up
               return $ MlTag { title = title
                              , isStart = isStart
                              , isEnd = isEnd
                              , mlMap = Map.fromList pairs
                              }
      where endsItself =     (string "/>" >> return True) 
                         <|> (string ">" >> return False) :: Parser Bool
            startsItself  =  (try $ string "</" >> return False)
                         <|> (string "<" >> return True) :: Parser Bool

    comment :: Parser MlTag -- found in Text.ParserCombinators.Parsec.Combinator
    comment  = do string "<!--"
                  manyTill anyChar (try $ string "-->")
                  return Comment

    strip :: Parser a -> Parser [Char]
    strip p = many $ (skipMany $ try p) >> anyChar

    mlTags :: String -> Either ParseError [MlTag]
    mlTags file =     eParse (strip comment) file
                  >>= eParse (many $ lexeme mlTag)

-- functions of type (Functor f => f MlTag -> _), and their helpers
  -- helpers
    parseId :: String -> Either ParseError MmNode
    parseId s = read <$> eParse (string "ID_" *> many digit) s

    mmTimeToTime :: Int -> T.UTCTime
    mmTimeToTime mt = T.addUTCTime dur start
      where seconds = floor $ fromIntegral mt / 1000
            dur = realToFrac $ T.secondsToDiffTime seconds
            start = T.UTCTime (T.fromGregorian 1970 1 1) 0

  -- MlTag -> _
    tagToKeep :: MlTag -> Bool
    tagToKeep t = elem (title t) ["node","arrowlink"]

    readMmNLab :: (MonadError String me) => MlTag -> me MmNLab
    readMmNLab tag = 
      let m = mlMap tag
          style = Map.lookup "LOCALIZED_STYLE_REF" m -- style stays Maybe
          parseTime = mmTimeToTime . read
      in do text <- mapLookupMe "TEXT" m
            mmId <- mapLookupMe "ID" m >>= eitherToMe parseId
            created <- mapLookupMe "CREATED" m
            modified <- mapLookupMe "MODIFIED" m
            return $ MmNLab text mmId style (parseTime created) 
                                            (parseTime modified)

    mlArrowDestMe :: (MonadError String me) => MlTag -> me MmNode
    mlArrowDestMe t = (mapLookupMe "DESTINATION" $ mlMap t)
                       >>= eitherToMe parseId

  -- dwtSpec :: [MlTag] -> Either String DwtSpec
    dwtSpec :: [MlTag] -> Either String DwtSpec
    dwtSpec [] = Right ([],[]) -- silly case; could arguably return Left
    dwtSpec tags =
      let relevantTags = filter (flip elem ["node","arrowlink"] . title) tags
      in do rootLab <- readMmNLab $ head relevantTags
            -- Assumes first tag is a node. It can't be an arrow.
            dwtSpec' [mmId rootLab] (tail relevantTags) ([rootLab], [])

    dwtSpec' :: [MmNode] -> [MlTag] -> DwtSpec -> Either String DwtSpec
    dwtSpec' [] [] spec = Right spec
    dwtSpec' _ [] spec = Left "ran out of MmTags but not ancestors."
    dwtSpec' ancestry (t:ts) spec@(nLabs,lEdges) = case title t of
      "node" -> case isStart t of
        False -> dwtSpec' (tail ancestry) ts spec
        True -> do newNLab <- readMmNLab t
                   let newLEdge = (head ancestry, mmId newNLab, TreeEdge)
                       newSpec = (newNLab : nLabs, newLEdge : lEdges)
                     in case isEnd t of
                       False -> dwtSpec' (mmId newNLab : ancestry) ts newSpec
                       True ->  dwtSpec'                 ancestry  ts newSpec
      "arrowlink" -> do dest <- mlArrowDestMe t
                        let newLEdge = (head ancestry, dest, ArrowEdge)
                          in dwtSpec' ancestry ts (nLabs, newLEdge:lEdges)
      _ -> Left "MmTag neither a node nor an arrow"

-- DwtSpec -> _
    -- WARNING: The Nodes of these objects are interdependent.
    frameNodes :: Mindmap -- no styles and no edges in this one
    frameNodes = mkGraph [ (0, Str "root|this graph")
                           , (1, Str ".system")
                             , (2, Str ".mm rels")
                               , (3, stringToTplt "_ .mm/ _")
                               , (4, stringToTplt "_ .mm~ _")
                             , (5, Str "times")
                               , (6, stringToTplt "_ was created on _")
                               , (7, stringToTplt "_ was last modified on _")
                             , (8, Str "styles") 
                           , (9, Str "rels")
                             , (10, stringToTplt "_ instance/ _")
                             , (11, stringToTplt "_ read as/ _") ] []

    edgeNode :: MmELab -> Node -- depends on frameNodes
    edgeNode TreeEdge = -3 -- frame is later negated, so negate all indexes for it
    edgeNode ArrowEdge = -4

    frameSansStyles :: Mindmap -- counting Rels, this has 22 Nodes
      -- so styles will occupy Nodes starting at 23
    frameSansStyles = conn [0,1] $ conn [0,9]
      $ conn [1,2] $ conn [1,5] $ conn [1,8]
      $ conn [2,3] $ conn [2,4]
      $ conn [5,6] $ conn [5,7]
      $ conn [9,10] $ conn [9,11] 
      $ frameNodes where conn = insRelUsf 10

    styles :: DwtSpec -> [String]
    styles = L.nub . Mb.mapMaybe style . fst

    negateMm :: Mindmap -> Mindmap
    negateMm m = gmap (\(a,b,c,d) -> (negAdj a, -b, c, negAdj d)) m
      where negAdj = map (\(label,n) -> (label,-n))

    frameOrphanStyles :: DwtSpec -> DwtFrame
    frameOrphanStyles spec = let ss = styles spec
      in ( negateMm $ foldl (\mm font -> insStr font mm) frameSansStyles ss
         , Map.fromList $ zip (styles spec) [-23,-24 ..] )

    frame :: (MonadError String me) => DwtFrame -> me DwtFrame
    frame (mm, mp) = do mm' <- foldM (\mm n -> insRel (-10) [-8, n] mm)
                                       -- n is already negative
                                     mm (Map.elems mp)
                        return (mm',mp)

    loadNodes :: (MonadError String me) => (DwtSpec, DwtFrame) -> me Mindmap
    loadNodes ( (ns,_), (mm, mp) ) =
      let noded = foldl (\mm n -> insNode (mmId n, Str $ text n) mm) mm ns
      in foldM (\mm n -> insRel (-11) [ mmId n
                                      , (Map.!) mp $ Mb.fromJust $ style n
                                      ] mm)
               noded $ filter (Mb.isJust . style) ns

    loadEdges :: (MonadError String me) => DwtSpec -> Mindmap -> me Mindmap
    loadEdges (_,es) mm = foldM (\mm (from,to,kind) 
                                  -> insRel (edgeNode kind) [from,to] mm
                                ) mm es

    -- load into the frame
      -- for each MmNLab
        -- add it keeping its ID intact
        -- connect it to the corresponding style node
        -- aborted; was going to:
          -- create two more nodes for its created-on and modified-on times
          -- connect it to those
          -- connect them to the system node
      -- for each MnELab: ?

-- deprecating: unsafe functions
    fromRight :: Either a b -> b
    fromRight (Right b) = b
    fromRight (Left _) = error "fromRight: Left"

    mlArrowDestUsf :: MlTag -> Either ParseError MmNode
    mlArrowDestUsf t = parseId $ mlMap t Map.! "DESTINATION"

    readMmNLabUsf :: MlTag -> MmNLab -- this process is lossy
      -- that is, the ml tag has more info than I use
    readMmNLabUsf tag = 
      let m = mlMap tag
          text = m Map.! "TEXT"
          mmId = fromRight $ parseId $ m Map.! "ID"
          style = if Map.member "LOCALIZED_STYLE_REF" m
                    then Just $ m Map.! "LOCALIZED_STYLE_REF"
                    else Nothing
          created = mmTimeToTime $ read $ m Map.! "CREATED"
          modified = mmTimeToTime $ read $ m Map.! "MODIFIED"
      in MmNLab text mmId style created modified
