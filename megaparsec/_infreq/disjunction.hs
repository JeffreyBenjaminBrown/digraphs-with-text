  -- I removed this from Experim.hs
  -- parse a disjunction
    data Cmd = A | B deriving (Show)

    parseA = lexeme (string "A") *> pure A
    parseB = lexeme (string "B") *> pure B
    parseCmd = sc *> (parseA <|> parseB)
