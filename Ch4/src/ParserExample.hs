{-# LANGUAGE GADTs #-}
module ParserExample where


data StringParser where
  StringParser :: {runStringParser :: String -> (String, String)}
                    -> StringParser

takeCharacters :: Int -> StringParser
takeCharacters numCharacters = StringParser $ \inputString ->
  splitAt numCharacters inputString

getNextWord :: StringParser
getNextWord = StringParser $ \someString ->
    case break (== ' ') someString of 
        (nextWord, "") -> (nextWord, "") 
        (nextWord, rest) -> (nextWord, tail rest)

combineParsers :: StringParser -> StringParser -> StringParser 
combineParsers firstParser secondParser = StringParser $ \someString ->
    let (_firstPart, firstResult) = runStringParser firstParser someString 
    in runStringParser secondParser firstResult


getNextWordAfterTenLetters :: StringParser 
getNextWordAfterTenLetters =
  combineParsers (takeCharacters 10) getNextWord

tenLettersAfterTheFirstWord :: StringParser
tenLettersAfterTheFirstWord =
  combineParsers getNextWord (takeCharacters 10)

parseString :: StringParser -> String -> String
parseString parser inputString =
  fst $ runStringParser parser inputString
