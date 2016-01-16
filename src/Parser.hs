{-# LANGUAGE OverloadedStrings #-}
-- Sample input
-- "sessionId (text)","page (text)","latency (number)","timeOnPage (number)"
-- 38516059,explore,64,148.676

module Parser (
    parseHead
  , parseRow
  ) where

import Control.Applicative
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import Types

{- Header Parsers
We wish to produce a list of `ColumnHead` values,
each of which will tell us:
1. The name of the column, and
2. The type of the column.
-}
parseHead :: (Monad m) => String -> m ColumnHeader
parseHead input = case P.parse parseHead' "unknown" input of
    Left _ -> error "Failed to parse input header: bailing out"
    Right a -> return a

-- Todo: Will choke on trailing comma.
parseHead' :: P.Parser ColumnHeader
parseHead' = do
  chdr <- ColumnHeader <$> parseHeadCol `P.sepBy` P.char ','
  P.spaces
  P.eof
  return chdr

parseHeadCol :: P.Parser ColumnHead
parseHeadCol = do
  _ <- quoteChar
  P.optional P.space
  name <- P.many1 P.alphaNum
  P.optional P.space
  ctype <- parseColType
  _ <- quoteChar
  return $ ColumnHead (T.pack name) ctype

parseColType :: P.Parser ColumnType
parseColType = P.try parseStringTypeCol
  <|> parseNumberTypeCol

parseStringTypeCol :: P.Parser ColumnType
parseStringTypeCol = P.string "(text)" >> return StringType

parseNumberTypeCol :: P.Parser ColumnType
parseNumberTypeCol = P.string "(number)" >> return NumberType

quoteChar :: P.Parser Char
quoteChar = P.oneOf "\"\'"

{- Row Parsers
We need a ColumnHeader to know which parser to apply to each row value.

We plan to partially apply the `parseRowVal` to each `ColumnHead`
so we can use this list of functions to parse the corresponding row values.
-}
parseRow :: (Monad m) => ColumnHeader -> String -> m Row
parseRow (ColumnHeader chdr) input = do
  let parsingFuncs = parseRowVal <$> chdr
  let splittedRow = splitOn "," input  -- Using String splitter here...
  return $ Row $ foldAp parsingFuncs splittedRow

foldAp :: [P.Parser RowVal] -> [String] -> [RowVal]
foldAp [] _ = []
foldAp _ [] = []
foldAp (f:fs) (y:ys) = actuallyParseRowVal (P.parse f "unknown" y) : foldAp fs ys

actuallyParseRowVal :: Either a RowVal -> RowVal
actuallyParseRowVal (Left _) = Null
actuallyParseRowVal (Right a) = a

parseRowVal :: ColumnHead -> P.Parser RowVal
parseRowVal ch = case coltype ch of
  NumberType -> parseRowNull
    <|> parseRowNumber
    P.<?> "Number or Null expected"
  StringType -> parseRowNull
    <|> P.try parseRowString
    P.<?> "String or Null expected"

parseRowString ::  P.Parser RowVal
parseRowString = StringVal <$> T.pack <$> P.many1 P.alphaNum

parseRowNumber :: P.Parser RowVal
parseRowNumber = do
     -- We return a function that might negate our result later
     doNeg <- (P.char '-' >> return negate) <|> return id
     -- Every number can have
     realPart <- realN
     fracPart <- fracN
     expPart  <- expN
     return $ NumberVal $ doNeg $ (realPart + fracPart) * 10 ** expPart

parseRowNull :: P.Parser RowVal
parseRowNull = P.eof >> return Null


{- Number parsing machinery:
copied for expediency from
https://gist.github.com/zearen-wover/1692518
which has not provided a license, unfortunately.
-}

realN :: P.Parser Double
realN = (P.char '0' >> return 0)
  <|> (do
          -- It's either just a 0, or begins with 1 - 9
          firstDigit <- P.satisfy (\ ch -> '1' <= ch && ch <= '9')
          -- after that it can be any digit
          restDigits <- P.many P.digit
          return $ read $ firstDigit : restDigits)

-- This parser will fail if there's no dot
-- That means there's no fractional part, so we default to 0
fracN :: P.Parser Double
fracN = fracN1 <|> zero

fracN1 :: P.Parser Double
fracN1 = do
  _ <- P.char '.'
  digits <- P.many1 P.digit
  return $ read $ "0." ++ digits

expN :: P.Parser Double
expN = expN1 <|> zero

expN1 :: P.Parser Double
expN1 = do
  _ <- P.oneOf "eE"
  doNeg <- (P.char '-' >> return negate)
    <|> (P.char '+' >> return id)
    <|> return id
  digits <- P.many1 P.digit
  return $ doNeg $ read digits

zero :: P.Parser Double
zero = return 0
