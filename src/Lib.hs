{-# LANGUAGE MultiWayIf, OverloadedStrings, TemplateHaskell #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Control.Lens
import qualified Data.Conduit.List as CL

import Accumulators.Basic
import Parser(parseHead
             , parseRow)
import Types(Row(..)
            , RowVal(..)
            , rowlen)


data Result = MaximaRow [RowVal]
            | MinimaRow [RowVal]
            | SumRow [RowVal]
            | NullCountRow [RowVal]
            | CountRow [RowVal]
            | AverageRow [RowVal]
            | Incomplete
            deriving (Show, Eq)


data ResultRows = ResultRow {
  _maxima :: Result
  , _minima :: Result
  , _sums :: Result
  , _nulls :: Result
  , _counts :: Result
  , _averages :: Result
  } deriving (Show)

makeLenses ''ResultRows

modifyResultRow :: Int -> Row -> ResultRows -> ResultRows
modifyResultRow 0 (Row row) result = let
  newresultEntry = MaximaRow row
  in set maxima newresultEntry result
modifyResultRow 1 (Row row) result = let
  newresultEntry = MinimaRow row
  in set minima newresultEntry result
modifyResultRow 2 (Row row) result = let
  newresultEntry = SumRow row
  in set sums newresultEntry result
modifyResultRow 3 (Row row) result = let
  newresultEntry = NullCountRow row
  in set nulls newresultEntry result
modifyResultRow 4 (Row row) result = let
  nullcounts = result^.nulls
  (NullCountRow nulltotals) = nullcounts
  [NumberVal total] = row
  countresult = CountRow $ fmap (\(NumberVal n) -> NumberVal $ total - n) nulltotals
  in set counts countresult result
modifyResultRow _ _ _ = error "Out of bounds call to modifyResultRow"

computeAverages :: ResultRows -> ResultRows
computeAverages result = let
  (SumRow sumVals) = result^.sums
  (CountRow countVals) = result^.counts
  averager (NumberVal a) (NumberVal b) = NumberVal $ a / b
  sumCountTogether = zipWith averager sumVals countVals
  newresultEntry = AverageRow sumCountTogether
  in set averages newresultEntry result
     
conduitMaxMin :: Row -> ZipConduit Row Row IO ()
conduitMaxMin initial = ZipConduit (conduitMax initial) <* ZipConduit (conduitMin initial)

conduitSumsNulls :: Row -> ZipConduit Row Row IO ()
conduitSumsNulls initial = ZipConduit (conduitSums initial) <* ZipConduit (conduitNulls initial)

conduitSummary :: Int -> ResultRows -> Conduit Row IO ResultRows
conduitSummary n output = do
  val <- await
  case val of
    Nothing -> do
      let newResult = computeAverages output
      yield newResult
      return ()
    Just row -> do
      let newResult = modifyResultRow n row output
      conduitSummary (n+1) newResult
      
conduit :: Row -> ConduitM Row Row IO ()
conduit row = do
  let initial = columnRecorderInit row 
  getZipConduit $ conduitMaxMin initial <* conduitSumsNulls initial <* ZipConduit (conduitCount 0)

source :: Source IO Row
source = do
  header <- parseHead sourceHead
  rows <- mapM (parseRow header) sourcedata
  CL.sourceList rows
  
sink :: Sink ResultRows IO ()
sink = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      liftIO $ print n
      sink

-- Example of tying them together
setup :: IO ()      
setup = do
  header <- parseHead sourceHead
  row <- parseRow header (sourcedata !! 2)
  source $$ conduit row =$= conduitSummary 0 initialResult =$ sink

columnRecorderInit :: Row -> Row
columnRecorderInit row = Row $ replicate (rowlen row) (NumberVal 0)

initialResult :: ResultRows
initialResult = ResultRow Incomplete Incomplete Incomplete Incomplete Incomplete Incomplete 


-- Sample source data for testing
sourceHead :: String
sourceHead = "\"sessionId (text)\",\"page (text)\",\"latency (number)\",\"timeOnPage (number)\""

sourcedata :: [String]
sourcedata = [
   "36ce2946,explore,22,89.600"
   ,"360ed618,,47,"
   , "39015706,,78,"
   , "b88ab0b7,welcome,7,39.926"
   , "b8867283,query,56,447.209"
   , "37e73994,query,81,387.251"
   , "b8b089f8,explore,118,133.843"
   , "b7328b13,explore,30,179.696"
   , "38943904,query,44,442.492"
   , "36977939,explore,22,116.256"
   ]

