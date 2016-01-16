{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
{-
λ> let testhead = "\"sessionId (text)\",\"page (text)\",\"latency (number)\",\"timeOnPage (number)\""
λ> let testrow1 = "38b56b0a,explore,44,125.591"
λ> let testrow2 =  "38b56b0a,,,125.591"
λ> chdr <- parseHead testhead
λ> let res1 = parseRow chdr1 testrow1
λ> res1
[String "38b56b0a",String "explore",Number 44.0,Number 125.591]
λ> let res2 = parseRow chdr1 testrow2
λ> res2
[String "38b56b0a",Null,Null,Number 125.591]
-}
module Lib where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$), (=$=), (=$)
                    , Conduit, Sink, Source
                    , await, yield
                    , ZipConduit
                    , getZipConduit)
import qualified Data.Conduit.List as CL
import Data.Sequence hiding (zip, replicate)
import qualified Data.Text as T
import Prelude hiding (head, length, drop)
import qualified Data.Vector.Unboxed as V

import Accumulators.Basic
import Parser(parseHead
             , parseRow)
import Types(Row(..)
            , RowVal(..)
            , rowlen
            , rowzip
            , preCompareMaxMin
            , sumRowVals
            , nullRowCount)

{- things to keep track of
For all columns, compute:
count
null count
For number columns, compute:
minimum
maximum
average
For text columns, compute:
count(shortest value)
count(longest value)
average length
break ties alphabetically as needed
-}
source :: Source IO Row
source = do
  header <- parseHead sourceHead
  rows <- mapM (parseRow header) sourcedata
  CL.sourceList rows
  
sink :: Sink Row IO ()
sink = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      liftIO $ print n
      sink

setup :: IO ()
setup = do
  header <- parseHead sourceHead
  row <- parseRow header (sourcedata !! 2)
  let initial = columnRecorderInit row
  source $$ conduitMin initial =$ sink

-- main :: IO ()
-- main = do
--   let initial = fromList []
--   sourcedata $$ conduit 0 0 initial =$ sink
--sink = CL.mapM_ print

columnRecorderInit :: Row -> Row
columnRecorderInit row = Row $ replicate (rowlen row) (NumberVal 0)

-- Sample source data
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

