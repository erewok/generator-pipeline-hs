{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS 
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import System.Environment

import Lib
import Accumulators.Basic
import Parser
import Types

main :: IO ()
main = do
  putStrLn "Consuming from Generator."
  [executable, number] <- getArgs
  
  -- kind of a cheap hack: run it once to get the header. actual run is later.
  (Inherited, fromProcess, ClosedStream, _) <-
    streamingProcess (shell $ executable ++ " 1")

  headerRow <- fromProcess $$ CB.lines =$ CL.head
  case headerRow of
    Nothing -> putStrLn "No header input. Check generator executable."
    Just bytestring -> do
      header <- parseHead (BS.unpack bytestring)
      let initial = rowRecordInit header
      (Inherited, fromProcess2, ClosedStream, _) <-
        streamingProcess (shell $ executable ++ " " ++ number)
      fromProcess2 $$ CB.lines
        =$= CL.mapM (parseRow header . BS.unpack)
        =$= conduit initial
        =$= conduitSummary 0 initialResult
        =$ sink

-- print vals while they stream by in order to check on them
printConduit :: ConduitM BS.ByteString BS.ByteString IO ()
printConduit = do
  val <- await
  case val of
    Nothing -> return ()
    (Just row) -> do
      lift $ print row
      yield row
      printConduit
  

rowRecordInit :: ColumnHeader -> Row
rowRecordInit header = Row $ replicate (headerlen header) (NumberVal 0)
