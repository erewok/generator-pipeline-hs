{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module Accumulators.Basic where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$), (=$=), (=$)
                    , Conduit, Sink, Source
                    , await, yield)
import qualified Data.Conduit.List as CL
import Data.Sequence
import Prelude hiding (head, length, drop)


averageLen :: Int
averageLen = 10

type Size = Int
type RunningSum = Int


source :: Source IO Int
source = CL.sourceList [1..1000]


conduit :: RunningSum -> Size -> Seq Int -> Conduit Int IO Int
conduit total len sequence = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      let newtotal = total + n
      let newsequence = (|>) sequence n
      if | length newsequence < len -> conduit newtotal len newsequence
         | length newsequence == len -> do
             yield $ newtotal `div` (fromIntegral len)
             conduit newtotal len newsequence
         | otherwise -> do
             let newSeq = viewl newsequence
             case newSeq of
               EmptyL -> return () -- shouldn't happen: we just appended to it above?
               (a :< seq) -> do
                 let dropFrontTotal = newtotal - a
                 yield $ dropFrontTotal `div` (fromIntegral len)
                 conduit dropFrontTotal len seq

sink :: Sink Int IO ()
sink = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      liftIO $ print n
      sink

main :: IO ()
main = do
  let initial = fromList []
  source $$ conduit 0 averageLen initial =$ sink
