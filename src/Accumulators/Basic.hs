{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module Accumulators.Basic where

import Data.Conduit (await
                    , yield
                    , Conduit)
import Prelude hiding (head, length, drop)

import Types(Row(..)
            , RowVal(..)
            , rowzip
            , preCompareMaxMin
            , sumRowVals
            , nullRowCount)



type NullCounts = Row
type Minima = Row
type Maxima = Row
type Sums = Row

conduitFunc :: (Row -> Row -> Row) -> Row -> Conduit Row IO Row
conduitFunc f record = do
  val <- await
  case val of
    Nothing -> do
      yield record
      return ()
    Just input -> do
      let updatedRecord = f record input
      -- yield updatedRecord
      conduitFunc f updatedRecord

conduitCount :: Int -> Conduit Row IO Row
conduitCount n = do
    val <- await
    case val of
      Nothing -> do
        yield $ Row [NumberVal $ fromIntegral n]
        return ()
      Just _ -> conduitCount (n + 1)

conduitSums :: Sums -> Conduit Row IO Sums
conduitSums = conduitFunc summedVals

conduitNulls :: NullCounts -> Conduit Row IO NullCounts
conduitNulls = conduitFunc nullCounts

conduitMax :: Maxima -> Conduit Row IO Maxima
conduitMax = conduitFunc rowMax

conduitMin :: Minima -> Conduit Row IO Minima
conduitMin = conduitFunc rowMin

compareMaxMin :: (Double -> Double -> Double) -> (RowVal, RowVal) -> RowVal
compareMaxMin f (NumberVal x, NumberVal y) = NumberVal $ f x y
compareMaxMin _ _ = error "Need to have two NumberVals to run this function"

summedVals :: Sums -> Row -> Sums
summedVals record input = Row $ map (NumberVal . sumRowVals) $ rowzip record input

nullCounts :: NullCounts -> Row -> Sums
nullCounts record input = summedVals record (nullRowCount input)


-- Todo: The following implementations of Max/Min are flawed.
-- would almost be better (more honest) to put in the lowest possible number here
-- and highest possible number for min. The real answer involves using Null on the left
-- and switching to a number as soon as one appears.
rowMax :: Maxima -> Row -> Maxima
rowMax record input =
  let
    vals = (map preCompareMaxMin $ rowzip record input)
    rowValMax (NumberVal x, NumberVal y) = NumberVal $ max x y
    rowValMax _ = error "rowValMax should only compare two NumberVals"
  in Row $ map rowValMax vals


-- Todo: fix min: see note above
rowMin :: Minima -> Row -> Minima
rowMin record input =
  let
    vals = (map preCompareMaxMin $ rowzip record input)
    rowValMin (NumberVal 0, NumberVal 0) = NumberVal 0
    rowValMin (NumberVal 0, NumberVal j) = NumberVal j
    rowValMin (NumberVal i, NumberVal j) = NumberVal (min i j)
    rowValMin _ = error "rowValMin should only compare two NumberVals"
  in Row $ map rowValMin vals
