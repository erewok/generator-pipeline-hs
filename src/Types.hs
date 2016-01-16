{-# LANGUAGE OverloadedStrings #-}

module Types (
      RowVal(..)
    , ColumnHead(..)
    , ColumnHeader(..)
    , ColumnType(..)
    , Row(..)
    , rowlen
    , rowzip
    , preCompareMaxMin
    , sumRowVals
    , nullRowCount
    ) where

import qualified Data.Text as T

newtype ColumnHeader = ColumnHeader [ColumnHead] deriving (Show)

data ColumnHead = ColumnHead { colname :: T.Text
                             , coltype :: ColumnType
                             } deriving (Show, Eq, Ord)

data ColumnType = NumberType
                 | StringType
                   deriving (Show, Eq, Ord)

newtype Row = Row [RowVal] deriving (Show)

data RowVal = NumberVal Double
            | StringVal T.Text
            | Null
            deriving (Show, Eq)

rowlen :: Row -> Int
rowlen (Row vals) = length vals

rowzip :: Row -> Row -> [(RowVal, RowVal)]
rowzip (Row firstrow) (Row secondrow) = zip firstrow secondrow 

-- In practice we are committed to putting records on left
-- (which means always NumberVals on left)
-- ToDo: this should be enforced by the compiler...

-- We throw away Nulls when comparing Max/Min
preCompareMaxMin :: (RowVal, RowVal) -> (RowVal, RowVal)
preCompareMaxMin (NumberVal n, Null) = (NumberVal n, NumberVal n)
preCompareMaxMin (NumberVal n, NumberVal i) = (NumberVal n, NumberVal i)
preCompareMaxMin (NumberVal n, StringVal s) = (NumberVal n, NumberVal $ fromIntegral $ T.length s)
preCompareMaxMin _ = error "Badly matched types when comparing max/min"

sumRowVals :: (RowVal, RowVal) -> Double
sumRowVals (NumberVal n, Null) = n
sumRowVals (NumberVal n, StringVal s) = n + fromIntegral (T.length s)
sumRowVals (NumberVal n, NumberVal i) = n + i
sumRowVals _ = error "Badly matched types when summing values"

nullRowCount :: Row -> Row
nullRowCount (Row vals) = Row $ map (NumberVal . isNull) vals

isNull :: RowVal -> Double
isNull Null = 1
isNull _ = 0
