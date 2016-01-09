{-# LANGUAGE OverloadedStrings #-}

module Types (
      RowVal(..)
    , ColumnHead(..)
    , ColumnHeader(..)
    , ColumnType(..)
    , Row(..)
    ) where

import qualified Data.Text as T

newtype ColumnHeader = ColumnHeader [ColumnHead] deriving (Show)

data ColumnHead = ColumnHead { colname :: T.Text
                             , coltype :: ColumnType
                             } deriving (Show, Eq, Ord)

data ColumnType = NumberType
                 | StringType
                   deriving (Show, Eq, Ord)

newtype Row = Row [RowVal]

data RowVal = Number Double
            | String T.Text
            | Null
            deriving (Show, Eq)

            

-- newtype ColumnHeader s a = ColumHeader { getHeader :: s -> (s, a)}

