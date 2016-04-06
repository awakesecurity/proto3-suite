{-# LANGUAGE DeriveGeneric #-}

module TestTypes where

import           Data.Int
import           Data.Protobuf.Wire.Generic
import qualified Data.Text.Lazy as TL
import           GHC.Generics

data Trivial = Trivial {trivialField :: Int32}
                deriving (Show, Generic, Eq)
instance HasEncoding Trivial

data MultipleFields =
  MultipleFields {multiFieldDouble :: Double,
                  multiFieldFloat :: Float,
                  multiFieldInt32 :: Int32,
                  multiFieldInt64 :: Int64,
                  multiFieldString :: TL.Text}
                  deriving (Show, Generic, Eq)
instance HasEncoding MultipleFields

data TestEnum = ENUM1 | ENUM2 | ENUM3
                deriving (Show, Generic, Enum, Eq)
instance HasEncoding TestEnum

data WithEnum = WithEnum {enumField :: TestEnum}
                deriving (Show, Generic, Eq)
instance HasEncoding WithEnum

data Nested = Nested {nestedField1 :: TL.Text,
                      nestedField2 :: Int32}
                      deriving (Show, Generic, Eq)
instance HasEncoding Nested

data WithNesting = WithNesting {nestedMessage :: Nested}
                    deriving (Show, Generic, Eq)
instance HasEncoding WithNesting

data WithRepetition = WithRepetition {repeatedField1 :: [Int32]}
                      deriving (Show, Generic, Eq)
instance HasEncoding WithRepetition
