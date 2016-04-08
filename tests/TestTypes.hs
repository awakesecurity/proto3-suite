{-# LANGUAGE DeriveGeneric #-}

module TestTypes where

import qualified Data.ByteString as B
import           Data.Int
import           Data.Monoid
import           Data.Protobuf.Wire.Generic
import           Data.Protobuf.Wire.Shared
import           Data.Protobuf.Wire.Decode.Parser
import qualified Data.Text.Lazy as TL
import           Data.Word (Word32, Word64)
import           GHC.Generics

data Trivial = Trivial {trivialField :: Int32}
                deriving (Show, Generic, Eq)
instance HasEncoding Trivial

data MultipleFields =
  MultipleFields {multiFieldDouble :: Double,
                  multiFieldFloat :: Float,
                  multiFieldInt32 :: Int32,
                  multiFieldInt64 :: Int64,
                  multiFieldString :: TL.Text,
                  multiFieldBool :: Bool}
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

instance ProtobufParsable Nested where
  fromField = parseEmbedded $ do
    x <- field $ FieldNumber 1
    y <- field $ FieldNumber 2
    return $ Nested x y

instance ProtobufMerge Nested where
  protobufMerge (Nested x1 y1) (Nested x2 y2) = Nested (x1 <> x2) y2

data WithNesting = WithNesting {nestedMessage :: Nested}
                    deriving (Show, Generic, Eq)
instance HasEncoding WithNesting

data WithRepetition = WithRepetition {repeatedField1 :: [Int32]}
                      deriving (Show, Generic, Eq)
instance HasEncoding WithRepetition

data WithFixed = WithFixed {fixed1 :: (Fixed Word32),
                            fixed2 :: (Fixed Int32),
                            fixed3 :: (Fixed Word64),
                            fixed4 :: (Fixed Int64)}
                            deriving (Show, Generic, Eq)

data WithBytes = WithBytes {bytes1 :: B.ByteString,
                            bytes2 :: [B.ByteString]}
                            deriving (Show, Generic, Eq)
instance HasEncoding WithBytes

data WithPacking = WithPacking {packing1 :: [Int32],
                                packing2 :: [Int32]}
                                deriving (Show, Generic, Eq)
instance HasEncoding WithPacking
