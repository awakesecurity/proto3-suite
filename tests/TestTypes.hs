{-# LANGUAGE DeriveGeneric #-}

module TestTypes where

import qualified Data.ByteString as B
import           Data.Int
import           Data.Monoid
import           Data.Protobuf.Wire.Generic
import           Data.Protobuf.Wire.Shared
import           Data.Protobuf.Wire.Decode.Parser as P
import qualified Data.Text.Lazy as TL
import           Data.Word (Word32, Word64)
import           GHC.Generics
import           Test.QuickCheck (Arbitrary, arbitrary)

data Trivial = Trivial {trivialField :: Int32}
                deriving (Show, Generic, Eq)
instance HasEncoding Trivial

instance Arbitrary Trivial where
  arbitrary = Trivial <$> arbitrary

trivialParser :: Parser Trivial
trivialParser = Trivial <$> field (FieldNumber 1)

data MultipleFields =
  MultipleFields {multiFieldDouble :: Double,
                  multiFieldFloat :: Float,
                  multiFieldInt32 :: Int32,
                  multiFieldInt64 :: Int64,
                  multiFieldString :: TL.Text,
                  multiFieldBool :: Bool}
                  deriving (Show, Generic, Eq)
instance HasEncoding MultipleFields

instance Arbitrary MultipleFields where
  arbitrary = MultipleFields
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> fmap TL.pack arbitrary
              <*> arbitrary

multipleFieldsParser :: Parser MultipleFields
multipleFieldsParser = MultipleFields
                       <$> field (FieldNumber 1)
                       <*> field (FieldNumber 2)
                       <*> field (FieldNumber 3)
                       <*> field (FieldNumber 4)
                       <*> field (FieldNumber 5)
                       <*> field (FieldNumber 6)

data TestEnum = ENUM1 | ENUM2 | ENUM3
                deriving (Show, Generic, Enum, Eq)
instance HasEncoding TestEnum

instance Arbitrary TestEnum where
  arbitrary = fmap toEnum arbitrary

data WithEnum = WithEnum {enumField :: TestEnum}
                deriving (Show, Generic, Eq)
instance HasEncoding WithEnum

instance Arbitrary WithEnum where
  arbitrary = WithEnum <$> arbitrary

withEnumParser :: Parser WithEnum
withEnumParser = WithEnum <$> P.enumField (FieldNumber 1)

data Nested = Nested {nestedField1 :: TL.Text,
                      nestedField2 :: Int32}
                      deriving (Show, Generic, Eq)
instance HasEncoding Nested

instance Arbitrary Nested where
  arbitrary = Nested <$> fmap TL.pack arbitrary <*> arbitrary

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

instance Arbitrary WithNesting where
  arbitrary = WithNesting <$> arbitrary

withNestingParser :: Parser WithNesting
withNestingParser = WithNesting <$> field (FieldNumber 1)

data WithRepetition = WithRepetition {repeatedField1 :: [Int32]}
                      deriving (Show, Generic, Eq)
instance HasEncoding WithRepetition

instance Arbitrary WithRepetition where
  arbitrary = WithRepetition <$> arbitrary

withRepetitionParser :: Parser WithRepetition
withRepetitionParser = WithRepetition <$> repeatedPacked (FieldNumber 1)

data WithFixed = WithFixed {fixed1 :: (Fixed Word32),
                            fixed2 :: (Signed (Fixed Int32)),
                            fixed3 :: (Fixed Word64),
                            fixed4 :: (Signed (Fixed Int64))}
                            deriving (Show, Generic, Eq)
instance HasEncoding WithFixed

instance Arbitrary a => Arbitrary (Fixed a) where
  arbitrary = Fixed <$> arbitrary

instance Arbitrary a => Arbitrary (Signed a) where
  arbitrary = Signed <$> arbitrary

instance Arbitrary WithFixed where
  arbitrary = WithFixed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

withFixedParser :: Parser WithFixed
withFixedParser = WithFixed
                  <$> field (FieldNumber 1)
                  <*> field (FieldNumber 2)
                  <*> field (FieldNumber 3)
                  <*> field (FieldNumber 4)

data WithBytes = WithBytes {bytes1 :: B.ByteString,
                            bytes2 :: [B.ByteString]}
                            deriving (Show, Generic, Eq)
instance HasEncoding WithBytes

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary WithBytes where
  arbitrary = WithBytes <$> arbitrary <*> arbitrary

withBytesParser = WithBytes
                  <$> field (FieldNumber 1)
                  <*> repeatedUnpacked (FieldNumber 2)

data WithPacking = WithPacking {packing1 :: [Int32],
                                packing2 :: [Int32]}
                                deriving (Show, Generic, Eq)
instance HasEncoding WithPacking

instance Arbitrary WithPacking where
  arbitrary = WithPacking <$> arbitrary <*> arbitrary

withPackingParser = WithPacking
                    <$> repeatedUnpacked (FieldNumber 1)
                    <*> repeatedPacked (FieldNumber 2)
