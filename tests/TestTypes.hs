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
import           GHC.Exts (toList, fromList)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof)

instance Arbitrary a => Arbitrary (PackedVec a) where
  arbitrary = fromList <$> arbitrary

instance Arbitrary a => Arbitrary (UnpackedVec a) where
  arbitrary = fromList <$> arbitrary

instance Arbitrary a => Arbitrary (NestedVec a) where
  arbitrary = fromList <$> arbitrary

instance Arbitrary a => Arbitrary (Nested a) where
  arbitrary = Nested <$> arbitrary

data Trivial = Trivial {trivialField :: Int32}
                deriving (Show, Generic, Eq)
instance HasEncoding Trivial
instance HasDecoding Trivial

instance Arbitrary Trivial where
  arbitrary = Trivial <$> arbitrary

data MultipleFields =
  MultipleFields {multiFieldDouble :: Double,
                  multiFieldFloat :: Float,
                  multiFieldInt32 :: Int32,
                  multiFieldInt64 :: Int64,
                  multiFieldString :: TL.Text,
                  multiFieldBool :: Bool}
                  deriving (Show, Generic, Eq)
instance HasEncoding MultipleFields
instance HasDecoding MultipleFields

instance Arbitrary MultipleFields where
  arbitrary = MultipleFields
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> fmap TL.pack arbitrary
              <*> arbitrary

data TestEnum = ENUM1 | ENUM2 | ENUM3
                deriving (Show, Generic, Enum, Eq)

instance Arbitrary TestEnum where
  arbitrary = oneof $ fmap return [ENUM1, ENUM2, ENUM3]

data WithEnum = WithEnum {enumField :: Enumerated (TestEnum)}
                deriving (Show, Generic, Eq)
instance HasEncoding WithEnum
instance HasDecoding WithEnum

instance Arbitrary e => Arbitrary (Enumerated e) where
  arbitrary = Enumerated <$> arbitrary

instance Arbitrary WithEnum where
  arbitrary = WithEnum <$> arbitrary

data NestedMsg = NestedMsg {nestedField1 :: TL.Text,
                      nestedField2 :: Int32}
                      deriving (Show, Generic, Eq)
instance HasEncoding NestedMsg
instance HasDecoding NestedMsg

instance Arbitrary NestedMsg where
  arbitrary = NestedMsg <$> fmap TL.pack arbitrary <*> arbitrary

instance Monoid NestedMsg where
  (NestedMsg t1 i1) `mappend` (NestedMsg t2 i2) = NestedMsg (t1 <> t2) i2
  mempty = NestedMsg mempty 0

{-
instance HasDecoding NestedMsg where
  decode = disembed $ do
    x <- decode $ FieldNumber 1
    y <- decode $ FieldNumber 2
    return $ NestedMsg x y
-}

data WithNesting = WithNesting {nestedMessage :: Nested NestedMsg}
                    deriving (Show, Generic, Eq)
instance HasEncoding WithNesting
instance HasDecoding WithNesting

instance Arbitrary WithNesting where
  arbitrary = WithNesting <$> arbitrary

data WithRepetition = WithRepetition {repeatedField1 :: PackedVec Int32}
                      deriving (Show, Generic, Eq)
instance HasEncoding WithRepetition
instance HasDecoding WithRepetition

instance Arbitrary WithRepetition where
  arbitrary = WithRepetition <$> arbitrary

data WithFixed = WithFixed {fixed1 :: (Fixed Word32),
                            fixed2 :: (Signed (Fixed Int32)),
                            fixed3 :: (Fixed Word64),
                            fixed4 :: (Signed (Fixed Int64))}
                            deriving (Show, Generic, Eq)
instance HasEncoding WithFixed
instance HasDecoding WithFixed

instance Arbitrary a => Arbitrary (Fixed a) where
  arbitrary = Fixed <$> arbitrary

instance Arbitrary a => Arbitrary (Signed a) where
  arbitrary = Signed <$> arbitrary

instance Arbitrary WithFixed where
  arbitrary = WithFixed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data WithBytes = WithBytes {bytes1 :: B.ByteString,
                            bytes2 :: UnpackedVec B.ByteString}
                            deriving (Show, Generic, Eq)
instance HasEncoding WithBytes
instance HasDecoding WithBytes

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary WithBytes where
  arbitrary = WithBytes <$> arbitrary <*> arbitrary

data WithPacking = WithPacking {packing1 :: UnpackedVec Int32,
                                packing2 :: PackedVec Int32}
                                deriving (Show, Generic, Eq)
instance HasEncoding WithPacking
instance HasDecoding WithPacking

instance Arbitrary WithPacking where
  arbitrary = WithPacking <$> arbitrary <*> arbitrary

data AllPackedTypes =
  AllPackedTypes {packedWord32 :: PackedVec Word32,
                  packedWord64 :: PackedVec Word64,
                  packedInt32 :: PackedVec Int32,
                  packedInt64 :: PackedVec Int64,
                  packedFixed32 :: PackedVec (Fixed Word32),
                  packedFixed64 :: PackedVec (Fixed Word64),
                  packedFloat :: PackedVec Float,
                  packedDouble :: PackedVec Double,
                  packedSFixed32 :: PackedVec (Signed (Fixed Int32)),
                  packedSFixed64 :: PackedVec (Signed (Fixed Int64))}
                  deriving (Show, Generic, Eq)
instance HasEncoding AllPackedTypes
instance HasDecoding AllPackedTypes

instance Arbitrary AllPackedTypes where
  arbitrary = AllPackedTypes <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary

data SignedInts =
  SignedInts {signed32 :: Signed Int32,
              signed64 :: Signed Int64}
              deriving (Show, Generic, Eq)
instance HasEncoding SignedInts
instance HasDecoding SignedInts

instance Arbitrary SignedInts where
  arbitrary = SignedInts <$> arbitrary <*> arbitrary

data WithNestingRepeated =
  WithNestingRepeated {nestedMessages :: NestedVec NestedMsg}
    deriving (Show, Eq, Generic)
instance HasEncoding WithNestingRepeated
instance HasDecoding WithNestingRepeated

instance Arbitrary WithNestingRepeated where
  arbitrary = WithNestingRepeated <$> arbitrary

data WithMaybe =
  WithMaybe {maybeMsg :: Maybe Int32}
  deriving (Show, Generic, Eq)
instance HasEncoding WithMaybe
instance HasDecoding WithMaybe

instance Arbitrary WithMaybe where
  arbitrary = WithMaybe <$> arbitrary

data WithNestingMaybe =
  WithNestingMaybe {nestedMaybe :: Maybe (Nested NestedMsg)}
  deriving (Show, Generic, Eq)
instance HasEncoding WithNestingMaybe
--instance HasDecoding WithNestingMaybe

instance Arbitrary WithNestingMaybe where
  arbitrary = WithNestingMaybe <$> arbitrary
