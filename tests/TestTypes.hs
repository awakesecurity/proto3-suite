{-# LANGUAGE DeriveGeneric #-}

module TestTypes where

import qualified Data.ByteString as B
import           Data.Int
import           Data.Protobuf.Wire
import           Data.Protobuf.Wire.Decode.Parser as P
import qualified Data.Text.Lazy as TL
import           Data.Word (Word32, Word64)
import           GHC.Generics
import           GHC.Exts (toList, fromList)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof)

data Trivial = Trivial {trivialField :: Int32}
                deriving (Show, Generic, Eq)
instance Message Trivial

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
instance Message MultipleFields

instance Arbitrary MultipleFields where
  arbitrary = MultipleFields
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> fmap TL.pack arbitrary
              <*> arbitrary

data TestEnum = ENUM1 | ENUM2 | ENUM3
                deriving (Show, Generic, Enum, Eq, Bounded)
instance Named TestEnum --TODO: this shouldn't be needed.

instance Arbitrary TestEnum where
  arbitrary = oneof $ fmap return [ENUM1, ENUM2, ENUM3]

data WithEnum = WithEnum {enumField :: Enumerated (TestEnum)}
                deriving (Show, Generic, Eq)
instance Message WithEnum
instance Named WithEnum

instance Arbitrary WithEnum where
  arbitrary = WithEnum <$> arbitrary

data NestedMsg = NestedMsg {nestedField1 :: TL.Text,
                            nestedField2 :: Int32,
                            nestedPacked :: PackedVec Int32,
                            nestedUnpacked :: UnpackedVec Int32}
                      deriving (Show, Generic, Eq)
instance Message NestedMsg
instance Named NestedMsg

instance Arbitrary NestedMsg where
  arbitrary = NestedMsg <$> fmap TL.pack arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

data WithNesting = WithNesting {nestedMessage :: Nested NestedMsg}
                    deriving (Show, Generic, Eq)
instance Message WithNesting

instance Arbitrary WithNesting where
  arbitrary = WithNesting <$> arbitrary

data WithRepetition = WithRepetition {repeatedField1 :: PackedVec Int32}
                      deriving (Show, Generic, Eq)
instance Message WithRepetition

instance Arbitrary WithRepetition where
  arbitrary = WithRepetition <$> arbitrary

data WithFixed = WithFixed {fixed1 :: (Fixed Word32),
                            fixed2 :: (Signed (Fixed Int32)),
                            fixed3 :: (Fixed Word64),
                            fixed4 :: (Signed (Fixed Int64))}
                            deriving (Show, Generic, Eq)
instance Message WithFixed

instance Arbitrary WithFixed where
  arbitrary = WithFixed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data WithBytes = WithBytes {bytes1 :: B.ByteString,
                            bytes2 :: UnpackedVec B.ByteString}
                            deriving (Show, Generic, Eq)
instance Message WithBytes

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary WithBytes where
  arbitrary = WithBytes <$> arbitrary <*> arbitrary

data WithPacking = WithPacking {packing1 :: UnpackedVec Int32,
                                packing2 :: PackedVec Int32}
                                deriving (Show, Generic, Eq)
instance Message WithPacking

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
instance Message AllPackedTypes

instance Arbitrary AllPackedTypes where
  arbitrary = AllPackedTypes <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary

data SignedInts =
  SignedInts {signed32 :: Signed Int32,
              signed64 :: Signed Int64}
              deriving (Show, Generic, Eq)
instance Message SignedInts

instance Arbitrary SignedInts where
  arbitrary = SignedInts <$> arbitrary <*> arbitrary

data WithNestingRepeated =
  WithNestingRepeated {nestedMessages :: NestedVec NestedMsg}
    deriving (Show, Eq, Generic)
instance Message WithNestingRepeated

instance Arbitrary WithNestingRepeated where
  arbitrary = WithNestingRepeated <$> arbitrary

data WithNestingRepeatedAbsent =
  WithNestingRepeatedAbsent {notRepeated :: Nested NestedMsg}
  deriving (Show, Eq, Generic)
instance Message WithNestingRepeatedAbsent

instance Arbitrary WithNestingRepeatedAbsent where
  arbitrary = WithNestingRepeatedAbsent <$> arbitrary

data NestedInt = NestedInt {nestedInt1 :: Int32, nestedInt2 :: Int32}
  deriving (Show, Eq, Generic)
instance Message NestedInt
instance Named NestedInt

data WithNestingRepeatedInts =
  WithNestingRepeatedInts {nestedInts :: Nested NestedInt}
  deriving (Show, Eq, Generic)
instance Message WithNestingRepeatedInts
