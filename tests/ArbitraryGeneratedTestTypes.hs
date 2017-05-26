{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryGeneratedTestTypes where

import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import qualified Data.Text.Arbitrary as T
import qualified Data.Vector         as V
import           GHC.Int
import           Test.QuickCheck     (Arbitrary, Gen, arbitrary, listOf, oneof)

import           GeneratedTestTypes

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> listOf arbitrary

instance Arbitrary Trivial where
  arbitrary = Trivial <$> arbitrary

instance Arbitrary MultipleFields where
  arbitrary = MultipleFields
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> fmap T.pack arbitrary
    <*> arbitrary

instance Arbitrary WithEnum where
  arbitrary = WithEnum <$> arbitrary

instance Arbitrary WithNesting_Nested where
  arbitrary = WithNesting_Nested <$> arbitrary <*> arbitrary

instance Arbitrary WithNesting where
  arbitrary = WithNesting <$> arbitrary

instance Arbitrary WithRepetition where
  arbitrary = WithRepetition <$> arbitrary

instance Arbitrary WithFixed where
  arbitrary = WithFixed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary WithBytes where
  arbitrary = WithBytes <$> arbitrary <*> arbitrary

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary AllPackedTypes where
  arbitrary = AllPackedTypes <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary

instance Arbitrary SignedInts where
  arbitrary = SignedInts <$> arbitrary <*> arbitrary

instance Arbitrary WithNestingRepeated where
  arbitrary = WithNestingRepeated <$> arbitrary

instance Arbitrary WithNestingRepeated_Nested where
  arbitrary = WithNestingRepeated_Nested <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Wrapped where
  arbitrary = Wrapped <$> arbitrary
