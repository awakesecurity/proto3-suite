{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryGeneratedTestTypes where

import           Data.String           (fromString)
import qualified Data.Text.Short       as TS
import qualified Data.Vector           as V
import qualified Proto3.Suite.Types as DotProto
import           Test.QuickCheck       (listOf)
import qualified Test.QuickCheck       as QC
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, Arbitrary (..))
import           TestProto
import qualified TestProtoImport
import qualified TestProtoOneof
import qualified TestProtoOneofImport

instance Arbitrary TS.ShortText where
  arbitrary = fmap TS.fromText arbitrary
  shrink = map TS.fromText . shrink . TS.toText

instance Arbitrary Trivial where
  arbitrary = Trivial <$> arbitrary

instance Arbitrary MultipleFields where
  arbitrary =
    MultipleFields
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> fmap fromString arbitrary
    <*> arbitrary

instance Arbitrary WithEnum_TestEnum where
  arbitrary = genericArbitrary

instance Arbitrary WithEnum where
  arbitrary = WithEnum <$> arbitrary

instance Arbitrary WithNesting_Nested where
  arbitrary =
    WithNesting_Nested
    <$> fmap fromString arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary WithNesting where
  arbitrary = WithNesting <$> arbitrary

instance Arbitrary WithRepetition where
  arbitrary = WithRepetition <$> arbitrary

instance Arbitrary WithRepeatedSigned where
  arbitrary = WithRepeatedSigned <$> arbitrary <*> arbitrary

instance Arbitrary WithFixed where
  arbitrary = WithFixed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary WithBytes where
  arbitrary = WithBytes <$> arbitrary <*> arbitrary

instance Arbitrary AllPackedTypes where
  arbitrary = do
    AllPackedTypes
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbEnums  <*> arbEnums
    where
      arbEnums = V.fromList <$> listOf (DotProto.Enumerated . Right <$> genericArbitrary)

instance Arbitrary SignedInts where
  arbitrary = SignedInts <$> arbitrary <*> arbitrary

instance Arbitrary WithNestingRepeated where
  arbitrary = WithNestingRepeated <$> arbitrary
  shrink (WithNestingRepeated xs) = map (WithNestingRepeated . V.fromList) $
    QC.shrinkList (const []) (V.toList xs)
      -- It is too expensive to shrink the submessages.

instance Arbitrary WithNestingRepeated_Nested where
  arbitrary =
    WithNestingRepeated_Nested
    <$> fmap fromString arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary WithNestingRepeatedInts where
  arbitrary = WithNestingRepeatedInts <$> arbitrary

instance Arbitrary NestedInts where
  arbitrary = NestedInts <$> arbitrary <*> arbitrary

instance Arbitrary OutOfOrderFields where
  arbitrary =
    OutOfOrderFields
    <$> arbitrary
    <*> fmap fromString arbitrary
    <*> arbitrary
    <*> fmap (fmap fromString) arbitrary

instance Arbitrary UsingImported where
  arbitrary =
    UsingImported
    <$> arbitrary
    <*> arbitrary

instance Arbitrary TestProtoImport.WithNesting where
  arbitrary =
    TestProtoImport.WithNesting
    <$> arbitrary
    <*> arbitrary

instance Arbitrary TestProtoImport.WithNesting_Nested where
  arbitrary =
    TestProtoImport.WithNesting_Nested
    <$> arbitrary
    <*> arbitrary

instance Arbitrary Wrapped where
  arbitrary = Wrapped <$> arbitrary

instance Arbitrary TestProtoOneof.DummyMsg where
  arbitrary =
    TestProtoOneof.DummyMsg
    <$> arbitrary

instance Arbitrary TestProtoOneof.Something where
  arbitrary =
    TestProtoOneof.Something
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary TestProtoOneof.SomethingPickOne where
  arbitrary =
    QC.oneof
      [ TestProtoOneof.SomethingPickOneName       <$> fmap fromString arbitrary
      , TestProtoOneof.SomethingPickOneSomeid     <$> arbitrary
      , TestProtoOneof.SomethingPickOneDummyMsg1  <$> arbitrary
      , TestProtoOneof.SomethingPickOneDummyMsg2  <$> arbitrary
      , TestProtoOneof.SomethingPickOneDummyEnum . DotProto.Enumerated . Right
        <$> genericArbitrary
      ]

instance Arbitrary TestProtoOneof.WithImported where
  arbitrary =
    TestProtoOneof.WithImported
    <$> arbitrary

instance Arbitrary TestProtoOneof.WithImportedPickOne where
  arbitrary =
    QC.oneof
      [ TestProtoOneof.WithImportedPickOneDummyMsg1 <$> arbitrary
      , TestProtoOneof.WithImportedPickOneWithOneof <$> arbitrary
      ]

instance Arbitrary TestProtoOneofImport.WithOneof where
  arbitrary =
    TestProtoOneofImport.WithOneof
    <$> arbitrary

instance Arbitrary TestProtoOneofImport.WithOneofPickOne where
  arbitrary =
    QC.oneof
      [ TestProtoOneofImport.WithOneofPickOneA <$> fmap fromString arbitrary
      , TestProtoOneofImport.WithOneofPickOneB <$> arbitrary
      ]
