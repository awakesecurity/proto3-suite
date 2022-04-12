{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryGeneratedTestTypes where

import qualified Data.Text.Lazy        as T
import qualified Proto3.Suite.Types as DotProto
import qualified Test.QuickCheck       as QC
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, Arbitrary (arbitrary))
import qualified TestProtoOneof
import qualified TestProtoOneofImport

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
      [ TestProtoOneof.SomethingPickOneName       <$> fmap T.pack arbitrary
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
      [ TestProtoOneofImport.WithOneofPickOneA <$> fmap T.pack arbitrary
      , TestProtoOneofImport.WithOneofPickOneB <$> arbitrary
      ]
