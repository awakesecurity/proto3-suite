{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- |
--
module Test.Proto.Generate.Name (testTree) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (MonadTest, Property, annotate, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Proto.Generate.Name.Gen (GenName)
import Test.Proto.Generate.Name.Gen qualified as Name.Gen

import Proto3.Suite.DotProto.Generate

-- -----------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test.Proto.Generate.Name"
    [ testProperty "filenames" resolveProtofile
    ]

-- | Testing combinator for name resolution functions.
testResolution ::
  (MonadTest m, Applicative f, Eq (f String), Show (f String)) =>
  (String -> f String) -> 
  GenName -> 
  m ()
testResolution resolve nm = do
  annotate ("protobuf name: " ++ occ)
  annotate ("expected name: " ++ res)
  annotate ("resolved name: " ++ show got)
  pure res === got
  where
    occ :: String
    occ = Name.Gen.nameOcc nm

    res :: String
    res = Name.Gen.nameRes nm

    got = resolve occ

resolveProtofile :: Property
resolveProtofile = property do
  nm <- forAll $ Gen.sized (Name.Gen.protofile . Range.linear 1 . fromIntegral)
  testResolution (renameProtoFile @(Either CompileError)) nm
