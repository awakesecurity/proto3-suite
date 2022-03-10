{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- |
--
module Test.Proto.Generate.Name (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (MonadTest, Property, annotate, forAll, property, (===))
import Hedgehog.Range qualified as Range

import Test.Proto.Generate.Name.Gen (GenName)
import Test.Proto.Generate.Name.Gen qualified as Name.Gen

import Proto3.Suite.DotProto.Generate

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Test.Proto.Generate.Name"
    [ testProperty "filenames" resolve'protofile
    ]

-- | Testing combinator for name resolution functions.
testResolution ::
  (MonadTest m, Applicative f, Eq (f String), Show (f String)) =>
  (String -> f String) -> GenName -> m ()
testResolution resolve nm = do
  let occ = Name.Gen.nameOcc nm
  let res = Name.Gen.nameRes nm
  let got = resolve occ

  annotate ("protobuf name: " ++ occ)
  annotate ("expected name: " ++ res)
  annotate ("resolved name: " ++ show got)

  pure res === got

-- -----------------------------------------------------------------------------
--
-- Name Resolution Tests
--

resolve'protofile :: Property
resolve'protofile = property do
  nm <- forAll (Name.Gen.protofile (Range.linear 1 10))
  testResolution (renameProtoFile @(Either CompileError)) nm
