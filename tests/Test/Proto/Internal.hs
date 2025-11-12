{-# LANGUAGE TypeApplications #-}

module Test.Proto.Internal (testTree) where 

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
-- import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty(..))

import GHC.Exts (IsList (..))

import Proto3.Suite.Class (HasDefault (..))
import Proto3.Suite.JSONPB.Class 
  ( defaultOptions
  , object
  , objectOrNull
  , pairs
  , pairsOrNull
  )
import Proto3.Suite.DotProto.AST (Path(..))
import Proto3.Suite.DotProto.Generate (renameProtoFile)
import Proto3.Suite.DotProto.Internal 
  ( CompileError (..)
  , toModulePath
  )

import Hedgehog (property, withTests, (===))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Internal"
    [ testNullOrObject
    , testPairsOrNull
    , testDefaults 
    , testRenameProtoFile
    , testModulePath
    ]

testNullOrObject :: TestTree 
testNullOrObject = 
  testProperty "nullOrObject" . withTests 1 $ property do 
    object [const []] defaultOptions === Aeson.Object (fromList [])

    objectOrNull [const []] defaultOptions === Aeson.Null

testPairsOrNull :: TestTree 
testPairsOrNull = 
  testProperty "pairsOrNull" . withTests 1 $ property do 
    pairs [const mempty] defaultOptions === Aeson.Encoding.emptyObject_

    pairsOrNull [] defaultOptions === Aeson.Encoding.null_

testDefaults :: TestTree
testDefaults =
  testProperty "defaults" . withTests 1 $ property do
    isDefault (def :: Aeson.Encoding.Encoding) === True

    isDefault (def :: Aeson.Value) === True

testRenameProtoFile :: TestTree
testRenameProtoFile =
  testProperty "renameProtoFile" . withTests 1 $ property do
    renameProtoFile @(Either CompileError) "abc_xyz" === Right "AbcXyz"

    renameProtoFile @(Either CompileError) "abc_1bc" === Left (InvalidModuleName "abc_1bc")

    renameProtoFile @(Either CompileError) "_" === Left (InvalidModuleName "_")

testModulePath :: TestTree
testModulePath = 
  testProperty "toModulePath" . withTests 1 $ property do 
    toModulePath "/absolute/path/fails.proto" === Left "expected include-relative path"

    toModulePath "relative/path/to/file_without_proto_suffix_fails" === Left "expected .proto suffix"

    toModulePath "relative/path/to/file_without_proto_suffix_fails.txt" === Left "expected .proto suffix"

    toModulePath "../foo.proto" === Left "expected include-relative path, but the path started with ../"

    toModulePath "foo..proto" === Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"

    toModulePath "foo/bar/baz..proto" === Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"

    toModulePath "foo.bar../baz.proto" === Left "path contained unexpected .. after canonicalization, please use form x.y.z.proto"

    toModulePath "google/protobuf/timestamp.proto" === Right (Path {components = "Google" :| ["Protobuf","Timestamp"]})

    toModulePath "a/b/c/google.protobuf.timestamp.proto" === Right (Path {components = "A" :| ["B","C","Google","Protobuf","Timestamp"]})

    toModulePath "foo/FiLeName_underscore.and.then.some.dots.proto" === Right (Path {components = "Foo" :| ["FiLeName_underscore","And","Then","Some","Dots"]})