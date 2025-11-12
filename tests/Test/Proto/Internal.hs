
module Test.Proto.Internal (testTree) where 

import Data.List.NonEmpty (NonEmpty(..))

import Proto3.Suite.DotProto.AST (Path(..))
import Proto3.Suite.DotProto.Internal (toModulePath)

import Hedgehog (property, withTests, (===))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Internal"
    [ testModulePath
    ]

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