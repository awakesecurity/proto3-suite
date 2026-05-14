{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Proto.Generate.CodeGen (testTree) where

import Control.Monad.Except (runExceptT)
import Data.List (isInfixOf)
import Proto3.Suite.DotProto.Generate
  ( CompileError
  , StringType(..)
  , getExtraInstances
  , readDotProtoWithContext
  , renderHsModuleForDotProto
  )
import Proto3.Suite.Haskell.Parser (initLogger)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | Run the code generation pipeline on a .proto file and return the
-- generated Haskell source as a 'String'.
generateModule :: StringType -> Bool -> FilePath -> IO String
generateModule stringType typeLevelFormat protoFile = do
  let ?stringType = stringType
      ?typeLevelFormat = typeLevelFormat
  result <- runExceptT $ do
    (dotProto, typeCtx) <- readDotProtoWithContext ["test-files"] protoFile
    renderHsModuleForDotProto ([], []) dotProto typeCtx
  case result of
    Left err -> assertFailure (show err) >> error "unreachable"
    Right src -> pure src

-- | Generate with default settings (Data.Text.Lazy.Text, typeLevelFormat on).
generateModule' :: FilePath -> IO String
generateModule' = generateModule (StringType "Data.Text.Lazy" "Text") True

-- | Assert that a generated source contains a given substring.
assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack =
  assertBool (label ++ ": expected source to contain " ++ show needle)
             (needle `isInfixOf` haystack)

-- | Assert that a generated source does NOT contain a given substring.
assertNotContains :: String -> String -> String -> IO ()
assertNotContains label needle haystack =
  assertBool (label ++ ": expected source to NOT contain " ++ show needle)
             (not (needle `isInfixOf` haystack))

-- | Assert that a type declaration exists (either @data@ or @newtype@).
assertContainsTypeDecl :: String -> String -> String -> IO ()
assertContainsTypeDecl label typeName src =
  assertBool (label ++ ": expected source to contain data or newtype declaration for " ++ typeName)
             (("data " ++ typeName) `isInfixOf` src ||
              ("newtype " ++ typeName) `isInfixOf` src)

testTree :: TestTree
testTree = testGroup "Code generation"
  [ codeGenSucceeds
  , moduleDeclarations
  , messageTypeDeclarations
  , enumTypeDeclarations
  , instanceDeclarations
  , nestedMessageTypes
  , oneofSumTypes
  , fieldNameTransformation
  , stringTypeParameterization
  , typeLevelFormatFlag
  , extraInstanceParsing
  ]

-- | Code generation succeeds for all test .proto files.
codeGenSucceeds :: TestTree
codeGenSucceeds = testGroup "Code generation succeeds"
  [ testCase proto $ generateModule' proto >> pure ()
  | proto <-
      [ "test_proto.proto"
      , "test_proto_import.proto"
      , "test_proto_oneof.proto"
      , "test_proto_oneof_import.proto"
      , "test_proto_negative_enum.proto"
      , "test_proto_nested_message.proto"
      , "test_proto_wrappers.proto"
      , "test_proto_optional.proto"
      ]
  ]

-- | Generated source contains correct module declarations.
moduleDeclarations :: TestTree
moduleDeclarations = testGroup "Module declarations"
  [ testCase "test_proto.proto -> module TestProto" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "module" "module TestProto" src
  , testCase "test_proto_oneof.proto -> module TestProtoOneof" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContains "module" "module TestProtoOneof" src
  , testCase "test_proto_optional.proto -> module TestProtoOptional" $ do
      src <- generateModule' "test_proto_optional.proto"
      assertContains "module" "module TestProtoOptional" src
  ]

-- | Generated source contains expected type declarations for messages.
-- Single-field messages may use @newtype@ instead of @data@.
messageTypeDeclarations :: TestTree
messageTypeDeclarations = testGroup "Message type declarations"
  [ testCase "Trivial" $ do
      src <- generateModule' "test_proto.proto"
      assertContainsTypeDecl "Trivial" "Trivial" src
  , testCase "MultipleFields" $ do
      src <- generateModule' "test_proto.proto"
      assertContainsTypeDecl "MultipleFields" "MultipleFields" src
  , testCase "WithEnum" $ do
      src <- generateModule' "test_proto.proto"
      assertContainsTypeDecl "WithEnum" "WithEnum" src
  , testCase "WithNesting" $ do
      src <- generateModule' "test_proto.proto"
      assertContainsTypeDecl "WithNesting" "WithNesting" src
  , testCase "MapTest" $ do
      src <- generateModule' "test_proto.proto"
      assertContainsTypeDecl "MapTest" "MapTest" src
  , testCase "Something (oneof)" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContainsTypeDecl "Something" "Something" src
  , testCase "WithOptional" $ do
      src <- generateModule' "test_proto_optional.proto"
      assertContainsTypeDecl "WithOptional" "WithOptional" src
  ]

-- | Generated source contains expected enum type declarations.
enumTypeDeclarations :: TestTree
enumTypeDeclarations = testGroup "Enum type declarations"
  [ testCase "WithEnum_TestEnum" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "TestEnum" "WithEnum_TestEnum" src
  , testCase "NegativeEnum" $ do
      src <- generateModule' "test_proto_negative_enum.proto"
      assertContains "NegativeEnum" "data NegativeEnum" src
  , testCase "DummyEnum" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContains "DummyEnum" "data DummyEnum" src
  ]

-- | Generated source contains required typeclass instances.
-- Note: instances are rendered with qualified names and parens, e.g.
-- @instance (HsProtobuf.Message Trivial)@.
instanceDeclarations :: TestTree
instanceDeclarations = testGroup "Instance declarations"
  [ testCase "Message Trivial" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "Message" "instance (HsProtobuf.Message Trivial)" src
  , testCase "ToJSONPB Trivial" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "ToJSONPB" "instance (HsJSONPB.ToJSONPB Trivial)" src
  , testCase "FromJSONPB Trivial" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "FromJSONPB" "instance (HsJSONPB.FromJSONPB Trivial)" src
  , testCase "Message Something" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContains "Message" "instance (HsProtobuf.Message Something)" src
  , testCase "Message WithOptional" $ do
      src <- generateModule' "test_proto_optional.proto"
      assertContains "Message" "instance (HsProtobuf.Message WithOptional)" src
  ]

-- | Nested messages produce underscore-separated type names.
nestedMessageTypes :: TestTree
nestedMessageTypes = testGroup "Nested message types"
  [ testCase "WithNesting_Nested" $ do
      src <- generateModule' "test_proto.proto"
      assertContainsTypeDecl "nested" "WithNesting_Nested" src
  , testCase "Message WithNesting_Nested" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "instance" "instance (HsProtobuf.Message WithNesting_Nested)" src
  ]

-- | Oneof fields produce sum types with properly named constructors.
oneofSumTypes :: TestTree
oneofSumTypes = testGroup "Oneof sum types"
  [ testCase "SomethingPickOne data type" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContains "pickOne type" "data SomethingPickOne" src
  , testCase "SomethingPickOneName constructor" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContains "name ctor" "SomethingPickOneName" src
  , testCase "SomethingPickOneSomeid constructor" $ do
      src <- generateModule' "test_proto_oneof.proto"
      assertContains "someid ctor" "SomethingPickOneSomeid" src
  ]

-- | Proto snake_case field names are transformed to camelCase.
fieldNameTransformation :: TestTree
fieldNameTransformation = testGroup "Field name transformation"
  [ testCase "multi_field_double -> multiFieldDouble" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "field" "multiFieldDouble" src
  , testCase "trivial_field -> trivialField" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "field" "trivialField" src
  , testCase "nested_message -> nestedMessage" $ do
      src <- generateModule' "test_proto.proto"
      assertContains "field" "nestedMessage" src
  ]

-- | Different string type settings produce different imports.
stringTypeParameterization :: TestTree
stringTypeParameterization = testGroup "String type parameterization"
  [ testCase "Data.Text.Lazy.Text generates lazy text import" $ do
      src <- generateModule (StringType "Data.Text.Lazy" "Text") True "test_proto.proto"
      assertContains "import" "Data.Text.Lazy" src
  , testCase "Data.Text.Text generates strict text import" $ do
      src <- generateModule (StringType "Data.Text" "Text") True "test_proto.proto"
      assertContains "import" "Data.Text" src
  ]

-- | The typeLevelFormat flag controls TypeFamilies extension.
typeLevelFormatFlag :: TestTree
typeLevelFormatFlag = testGroup "Type-level format flag"
  [ testCase "typeLevelFormat=True includes TypeFamilies" $ do
      src <- generateModule (StringType "Data.Text.Lazy" "Text") True "test_proto.proto"
      assertContains "pragma" "TypeFamilies" src
  , testCase "typeLevelFormat=False excludes TypeFamilies" $ do
      src <- generateModule (StringType "Data.Text.Lazy" "Text") False "test_proto.proto"
      assertNotContains "pragma" "TypeFamilies" src
  ]

-- | 'getExtraInstances' includes both regular instance declarations and
-- standalone deriving declarations from extra instance files.
extraInstanceParsing :: TestTree
extraInstanceParsing =
  testCase "getExtraInstances includes standalone deriving declarations" $ do
    logger <- initLogger
    result <- runExceptT $ getExtraInstances logger "test-files/extra_instances_deriving.hs"
    case result of
      Left err -> assertFailure (show err)
      Right (_imports, decls) ->
        -- The fixture contains 4 top-level declarations:
        --   data Foo = Foo          (TyClD, filtered out)
        --   helper = 42             (ValD,  filtered out)
        --   instance Show Foo       (InstD, kept)
        --   deriving instance Eq Foo (DerivD, kept)
        assertEqual "expected 2 declarations (1 instance + 1 standalone deriving)"
          2 (length decls)
