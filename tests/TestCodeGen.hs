{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TestCodeGen where

import           ArbitraryGeneratedTestTypes    ()
import           Control.Applicative
import           Control.Monad
#ifdef SWAGGER
import qualified Data.Aeson
#endif
import qualified Data.ByteString.Lazy           as LBS
import           Data.Proxy                     (Proxy(..))
import           Data.String                    (IsString)
#ifdef SWAGGER
import           Data.Swagger                   (ToSchema)
import qualified Data.Swagger
#endif
import qualified Data.Text                      as T
import           Data.Typeable                  (Typeable, typeRep,
#ifdef SWAGGER
                                                 splitTyConApp, tyConName
#endif
                                                )
#ifdef SWAGGER
import           GHC.Stack                      (HasCallStack)
#endif
import           Google.Protobuf.Timestamp      (Timestamp(..))
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.Class             (def)
import           Proto3.Suite.DotProto.Generate
import           Proto3.Suite.DotProto          (fieldLikeName, prefixedEnumFieldName, typeLikeName)
import           Proto3.Suite.Haskell.Parser    (Logger)
import           Proto3.Suite.JSONPB            (FromJSONPB (..), Options (..),
                                                 ToJSONPB (..), defaultOptions,
                                                 eitherDecode, encode,
                                                 jsonPBOptions)
import           Proto3.Suite.Types             (Enumerated(..))
import           System.Exit
import           Test.Proto.ToEncoder           (Iterator, Stripping)
import           Test.Tasty
import           Test.Tasty.HUnit               (testCase, (@?=))
import           Test.Tasty.QuickCheck          (Arbitrary, (===), testProperty)
import qualified Turtle
import qualified Turtle.Format                  as F
import qualified TestProto
import qualified TestProtoOneof
#ifdef SWAGGER
import qualified TestProtoWrappers
#endif

codeGenTests :: Logger -> TestTree
codeGenTests logger = testGroup "Code generator unit tests"
  [ jsonpbTests
  , hasDefaultTests
  , pascalCaseMessageNames
  , camelCaseMessageFieldNames
  , don'tAlterEnumFieldNames
  , knownTypeMessages
  , pythonInteroperation logger
#ifdef SWAGGER
  , swaggerTests
  , swaggerWrapperFormat
#endif
  ]

pythonInteroperation :: Logger -> TestTree
pythonInteroperation logger = testGroup "Python interoperation" $ do
#ifdef LARGE_RECORDS
  recStyle <- [RegularRecords, LargeRecords]
#else
  recStyle <- [RegularRecords]
#endif
  tt <- ["Data.Text.Lazy.Text", "Data.Text.Text", "Data.Text.Short.ShortText"]
  format <- ["Binary", "Jsonpb"]
  testEncode <- [True, False]
  direct <- [False, True]
  guard $ not direct || (testEncode && format == "Binary")
  let f = if testEncode then simpleEncodeDotProto direct else simpleDecodeDotProto
  pure @[] (f logger recStyle tt format)

#ifdef SWAGGER
swaggerWrapperFormat :: TestTree
swaggerWrapperFormat = testGroup "Swagger Wrapper Format"
    [ expectSchema @TestProtoWrappers.TestDoubleValue
           "{\"properties\":{\"wrapper\":{\"format\":\"DoubleValue\",\"type\":\"number\"},\"many\":{\"items\":{\"format\":\"double\",\"type\":\"number\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestDoubleValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"double\",\"type\":\"number\"},\"many\":{\"items\":{\"format\":\"double\",\"type\":\"number\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestDoubleValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestFloatValue
           "{\"properties\":{\"wrapper\":{\"format\":\"FloatValue\",\"type\":\"number\"},\"many\":{\"items\":{\"format\":\"float\",\"type\":\"number\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestFloatValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"float\",\"type\":\"number\"},\"many\":{\"items\":{\"format\":\"float\",\"type\":\"number\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestFloatValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestInt64Value
           "{\"properties\":{\"wrapper\":{\"format\":\"Int64Value\",\"maximum\":9223372036854775807,\"minimum\":-9223372036854775808,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int64\",\"maximum\":9223372036854775807,\"minimum\":-9223372036854775808,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestInt64ValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"int64\",\"maximum\":9223372036854775807,\"minimum\":-9223372036854775808,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int64\",\"maximum\":9223372036854775807,\"minimum\":-9223372036854775808,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestInt64ValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestUInt64Value
           "{\"properties\":{\"wrapper\":{\"format\":\"UInt64Value\",\"maximum\":18446744073709551615,\"minimum\":0,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int64\",\"maximum\":18446744073709551615,\"minimum\":0,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestUInt64ValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"int64\",\"maximum\":18446744073709551615,\"minimum\":0,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int64\",\"maximum\":18446744073709551615,\"minimum\":0,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestUInt64ValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestInt32Value
           "{\"properties\":{\"wrapper\":{\"format\":\"Int32Value\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int32\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestInt32ValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"int32\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int32\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestInt32ValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestUInt32Value
           "{\"properties\":{\"wrapper\":{\"format\":\"UInt32Value\",\"maximum\":4294967295,\"minimum\":0,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int32\",\"maximum\":4294967295,\"minimum\":0,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestUInt32ValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"int32\",\"maximum\":4294967295,\"minimum\":0,\"type\":\"integer\"},\"many\":{\"items\":{\"format\":\"int32\",\"maximum\":4294967295,\"minimum\":0,\"type\":\"integer\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestUInt32ValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestBoolValue
           "{\"properties\":{\"wrapper\":{\"format\":\"BoolValue\",\"type\":\"boolean\"},\"many\":{\"items\":{\"type\":\"boolean\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestBoolValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"type\":\"boolean\"},\"many\":{\"items\":{\"type\":\"boolean\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestBoolValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestStringValue
           "{\"properties\":{\"wrapper\":{\"format\":\"StringValue\",\"type\":\"string\"},\"many\":{\"items\":{\"type\":\"string\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestStringValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"type\":\"string\"},\"many\":{\"items\":{\"type\":\"string\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestStringValuePickOne\"}},\"type\":\"object\"}"
    , expectSchema @TestProtoWrappers.TestBytesValue
           "{\"properties\":{\"wrapper\":{\"format\":\"BytesValue\",\"type\":\"string\"},\"many\":{\"items\":{\"format\":\"byte\",\"type\":\"string\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestBytesValuePickOne\"}},\"type\":\"object\"}"
           "{\"properties\":{\"wrapper\":{\"format\":\"byte\",\"type\":\"string\"},\"many\":{\"items\":{\"format\":\"byte\",\"type\":\"string\"},\"type\":\"array\"},\"pickOne\":{\"$ref\":\"#/definitions/TestBytesValuePickOne\"}},\"type\":\"object\"}"
    ]
  where
    expectSchema ::
      forall a .
      (ToSchema a, Typeable a, HasCallStack) =>
      LBS.ByteString ->
      LBS.ByteString ->
      TestTree
    expectSchema wrapperFormat noWrapperFormat =
        testCase (tyConName (fst (splitTyConApp (typeRep (Proxy @a))))) $ do
          lbsSchemaOf @a @?= (if wf then wrapperFormat else noWrapperFormat)
      where
        wf :: Bool
#ifdef SWAGGER_WRAPPER_FORMAT
        wf = True
#else
        wf = False
#endif
#endif

knownTypeMessages :: TestTree
knownTypeMessages =
  testGroup
    "KnownType custom codec"
    [ testCase "Timestamp rfc3339 json encoding"
        $ encode defaultOptions (Timestamp 0 0) @?= "\"1970-01-01T00:00:00Z\""
    , testCase "Timestamp rfc3339 json decoding"
        $ eitherDecode "\"1970-01-01T00:00:00Z\"" @?= Right (Timestamp 0 0)
    ]

pascalCaseMessageNames :: TestTree
pascalCaseMessageNames = testGroup "PascalCasing of message names"
  [ testCase "Capitalizes letters after underscores"
      $ typeLikeName "protocol_analysis" @?= Right "ProtocolAnalysis"

  , testCase "Preserves casing of interior letters"
      $ typeLikeName "analyze_HTTP" @?= Right "AnalyzeHTTP"

  , testCase "Handles non-alphanumeric characters after underscore"
      $ typeLikeName "analyze_http_2" @?= Right "AnalyzeHttp2"

  , testCase "Preserves one underscore in double underscore sequence"
      $ typeLikeName "Analyze__HTTP" @?= Right "Analyze_HTTP"

  , testCase "Handles names prefixed with underscore"
      $ typeLikeName "_message_name" @?= Right "XMessageName"

  , testCase "Preserves trailing underscore"
      $ typeLikeName "message_name_" @?= Right "MessageName_"
  ]

camelCaseMessageFieldNames :: TestTree
camelCaseMessageFieldNames = testGroup "camelCasing of field names"
  [ testCase "Preserves capitalization patterns"
      $ fieldLikeName "IP" @?= "ip"

  , testCase "Preserves underscores"
      $ fieldLikeName "IP_address" @?= "ip_address"
  ]

don'tAlterEnumFieldNames :: TestTree
don'tAlterEnumFieldNames
  = testGroup "Do not alter enumeration field names"
  $ tc <$> [ "fnord"
           , "FNORD"
           , "PascalCase"
           , "camelCase"
           , "VOCIFEROUS_SNAKE_CASE"
           , "snake_case"
           , "snake_case_"
           ]
  where
    enumName     = "MyEnum"
    tc fieldName = testCase fieldName $
        prefixedEnumFieldName enumName fieldName @?= (enumName <> fieldName)

setPythonPath :: IO ()
setPythonPath = Turtle.export "PYTHONPATH" .
  maybe pyTmpDir (\p -> pyTmpDir <> ":" <> p) =<< Turtle.need "PYTHONPATH"

simpleEncodeDotProto :: Bool -> Logger -> RecordStyle -> String -> T.Text -> TestTree
simpleEncodeDotProto direct logger recStyle chosenStringType format =
    testCase ("generate code for a simple .proto and then use it to encode messages" ++
              " with string type " ++ chosenStringType ++ " in format " ++ show format ++
              ", record style " ++ show recStyle ++
              (if direct then ", direct mode" else ", intermediate mode"))
    $ do
         decodedStringType <- either die pure (parseStringType chosenStringType)

         compileTestDotProtos logger recStyle decodedStringType direct
         -- Compile our generated encoder
         let encodeCmd = "tests/encode.sh " <> hsTmpDir
                           <> (if direct then " -DTYPE_LEVEL_FORMAT" else "")
#if DHALL
                           <> " -DDHALL"
#endif
         Turtle.shell encodeCmd empty >>= (@?= ExitSuccess)

         -- The python test of encoding exits with a special error code to indicate
         -- all tests were successful.  When directly encoding without an intermediate
         -- data structure, we run the test several times (still compiling just once)
         -- in order to cover various supported ways of iterating over repeated fields.
         setPythonPath
         let iterators :: [Iterator]
             iterators
               | direct = [minBound .. maxBound]
               | otherwise = [minBound]  -- Just an unused placeholder
             strippings :: [Stripping]
             strippings
               | direct = [minBound .. maxBound]
               | otherwise = [minBound]  -- Just an unused placeholder
         forM_ iterators $ \(iterator :: Iterator) -> do
           forM_ strippings $ \(stripping :: Stripping) -> do
             when direct $ do
               putStrLn $ "        iterator: " ++ show iterator
               putStrLn $ "        stripping: " ++ show stripping
             let cmd = hsTmpDir <> "/simpleEncodeDotProto " <> format <>
                       " " <> T.pack (show iterator) <> " " <> T.pack (show stripping) <>
                       " | python tests/check_simple_dot_proto.py " <> format
             Turtle.shell cmd empty >>= (@?= ExitFailure 12)

         -- Not using bracket so that we can inspect the output to fix the tests
         Turtle.rmtree hsTmpDir
         Turtle.rmtree pyTmpDir

simpleDecodeDotProto :: Logger -> RecordStyle -> String -> T.Text -> TestTree
simpleDecodeDotProto logger recStyle chosenStringType format =
    testCase ("generate code for a simple .proto and then use it to decode messages" ++
              " with string type " ++ chosenStringType ++ " in format " ++ show format ++
              ", record style " ++ show recStyle)
    $ do
         decodedStringType <- either die pure (parseStringType chosenStringType)

         compileTestDotProtos logger recStyle decodedStringType False
         -- Compile our generated decoder
         let decodeCmd = "tests/decode.sh " <> hsTmpDir
#if DHALL
                           <> " -DDHALL"
#endif
         Turtle.shell decodeCmd empty >>= (@?= ExitSuccess)

         setPythonPath
         let cmd = "python tests/send_simple_dot_proto.py " <> format <> " | FORMAT=" <> format <> " " <> hsTmpDir <> "/simpleDecodeDotProto "
         Turtle.shell cmd empty >>= (@?= ExitSuccess)

         -- Not using bracket so that we can inspect the output to fix the tests
         Turtle.rmtree hsTmpDir
         Turtle.rmtree pyTmpDir

-- * Helpers

hsTmpDir, pyTmpDir :: IsString a => a
hsTmpDir = "test-files/hs-tmp"
pyTmpDir = "test-files/py-tmp"

defaultStringType :: StringType
defaultStringType = StringType "Data.Text.Lazy" "Text"

compileTestDotProtos :: Logger -> RecordStyle -> StringType -> Bool -> IO ()
compileTestDotProtos logger recStyle decodedStringType typeLevel = do
  Turtle.mktree hsTmpDir
  Turtle.mktree pyTmpDir
  let protoFiles :: [Turtle.FilePath]
      protoFiles =
        [ "test_proto.proto"
        , "test_proto_import.proto"
        , "test_proto_negative_enum.proto"
        , "test_proto_oneof.proto"
        , "test_proto_oneof_import.proto"
        {- These tests have been temporarily removed to pass CI.
        , "test_proto_leading_dot.proto"
        , "test_proto_protoc_plugin.proto"
        -}
        , "test_proto_nested_message.proto"
        , "test_proto_wrappers.proto"
        , "test_proto_negative_enum.proto"
        ]

  forM_ protoFiles $ \protoFile -> do
    compileDotProtoFileOrDie logger
        CompileArgs{ includeDir = ["test-files"]
                   , extraInstanceFiles = ["test-files" Turtle.</> "Orphan.hs"]
                   , outputDir = hsTmpDir
                   , inputProto = protoFile
                   , stringType = decodedStringType
                   , recordStyle = recStyle
                   , typeLevelFormat = typeLevel
                   }

    let cmd = T.concat [ "protoc --python_out="
                       , pyTmpDir
                       , " --proto_path=test-files"
                       , " test-files/" <> Turtle.format F.fp protoFile
                       ]
    Turtle.shell cmd empty >>= (@?= ExitSuccess)

  Turtle.touch (pyTmpDir Turtle.</> "__init__.py")

jsonpbTests :: TestTree
jsonpbTests = testGroup "JSONPB tests"
  [ testGroup "Round-trip tests"
      [ roundTripTest @TestProto.Trivial
      , roundTripTest @TestProto.MultipleFields
      , roundTripTest @TestProto.SignedInts
      , testProperty "roundTrip (SignedInts minBound minBound)" $
          roundTrip (TestProto.SignedInts minBound minBound)
      , testProperty "roundTrip (SignedInts maxBound maxBound)" $
          roundTrip (TestProto.SignedInts maxBound maxBound)
      , testProperty "roundTrip . WithEnum . Enumerated . Right" $
          roundTrip . TestProto.WithEnum . Enumerated . Right
      , roundTripTest @TestProto.WithNesting
      , roundTripTest @TestProto.WithNestingRepeated
      , roundTripTest @TestProto.WithNestingRepeatedInts
      , roundTripTest @TestProto.WithBytes
      , roundTripTest @TestProto.OutOfOrderFields
      , roundTripTest @TestProto.UsingImported
      , roundTripTest @TestProto.Wrapped
      , roundTripTest @TestProtoOneof.Something
      , roundTripTest @TestProtoOneof.WithImported
      ]
  , testGroup "Specific encoding tests" $
      let jsonPB = jsonPBOptions
          json = defaultOptions
      in
      [ encodesAs jsonPB (TestProto.MultipleFields 0 0 0 0 "" False)                                "{}"
      , encodesAs json   (TestProto.MultipleFields 0 2.0 0 0 "" True)                               "{\"multiFieldDouble\":0.0,\"multiFieldFloat\":2.0,\"multiFieldInt32\":0,\"multiFieldInt64\":\"0\",\"multiFieldString\":\"\",\"multiFieldBool\":true}"
      , encodesAs jsonPB (TestProto.SignedInts minBound minBound)
          "{\"signed32\":-2147483648,\"signed64\":\"-9223372036854775808\"}"
      , encodesAs jsonPB (TestProto.SignedInts maxBound maxBound)
          "{\"signed32\":2147483647,\"signed64\":\"9223372036854775807\"}"
      , encodesAs jsonPB (TestProto.WithEnum (Enumerated (Right TestProto.WithEnum_TestEnumENUM1)))
          "{}"
      , encodesAs json   (TestProto.WithEnum (Enumerated (Right TestProto.WithEnum_TestEnumENUM1)))
          "{\"enumField\":\"ENUM1\"}"
      , encodesAs jsonPB (TestProto.WithEnum (Enumerated (Right TestProto.WithEnum_TestEnumENUM3)))
          "{\"enumField\":\"ENUM3\"}"
      , encodesAs jsonPB (TestProto.WithNesting $ Just $ TestProto.WithNesting_Nested "" 0 [1,2] [66,99])
          "{\"nestedMessage\":{\"nestedPacked\":[1,2],\"nestedUnpacked\":[66,99]}}"
      , encodesAs jsonPB (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneName "")))
          "{\"value\":\"42\",\"another\":99,\"name\":\"\"}"
      , encodesAs jsonPB (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneSomeid 0)))
          "{\"value\":\"42\",\"another\":99,\"someid\":0}"
      , encodesAs jsonPB (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg1 (TestProtoOneof.DummyMsg 66))))
          "{\"value\":\"42\",\"another\":99,\"dummyMsg1\":{\"dummy\":66}}"
      , encodesAs jsonPB (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg2 (TestProtoOneof.DummyMsg 67))))
          "{\"value\":\"42\",\"another\":99,\"dummyMsg2\":{\"dummy\":67}}"
      , encodesAs jsonPB (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyEnum (Enumerated (Right TestProtoOneof.DummyEnumDUMMY0)))))
          "{\"value\":\"42\",\"another\":99,\"dummyEnum\":\"DUMMY0\"}"
      , encodesAs jsonPB (TestProtoOneof.Something 42 99 Nothing)
          "{\"value\":\"42\",\"another\":99}"
      , encodesAs json   (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneName "")))
          "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"name\":\"\"}}"
      , encodesAs json   (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneSomeid 0)))
          "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"someid\":0}}"
      , encodesAs json   (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg1 (TestProtoOneof.DummyMsg 66))))
          "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"dummyMsg1\":{\"dummy\":66}}}"
      , encodesAs json   (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg2 (TestProtoOneof.DummyMsg 67))))
          "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"dummyMsg2\":{\"dummy\":67}}}"
      , encodesAs json   (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyEnum (Enumerated (Right TestProtoOneof.DummyEnumDUMMY0)))))
          "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"dummyEnum\":\"DUMMY0\"}}"
      , encodesAs json   (TestProtoOneof.Something 42 99 Nothing)
          "{\"value\":\"42\",\"another\":99,\"pickOne\":null}"
      ]
  , testGroup "Specific decoding tests" $
      [ decodesAs "{\"signed32\":2147483647,\"signed64\":\"9223372036854775807\"}"
          (TestProto.SignedInts 2147483647 9223372036854775807)
      , decodesAs "{\"enumField\":\"ENUM3\"}"                                             (TestProto.WithEnum (Enumerated (Right TestProto.WithEnum_TestEnumENUM3)))
      , decodesAs "{\"enumField\":null}"
          (TestProto.WithEnum (Enumerated (Right TestProto.WithEnum_TestEnumENUM1)))
      , decodesAs "{}"
          (TestProto.WithEnum (Enumerated (Right TestProto.WithEnum_TestEnumENUM1)))
      , decodesAs "{\"nestedMessage\":{}}"
          (TestProto.WithNesting $ Just $ TestProto.WithNesting_Nested "" 0 [] [])
      , testGroup "JSONPB"
          [ decodesAs "{\"value\":\"42\",\"another\":99,\"someid\":66}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneSomeid 66)))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"name\":\"foo\"}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneName "foo")))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"dummyMsg1\":{\"dummy\":41}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg1 (TestProtoOneof.DummyMsg 41))))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"dummyMsg2\":{\"dummy\":43}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg2 (TestProtoOneof.DummyMsg 43))))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"dummyEnum\":\"DUMMY0\"}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyEnum (Enumerated (Right TestProtoOneof.DummyEnumDUMMY0)))))
          , decodesAs "{\"value\":\"42\",\"another\":99}"
              (TestProtoOneof.Something 42 99 Nothing)
          ]
      , testGroup "JSON"
          [ decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"name\":\"\"}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneName "")))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"someid\":0}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneSomeid 0)))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"dummyMsg1\":{\"dummy\":66}}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg1 (TestProtoOneof.DummyMsg 66))))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"dummyMsg2\":{\"dummy\":67}}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyMsg2 (TestProtoOneof.DummyMsg 67))))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":{\"dummyEnum\":\"DUMMY0\"}}"
              (TestProtoOneof.Something 42 99 (Just (TestProtoOneof.SomethingPickOneDummyEnum (Enumerated (Right TestProtoOneof.DummyEnumDUMMY0)))))
          , decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":{}}"                      (TestProtoOneof.Something 42 99 Nothing)
          , decodesAs "{\"value\":\"42\",\"another\":99,\"pickOne\":null}"                    (TestProtoOneof.Something 42 99 Nothing)
          ]
      ]
  ]

#ifdef SWAGGER
swaggerTests :: TestTree
swaggerTests = testGroup "Swagger tests"
  [ schemaOf @TestProtoOneof.Something
      "{\"properties\":{\"value\":{\"format\":\"int64\",\"maximum\":9223372036854775807,\"minimum\":-9223372036854775808,\"type\":\"integer\"},\"another\":{\"format\":\"int32\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"},\"pickOne\":{\"$ref\":\"#/definitions/SomethingPickOne\"}},\"type\":\"object\"}"
  , schemaOf @TestProtoOneof.SomethingPickOne
      "{\"properties\":{\"name\":{\"type\":\"string\"},\"someid\":{\"format\":\"int32\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"},\"dummyMsg1\":{\"$ref\":\"#/definitions/DummyMsg\"},\"dummyMsg2\":{\"$ref\":\"#/definitions/DummyMsg\"},\"dummyEnum\":{\"$ref\":\"#/definitions/DummyEnum\"}},\"maxProperties\":1,\"minProperties\":1,\"type\":\"object\"}"
  , schemaOf @TestProtoOneof.DummyMsg
      "{\"properties\":{\"dummy\":{\"format\":\"int32\",\"maximum\":2147483647,\"minimum\":-2147483648,\"type\":\"integer\"}},\"type\":\"object\"}"
  , schemaOf @(Enumerated TestProtoOneof.DummyEnum)
      "{\"enum\":[\"DUMMY0\",\"DUMMY1\"],\"type\":\"string\"}"

  ]
#endif

hasDefaultTests :: TestTree
hasDefaultTests = testGroup "Generic HasDefault"
  [ testProperty "MultipleFields" $
      (def :: TestProto.MultipleFields) ===
        TestProto.MultipleFields {multipleFieldsMultiFieldDouble = 0.0, multipleFieldsMultiFieldFloat = 0.0, multipleFieldsMultiFieldInt32 = 0, multipleFieldsMultiFieldInt64 = 0, multipleFieldsMultiFieldString = "", multipleFieldsMultiFieldBool = False}
  , testProperty "WithNesting" $
      (def :: TestProto.WithNesting) ===
        TestProto.WithNesting {withNestingNestedMessage = Nothing}
  , testProperty "WithNestingRepeated" $
      (def :: TestProto.WithNestingRepeated) ===
        TestProto.WithNestingRepeated {withNestingRepeatedNestedMessages = []}
  , testProperty "WithEnum" $
      (def :: TestProto.WithEnum) ===
        TestProto.WithEnum {withEnumEnumField = Enumerated {enumerated = Right TestProto.WithEnum_TestEnumENUM1}}
  ]

-- * Helper quickcheck props

roundTripTest ::
  forall a .
  (ToJSONPB a, FromJSONPB a, Eq a, Arbitrary a, Show a, Typeable a) =>
  TestTree
roundTripTest =
  testProperty ("roundTripTest @" ++ show (typeRep (Proxy :: Proxy a))) $
    roundTrip @a

roundTrip :: (ToJSONPB a, FromJSONPB a, Eq a)
          => a -> Bool
roundTrip x = roundTrip' False && roundTrip' True
  where
    roundTrip' emitDefaults =
      eitherDecode (encode defaultOptions{ optEmitDefaultValuedFields = emitDefaults} x)
      ==
      Right x

encodesAs ::
  forall a .
  (ToJSONPB a, Eq a, Show a, Typeable a) =>
  Options -> a -> LBS.ByteString -> TestTree
encodesAs opts x bs = testProperty (testName "") (encode opts x === bs)
  where
    testName =
      showString "encode @" .
      showsPrec 11 (typeRep (Proxy :: Proxy a)) .
      showChar ' ' .
      showsPrec 11 opts .
      showChar ' ' .
      showsPrec 11 x .
      showString " == " .
      showsPrec 5 bs

decodesAs ::
  forall a .
  (FromJSONPB a, Eq a, Show a, Typeable a) =>
  LBS.ByteString -> a -> TestTree
decodesAs bs x = testProperty (testName "")  (eitherDecode bs === Right x)
  where
    testName =
      showString "eitherDecode @" .
      showsPrec 11 (typeRep (Proxy :: Proxy a)) .
      showChar ' ' .
      showsPrec 11 bs .
      showString " == Right " .
      showsPrec 11 x

#ifdef SWAGGER
schemaOf ::
  forall a .
  (ToSchema a, Eq a, Show a, Typeable a) =>
  LBS.ByteString -> TestTree
schemaOf bs = testProperty (testName "") (lbsSchemaOf @a === bs)
  where
    testName =
      showString "lbsSchemaOf @" .
      showsPrec 11 (typeRep (Proxy :: Proxy a)) .
      showString " == " .
      showsPrec 5 bs

lbsSchemaOf :: forall a . ToSchema a => LBS.ByteString
lbsSchemaOf = Data.Aeson.encode (Data.Swagger.toSchema (Proxy @a))
#endif
