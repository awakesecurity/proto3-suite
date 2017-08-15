{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCodeGen where

import           Control.Applicative
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit               (testCase, (@?=))
import           Turtle                         hiding (err)

import           Proto3.Suite.DotProto.Generate


codeGenTests :: TestTree
codeGenTests = testGroup "Code generator unit tests"
  [ camelCaseMessageNames
  , camelCaseFieldNames
  , simpleEncodeDotProto
  , simpleDecodeDotProto
  ]

camelCaseMessageNames :: TestTree
camelCaseMessageNames = testGroup "CamelCasing of message names"
  [ testCase "Capitalizes letters after underscores" (typeLikeName "protocol_analysis" @?= Right "ProtocolAnalysis")
  , testCase "Preserves casing of interior letters"  (typeLikeName "analyze_HTTP" @?= Right "AnalyzeHTTP")
  , testCase "Handles non-alphanumeric characters after underscore" (typeLikeName "analyze_http_2" @?= Right "AnalyzeHttp2")
  , testCase "Preserves one underscore in double underscore sequence" (typeLikeName "Analyze__HTTP" @?= Right "Analyze_HTTP")
  , testCase "Handles names prefixed with underscore" (typeLikeName "_message_name" @?= Right "XMessageName")
  , testCase "Preserves trailing underscore" (typeLikeName "message_name_" @?= Right "MessageName_") ]


camelCaseFieldNames :: TestTree
camelCaseFieldNames = testGroup "camelCasing of field names"
  [ testCase "Preserves capitalization patterns" (fieldLikeName "IP" @?= "ip")
  , testCase "Preserves underscores"             (fieldLikeName "IP_address" @?= "ip_address") ]

simpleEncodeDotProto :: TestTree
simpleEncodeDotProto =
    testCase "generate code for a simple .proto and then use it to encode messages" $
    do mktree hsTmpDir
       mktree pyTmpDir

       compileTestDotProto

       (@?= ExitSuccess) =<< proc "tests/encode.sh" [hsTmpDir] empty
       (@?= ExitSuccess) =<< shell (T.concat ["protoc --python_out=", pyTmpDir, " --proto_path=test-files", " test-files/test.proto"]) empty
       (@?= ExitSuccess) =<< shell (T.concat ["protoc --python_out=", pyTmpDir, " --proto_path=test-files", " test-files/test_import.proto"]) empty
       touch (pyTmpDir </> "__init__.py")

       m <- need "PYTHONPATH"
       pythonPath <- case m of
           Nothing         -> fail "PYTHONPATH environment variable is not set"
           Just pythonPath -> return pythonPath
       export "PYTHONPATH" (pythonPath <> ":" <> pyTmpDir)

       let cmd = (hsTmpDir <> "/simpleEncodeDotProto | python tests/check_simple_dot_proto.py")
       -- The python test exits with a special error code to indicate all tests
       -- were successful
       (@?= ExitFailure 12) =<< shell cmd empty

       -- Not using bracket so that we can inspect the output to fix the tests
       rmtree hsTmpDir
       rmtree pyTmpDir

simpleDecodeDotProto :: TestTree
simpleDecodeDotProto =
    testCase "generate code for a simple .proto and then use it to decode messages" $
    do mktree hsTmpDir
       mktree pyTmpDir

       compileTestDotProto

       (@?= ExitSuccess) =<< proc "tests/decode.sh" [hsTmpDir] empty
       (@?= ExitSuccess) =<< shell (T.concat ["protoc --python_out=", pyTmpDir, " --proto_path=test-files", " test-files/test.proto"]) empty
       (@?= ExitSuccess) =<< shell (T.concat ["protoc --python_out=", pyTmpDir, " --proto_path=test-files", " test-files/test_import.proto"]) empty
       touch (pyTmpDir </> "__init__.py")

       m <- need "PYTHONPATH"
       pythonPath <- case m of
           Nothing         -> fail "PYTHONPATH environment variable is not set"
           Just pythonPath -> return pythonPath
       export "PYTHONPATH" (pythonPath <> ":" <> pyTmpDir)

       let cmd = "python tests/send_simple_dot_proto.py | " <> hsTmpDir <> "/simpleDecodeDotProto "
       (@?= ExitSuccess) =<< shell cmd empty

       rmtree hsTmpDir
       rmtree pyTmpDir

-- * Helpers

hsTmpDir, pyTmpDir :: IsString a => a
hsTmpDir = "test-files/hs-tmp"
pyTmpDir = "test-files/py-tmp"

compileTestDotProto :: IO ()
compileTestDotProto = do
  readDotProtoWithContext ["test-files"] "test.proto" >>= \case
    Left err -> fail (show err)
    Right (dp, ctxt) ->
      case renderHsModuleForDotProto dp ctxt of
        Left err -> fail ("compileTestDotProto: Error compiling test.proto: " <> show err)
        Right hsSrc -> writeFile (hsTmpDir <> "/GeneratedTestTypes.hs") hsSrc

  readDotProtoWithContext ["test-files"] "test_import.proto" >>= \case
    Left err -> fail (show err)
    Right (dp, ctxt) ->
      case renderHsModuleForDotProto dp ctxt of
        Left err -> fail ("compileTestDotProto: Error compiling test_import.proto: " <> show err)
        Right hsSrc -> writeFile (hsTmpDir <> "/GeneratedImportedTestTypes.hs") hsSrc
