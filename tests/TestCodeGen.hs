{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCodeGen where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.List
import qualified Data.Text as T
import           Data.String (IsString)
import           Test.Tasty
import           Test.Tasty.HUnit (Assertion, (@?=), (@=?), testCase, assertBool)
import           Test.Tasty.QuickCheck (testProperty, (===))
import           Test.QuickCheck (Arbitrary(..), Property, elements)
import           Test.QuickCheck.Monadic (monadicIO)
import           System.Exit
import           Turtle hiding (err)

import           Proto3.Suite.DotProto
import           Proto3.Suite.DotProto.Generate

import Proto3.Suite
import GHC.Int

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

       exitCode <- proc "tests/encode.sh" [hsTmpDir] empty
       exitCode @?= ExitSuccess

       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test.proto"]) empty
       exitCode @?= ExitSuccess
       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test_import.proto"]) empty
       exitCode @?= ExitSuccess
       touch (pyTmpDir </> "test_files" </> "__init__.py")

       m <- need "PYTHONPATH"
       pythonPath <- case m of
           Nothing         -> fail "PYTHONPATH environment variable is not set"
           Just pythonPath -> return pythonPath
       export "PYTHONPATH" (pythonPath <> ":" <> pyTmpDir)

       let cmd = (hsTmpDir <> "/simpleEncodeDotProto | python tests/check_simple_dot_proto.py")
       exitCode <- shell cmd empty
       exitCode @?= ExitFailure 12  -- We exit the python test with a special error code to make sure all tests completed

       -- Not using bracket so that we can inspect the output to fix the tests
       rmtree hsTmpDir
       rmtree pyTmpDir

simpleDecodeDotProto :: TestTree
simpleDecodeDotProto =
    testCase "generate code for a simple .proto and then use it to decode messages" $
    do mktree hsTmpDir
       mktree pyTmpDir

       compileTestDotProto

       exitCode <- proc "tests/decode.sh" [hsTmpDir] empty
       exitCode @?= ExitSuccess

       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test.proto"]) empty
       exitCode @?= ExitSuccess
       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test_import.proto"]) empty
       exitCode @?= ExitSuccess
       touch (pyTmpDir </> "test_files" </> "__init__.py")

       m <- need "PYTHONPATH"
       pythonPath <- case m of
           Nothing         -> fail "PYTHONPATH environment variable is not set"
           Just pythonPath -> return pythonPath
       export "PYTHONPATH" (pythonPath <> ":" <> pyTmpDir)

       let cmd = "python tests/send_simple_dot_proto.py | " <> hsTmpDir <> "/simpleDecodeDotProto "
       exitCode <- shell cmd empty
       exitCode @?= ExitSuccess

       rmtree hsTmpDir
       rmtree pyTmpDir

-- * Helpers

hsTmpDir, pyTmpDir :: IsString a => a
hsTmpDir = "test-files/hs-tmp"
pyTmpDir = "test-files/py-tmp"

compileTestDotProto :: IO ()
compileTestDotProto = do
  readDotProtoWithContext "test-files/test.proto" >>= \case
    Left err -> fail (show err)
    Right (dp, ctxt) ->
      case renderHsModuleForDotProto dp ctxt of
        Left err -> fail ("compileTestDotProto: Error compiling test.proto: " <> show err)
        Right hsSrc -> writeFile (hsTmpDir <> "/Test.hs") hsSrc

  readDotProtoWithContext "test-files/test_import.proto" >>= \case
    Left err -> fail (show err)
    Right (dp, ctxt) ->
      case renderHsModuleForDotProto dp ctxt of
        Left err -> fail ("compileTestDotProto: Error compiling test_import.proto: " <> show err)
        Right hsSrc -> writeFile (hsTmpDir <> "/TestImport.hs") hsSrc
