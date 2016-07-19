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
import           Turtle

import           Data.Protobuf.Wire.DotProto
import           Data.Protobuf.Wire.DotProto.Generate

codeGenTests :: TestTree
codeGenTests = testGroup "Code generator unit tests"
  [ camelCaseMessageNames
  , camelCaseFieldNames
  , simpleEncodeDotProto
  , simpleDecodeDotProto]

camelCaseMessageNames :: TestTree
camelCaseMessageNames = testGroup "CamelCase'ing of message names"
  [ testCase "Capitalizes letters after underscores" (typeLikeName "protocol_analysis" @?= Right "ProtocolAnalysis")
  , testCase "Preserves casing of interior letters"  (typeLikeName "analyze_HTTP" @?= Right "AnalyzeHTTP")
  , testCase "Handles non-alphanumeric characters after underscore" (typeLikeName "analyze_http_2" @?= Right "AnalyzeHttp2")
  , testCase "Preserves one underscore in double underscore sequence" (typeLikeName "Analyze__HTTP" @?= Right "Analyze_HTTP")
  , testCase "Handles names prefixed with undedrscore" (typeLikeName "_message_name" @?= Right "XMessageName")
  , testCase "Preserves trailing underscore" (typeLikeName "message_name_" @?= Right "MessageName_") ]


camelCaseFieldNames :: TestTree
camelCaseFieldNames = testGroup "camelCase'ing of field names"
  [ testCase "Preserves capitalization patterns" (fieldLikeName "IP" @?= "ip")
  , testCase "Preserves underscores"             (fieldLikeName "IP_address" @?= "ip_address") ]

simpleEncodeDotProto :: TestTree
simpleEncodeDotProto =
    testCase "generate code for a simple .proto and then use it to encode messages" $
    do mktree hsTmpDir
       mktree pyTmpDir

       compileTestDotProto

       exitCode <- shell (T.concat ["stack ghc -- --make -odir ", hsTmpDir, " -hidir ", hsTmpDir, " -o ", hsTmpDir, "/simpleEncodeDotProto ", hsTmpDir, "/Test.hs ", hsTmpDir, "/TestImport.hs tests/SimpleEncodeDotProto.hs >/dev/null"]) empty
       exitCode @?= ExitSuccess

       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test.proto"]) empty
       exitCode @?= ExitSuccess
       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test_import.proto"]) empty
       exitCode @?= ExitSuccess
       touch (pyTmpDir </> "test_files" </> "__init__.py")

       export "PYTHONPATH" pyTmpDir
       exitCode <- shell (hsTmpDir <> "/simpleEncodeDotProto | python tests/check_simple_dot_proto.py") empty
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

       exitCode <- shell (T.concat ["stack ghc -- --make -odir ", hsTmpDir, " -hidir ", hsTmpDir, " -o ", hsTmpDir, "/simpleDecodeDotProto ", hsTmpDir, "/Test.hs ", hsTmpDir, "/TestImport.hs tests/SimpleDecodeDotProto.hs >/dev/null"]) empty
       exitCode @?= ExitSuccess

       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test.proto"]) empty
       exitCode @?= ExitSuccess
       exitCode <- shell (T.concat ["protoc --python_out=", pyTmpDir, " test-files/test_import.proto"]) empty
       exitCode @?= ExitSuccess
       touch (pyTmpDir </> "test_files" </> "__init__.py")

       export "PYTHONPATH" pyTmpDir
       exitCode <- shell ("python tests/send_simple_dot_proto.py | " <> hsTmpDir <> "/simpleDecodeDotProto ") empty
       exitCode @?= ExitSuccess

       rmtree hsTmpDir
       rmtree pyTmpDir

-- * Helpers

hsTmpDir, pyTmpDir :: IsString a => a
hsTmpDir = "test-files/tmp"
pyTmpDir = "test-files/py-tmp"

compileTestDotProto =
    do dpRes <- readDotProtoWithContext "test-files/test.proto"
       case dpRes of
         Left err -> fail (show err)
         Right (dp, ctxt) ->
           case renderHsModuleForDotProto dp ctxt of
             Left err -> fail ("compileTestDotProto: Error compiling test.proto: " <> show err)
             Right hsSrc -> writeFile "test-files/tmp/Test.hs" hsSrc

       dpRes <- readDotProtoWithContext "test-files/test_import.proto"
       case dpRes of
         Left err -> fail (show err)
         Right (dp, ctxt) ->
           case renderHsModuleForDotProto dp ctxt of
             Left err -> fail ("compileTestDotProto: Error compiling test_import.proto: " <> show err)
             Right hsSrc -> writeFile "test-files/tmp/TestImport.hs" hsSrc
