{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Data.List.NonEmpty          as NE
import           Proto3.Suite.DotProto
import           Proto3.Wire.Types           as P
import qualified Test.DocTest
import           Test.QuickCheck             (Property, counterexample)
import           Test.Tasty
import           Test.Tasty.HUnit            (Assertion, testCase, (@=?))
import           Test.Tasty.QuickCheck       (testProperty)

import qualified Test.Proto.Generate.Name

-- -----------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ docTests
  , dotProtoUnitTests
  , Test.Proto.Generate.Name.tests
  ]

-- -----------------------------------------------------------------------------
-- Doctests

docTests :: TestTree
docTests = testCase "doctests" $ do
  putStrLn "Running all doctests..."
  Test.DocTest.doctest
    [ "-isrc"
    , "-itests"
    , "src/Proto3/Suite/DotProto/Internal.hs"
    ]

testDotProtoParse :: String -> DotProto -> Assertion
testDotProtoParse contents ast = do
  let path = metaModulePath $ protoMeta ast
  case parseProto path contents of
    Left err     -> error $ show err
    Right result -> ast @=? result

testDotProtoPrint :: DotProto -> String -> Assertion
testDotProtoPrint ast expected = expected @=? toProtoFileDef ast

testDotProtoRoundtrip :: DotProto -> Assertion
testDotProtoRoundtrip ast =
  let path = metaModulePath $ protoMeta ast in
  Right ast @=? parseProto path (toProtoFileDef ast)

dotProtoUnitTests :: TestTree
dotProtoUnitTests = testGroup ".proto parsing tests"
  [ dotProtoParseTrivial
  , dotProtoPrintTrivial
  , dotProtoRoundtripTrivial
  , dotProtoRoundtripSimpleMessage
  , qcDotProtoRoundtrip
  ]

trivialDotProto :: DotProto
trivialDotProto = DotProto [] [] DotProtoNoPackage [] (DotProtoMeta (Path $ "test-files" NE.:| ["test_trivial"]))

dotProtoParseTrivial :: TestTree
dotProtoParseTrivial = testCase
  "Parse a content-less file" $
  testDotProtoParse "syntax = \"proto3\";\n" trivialDotProto

dotProtoPrintTrivial :: TestTree
dotProtoPrintTrivial = testCase
  "Print a content-less DotProto" $
  testDotProtoPrint trivialDotProto "syntax = \"proto3\";"

dotProtoRoundtripTrivial :: TestTree
dotProtoRoundtripTrivial = testCase
  "Printing then parsing a content-less DotProto yields an empty DotProto" $
  testDotProtoRoundtrip trivialDotProto

dotProtoSimpleMessage :: DotProto
dotProtoSimpleMessage = DotProto [] [] DotProtoNoPackage
  [ DotProtoMessage "" (Single "MessageTest")
      [ DotProtoMessageField $
          DotProtoField (fieldNumber 1) (Prim Int32) (Single "testfield") [] ""
      ]
  ]
  (DotProtoMeta (Path ("test-files" NE.:| ["simple"])))

dotProtoRoundtripSimpleMessage :: TestTree
dotProtoRoundtripSimpleMessage = testCase
  "Round-trip for a single flat message" $
  testDotProtoRoundtrip dotProtoSimpleMessage

qcDotProtoRoundtrip :: TestTree
qcDotProtoRoundtrip = testProperty
  "Round-trip for a randomly-generated .proto AST" roundtrip
  where
    roundtrip :: DotProto -> Property
    roundtrip ast = let generated = toProtoFileDef ast
                    in case parseProto fakePath generated of
                      Left err     -> error $ formatParseError err generated
                      Right result -> counterexample (formatMismatch ast generated result ) (ast == result)

    formatMismatch initial generated result = "AST changed during reparsing\n\nInitial AST:\n\n"
                                           ++ show initial
                                           ++ "\n\nGenerated .proto file:\n\n"
                                           ++ generated
                                           ++ "\n\nReparsed AST:\n\n"
                                           ++ show result
                                           ++ "\n\nRegenerated .proto file:\n\n"
                                           ++ (toProtoFileDef result)
    formatParseError err generated = "Parsec error:\n\n"
                                  ++ show err
                                  ++ "\n\nWhen attempting to parse:\n\n"
                                  ++ generated
                                  ++ "\n\nInitial AST:\n\n"
