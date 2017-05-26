{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString             as B
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import           Data.Either                 (isRight)
import           Data.Int
import           Data.Maybe                  (fromJust)
import           Data.Monoid
import           Data.Serialize.Get          (runGet)
import           Data.String
import qualified Data.Text.Lazy              as TL
import           Data.Proxy
import           Data.Word                   (Word64)
import           GHC.Exts                    (fromList)
import           Proto3.Suite
import           Proto3.Suite.DotProto       as AST
import           Proto3.Wire
import           Proto3.Wire.Decode          (ParseError)
import qualified Proto3.Wire.Decode          as Decode
import qualified Proto3.Wire.Encode          as Encode
import           Proto3.Wire.Types           as P
import           Test.QuickCheck             (Arbitrary, Property, arbitrary,
                                              counterexample, oneof)
import           Test.Tasty
import           Test.Tasty.HUnit            (Assertion, assertBool, testCase,
                                              (@=?), (@?=))
import           Test.Tasty.QuickCheck       (testProperty, (===))

-- NB: GeneratedTestTypes.hs etc. should be manually generated via something
-- like:
--
--   [nix-shell]$ compile-proto-file --proto test-files/test.proto        > tests/GeneratedTestTypes.hs
--   [nix-shell]$ compile-proto-file --proto test-files/test_import.proto > tests/GeneratedImportedTestTypes.hs
--
-- And these commands would need to be run whenever test.proto or
-- test_import.proto change.
--
-- However, compile-proto-file hasn't been taken out of grpc-haskell and put
-- into this package where it belongs, and so doing the above doesn't work yet
-- while iterating on the code generation. Here's the quick and dirty way to
-- invoke CG in the meantime (a fix for this is intended soon):
--
-- Load this module into ghci from the root of the repository, then:
--
--   > Right (dp, tc) <- readDotProtoWithContext "/path/to/repo/root/test-files/test.proto"
--   > let Right src = renderHsModuleForDotProto dp tc
--   > writeFile "/path/to/repo/root/tests/GeneratedTestTypes.hs" src
--   > Right (dp, tc) <- readDotProtoWithContext "/path/to/repo/root/test-files/test_import.proto"
--   > let Right src = renderHsModuleForDotProto dp tc
--   > writeFile "/path/to/repo/root/tests/GeneratedImportedTestTypes.hs" src
--
-- TODO: Get compile-proto-file into the repository.
-- TODO: Automate generation of these modules as a part of the build process.

import qualified GeneratedTestTypes          as GTT
import           ArbitraryGeneratedTestTypes ()

import qualified OldTestTypes                as OTT
import           TestCodeGen

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ qcProperties
  , encodeUnitTests
  , decodeUnitTests
  , parserUnitTests
  , dotProtoUnitTests
  , codeGenTests
  ]

--------------------------------------------------------------------------------
-- QuickCheck properties

type MsgProp a = a -> Property

qcProperties :: TestTree
qcProperties = testGroup "QuickCheck properties"
  [ qcPropDecEncId
  ]

-- | Verifies that @decode . encode = id@ for various message types
qcPropDecEncId :: TestTree
qcPropDecEncId = testGroup "Property: (decode . encode = id) for various message types"
  [ testProperty "Trivial"             (prop :: MsgProp GTT.Trivial)
  , testProperty "MultipleFields"      (prop :: MsgProp GTT.MultipleFields)
  , testProperty "WithEnum"            (prop :: MsgProp GTT.WithEnum)
  , testProperty "WithNesting"         (prop :: MsgProp GTT.WithNesting)
  , testProperty "WithRepetition"      (prop :: MsgProp GTT.WithRepetition)
  , testProperty "WithFixed"           (prop :: MsgProp GTT.WithFixed)
  , testProperty "WithBytes"           (prop :: MsgProp GTT.WithBytes)
  , testProperty "AllPackedTypes"      (prop :: MsgProp GTT.AllPackedTypes)
  , testProperty "SignedInts"          (prop :: MsgProp GTT.SignedInts)
  , testProperty "WithNestingRepeated" (prop :: MsgProp GTT.WithNestingRepeated)
  , deeplyNest prop 1000
  ]
  where
    prop :: (Message a, Arbitrary a, Eq a, Show a) => MsgProp a
    prop msg = msg === (dec . enc) msg
      where
        dec = either (error . ("error parsing: " <>) . show) id . fromByteString
        enc = BL.toStrict . toLazyByteString

    deeplyNest :: MsgProp GTT.Wrapped -> Int -> TestTree
    deeplyNest pf 0 = testProperty "Deeply nested" pf
    deeplyNest pf n = deeplyNest (pf . GTT.Wrapped . Just) (n-1)


--------------------------------------------------------------------------------
-- Encoding

encodeUnitTests :: TestTree
encodeUnitTests = testGroup "Encoder unit tests"
  [ encoderMatchesGoldens
  ]

-- TODO: We should consider generating the reference encodings
-- (test-files/make_reference_encodings.py) as a part of running the test suite
-- rather than having them in the repository.
encoderMatchesGoldens :: TestTree
encoderMatchesGoldens = testGroup "Encoder matches golden encodings"
  [ check "trivial.bin"               $ GTT.Trivial 123
  , check "trivial_negative.bin"      $ GTT.Trivial (-1)
  , check "multiple_fields.bin"       $ GTT.MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True
  , check "signedints.bin"            $ GTT.SignedInts (-42) (-84)
  , check "with_nesting.bin"          $ GTT.WithNesting $ Just $ GTT.WithNesting_Nested "123abc" 123456 [] []
  , check "with_enum0.bin"            $ GTT.WithEnum $ Enumerated $ Right $ GTT.WithEnum_TestEnumENUM1
  , check "with_enum1.bin"            $ GTT.WithEnum $ Enumerated $ Right $ GTT.WithEnum_TestEnumENUM2
  , check "with_repetition.bin"       $ GTT.WithRepetition [1..5]
  , check "with_bytes.bin"            $ GTT.WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])
  , check "with_nesting_repeated.bin" $ GTT.WithNestingRepeated
                                          [ GTT.WithNestingRepeated_Nested "123abc" 123456 [1,2,3,4] [5,6,7,8]
                                          , GTT.WithNestingRepeated_Nested "abc123" 654321 [0,9,8,7] [6,5,4,3]
                                          ]
  ]
  where
    check fp v = testCase fp $ do
      goldenEncoding <- BL.readFile (testFilesPfx <> fp)
      toLazyByteString v @?= goldenEncoding

--------------------------------------------------------------------------------
-- Decoding

decodeUnitTests :: TestTree
decodeUnitTests = testGroup "Decoder unit tests"
  [ decodeFromGoldens
  ]

decodeFromGoldens :: TestTree
decodeFromGoldens = testGroup "Decode golden encodings into key/value lists"
  [ check "trivial.bin"
  , check "trivial_negative.bin"
  , check "multiple_fields.bin"
  , check "signedints.bin"
  , check "with_nesting.bin"
  , check "with_enum0.bin"
  , check "with_enum1.bin"
  , check "with_repetition.bin"
  , check "with_bytes.bin"
  , check "with_nesting_repeated.bin"
  ]
  where
    check fp = testCase fp $ do
      kvs <- Decode.decodeWire <$> B.readFile (testFilesPfx <> fp)
      assertBool ("parsing " <> fp <> " into a key-value list succeeds") (isRight kvs)

--------------------------------------------------------------------------------
-- Parser

parserUnitTests :: TestTree
parserUnitTests = testGroup "Parser unit tests"
  [ parseFromGoldens
  ]

parseFromGoldens :: TestTree
parseFromGoldens = testGroup "Parse golden encodings"
  [ check "trivial.bin"               $ GTT.Trivial 123
  , check "multiple_fields.bin"       $ GTT.MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True
  , check "signedints.bin"            $ GTT.SignedInts (-42) (-84)
  , check "with_nesting.bin"          $ GTT.WithNesting $ Just $ GTT.WithNesting_Nested "123abc" 123456 [] []
  , check "with_enum0.bin"            $ GTT.WithEnum $ Enumerated $ Right $ GTT.WithEnum_TestEnumENUM1
  , check "with_enum1.bin"            $ GTT.WithEnum $ Enumerated $ Right $ GTT.WithEnum_TestEnumENUM2
  , check "with_repetition.bin"       $ GTT.WithRepetition [1..5]
  , check "with_fixed.bin"            $ GTT.WithFixed (Fixed 16) (Fixed (-123)) (Fixed 4096) (Fixed (-4096))
  , check "with_bytes.bin"            $ GTT.WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])
  , check "with_packing.bin"          $ GTT.WithPacking [1,2,3] [1,2,3]
  , check "all_packed_types.bin"      $ GTT.AllPackedTypes
                                          [1,2,3]
                                          [1,2,3]
                                          [-1,-2,-3]
                                          [-1,-2,-3]
                                          (fromList $ map Fixed [1..3])
                                          (fromList $ map Fixed [1..3])
                                          [1.0,2.0]
                                          [1.0,-1.0]
                                          (fromList $ map Fixed [1,2,3])
                                          (fromList $ map Fixed [1,2,3])
  , check "with_nesting_repeated.bin" $ GTT.WithNestingRepeated
                                          [ GTT.WithNestingRepeated_Nested "123abc" 123456 [1,2,3,4] [5,6,7,8]
                                          , GTT.WithNestingRepeated_Nested "abc123" 654321 [0,9,8,7] [6,5,4,3]
                                          ]
  , -- Checks parsing repeated embedded messages when one is expected (i.e.,
    -- this tests correct merging; this value was encoded as a
    -- WithNestingRepeated).
    check "with_nesting_repeated.bin" $ GTT.WithNesting $ Just $ GTT.WithNesting_Nested "abc123" 654321 [1,2,3,4,0,9,8,7] [5,6,7,8,6,5,4,3]
  , -- Checks that embedded message merging works correctly when fields have
    -- default values; this value was encoded as a WithNestingRepeatedInts
    check "with_nesting_ints.bin"     $ GTT.WithNestingInts $ Just $ GTT.NestedInts 2 2
  ]
  where
    check fp = testCase fp . testParser (testFilesPfx <> fp) fromByteString

--------------------------------------------------------------------------------
-- Helpers

dotProtoFor :: (Named a, Message a) => Proxy a -> DotProto
dotProtoFor proxy = DotProto [] [] DotProtoNoPackage
  [ DotProtoMessage (Single (nameOf proxy)) (DotProtoMessageField <$> dotProto proxy)
  ]

showDotProtoFor :: (Named a, Message a) => Proxy a -> IO ()
showDotProtoFor = putStrLn . toProtoFileDef . dotProtoFor

instance Arbitrary WireType where
  arbitrary = oneof $ map return [Varint, P.Fixed32, P.Fixed64, LengthDelimited]

testFilesPfx :: IsString a => a
testFilesPfx = "test-files/"

testParser :: (Show a, Eq a)
           => FilePath -> (B.ByteString -> Either ParseError a) -> a -> IO ()
testParser fp p reference = do
  bs <- B.readFile fp
  case p bs of
    Left err        -> error $ "Got error: " ++ show err
    Right ourResult -> ourResult @?= reference

testDotProtoParse :: FilePath -> DotProto -> Assertion
testDotProtoParse file ast = do
  contents <- readFile file
  case parseProto contents of
    Left err     -> error $ show err
    Right result -> ast @=? result

testDotProtoPrint :: DotProto -> String -> Assertion
testDotProtoPrint ast expected = expected @=? toProtoFileDef ast

testDotProtoRoundtrip :: DotProto -> Assertion
testDotProtoRoundtrip ast = let Right result = parseProto $ toProtoFileDef ast
                            in ast @=? result

dotProtoUnitTests :: TestTree
dotProtoUnitTests = testGroup ".proto parsing tests"
  [ dotProtoParseTrivial
  , dotProtoPrintTrivial
  , dotProtoRoundtripTrivial
  , dotProtoRoundtripSimpleMessage
  , qcDotProtoRoundtrip
  ]

trivialDotProto :: DotProto
trivialDotProto = DotProto [] [] DotProtoNoPackage []

dotProtoParseTrivial :: TestTree
dotProtoParseTrivial = testCase
  "Parse a content-less file" $
  testDotProtoParse "test-files/trivial.proto" trivialDotProto

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
  [ DotProtoMessage (Single "MessageTest")
      [ DotProtoMessageField $
          DotProtoField (fieldNumber 1) (Prim Int32) (Single "testfield") [] Nothing
      ]
  ]

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
                    in case parseProto generated of
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
