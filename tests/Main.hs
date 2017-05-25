{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import           Data.Either             (isRight)
import           Data.Int
import           Data.Maybe              (fromJust)
import           Data.Monoid
import           Data.Serialize.Get      (runGet)
import           Data.String
import qualified Data.Text.Lazy          as TL
import           Data.Word               (Word64)
import           GHC.Exts                (fromList)
import           Proto3.Suite
import           Proto3.Suite.DotProto   as AST
import           Proto3.Wire
import           Proto3.Wire.Decode      (ParseError)
import qualified Proto3.Wire.Decode      as Decode
import qualified Proto3.Wire.Encode      as Encode
import           Proto3.Wire.Types       as P
import           Test.QuickCheck         (Arbitrary, Property, arbitrary,
                                          counterexample, oneof)
import           Test.Tasty
import           Test.Tasty.HUnit        (Assertion, assertBool, testCase,
                                          (@=?), (@?=))
import           Test.Tasty.QuickCheck   (testProperty, (===))

import           TestCodeGen
import           TestTypes

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

-- decode . encode = id
qcProperties :: TestTree
qcProperties = testGroup "QuickCheck properties"
  [ qcPropDecEncId
  ]

qcPropDecEncId :: TestTree
qcPropDecEncId = testGroup "Property: (decode . encode = id) for various message types"
  [ testProperty "Trivial"             (prop :: MsgProp Trivial)
  , testProperty "MultipleFields"      (prop :: MsgProp MultipleFields)
  , testProperty "WithEnum"            (prop :: MsgProp WithEnum)
  , testProperty "WithNesting"         (prop :: MsgProp WithNesting)
  , testProperty "WithRepetition"      (prop :: MsgProp WithRepetition)
  , testProperty "WithFixed"           (prop :: MsgProp WithFixed)
  , testProperty "WithBytes"           (prop :: MsgProp WithBytes)
  , testProperty "AllPackedTypes"      (prop :: MsgProp AllPackedTypes)
  , testProperty "SignedInts"          (prop :: MsgProp SignedInts)
  , testProperty "WithNestingRepeated" (prop :: MsgProp WithNestingRepeated)
  , let
      go :: (Message a, Named a, Arbitrary a, Eq a, Show a) => (a -> Property) -> Int -> TestTree
      go pf 0 = testProperty "Deeply nested" pf
      go pf n = go (pf . fromJust . nested . unNestedAlways . unWrapped) (n - 1)
   in
     go (prop :: MsgProp Trivial) 10000
  ]
  where
    prop :: (Message a, Arbitrary a, Eq a, Show a) => MsgProp a
    prop msg = msg === (dec . enc) msg
      where
        dec = either (error . ("got error parsing: " <>) . show) id . fromByteString
        enc = BL.toStrict . toLazyByteString

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
  [ check "trivial.bin"               $ Trivial 123
  , check "trivial_negative.bin"      $ Trivial (-1)
  , check "multiple_fields.bin"       $ MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True
  , check "signedints.bin"            $ SignedInts (-42) (-84)
  , check "with_nesting.bin"          $ WithNesting $ Nested $ Just $ NestedMsg "123abc" 123456 [] []
  , check "with_enum0.bin"            $ WithEnum $ Enumerated $ Right ENUM1
  , check "with_enum1.bin"            $ WithEnum $ Enumerated $ Right ENUM2
  , check "with_repetition.bin"       $ WithRepetition [1..5]
  , check "with_bytes.bin"            $ WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])
  , check "with_nesting_repeated.bin" $ WithNestingRepeated
                                          [ NestedMsg "123abc" 123456 [1,2,3,4] [5,6,7,8]
                                          , NestedMsg "abc123" 654321 [0,9,8,7] [6,5,4,3]
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
  [ check "trivial.bin"               $ Trivial 123
  , check "multiple_fields.bin"       $ MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True
  , check "signedints.bin"            $ SignedInts (-42) (-84)
  , check "with_nesting.bin"          $ WithNesting $ Nested $ Just $ NestedMsg "123abc" 123456 [] []
  , check "with_enum0.bin"            $ WithEnum $ Enumerated (Right ENUM1)
  , check "with_enum1.bin"            $ WithEnum $ Enumerated (Right ENUM2)
  , check "with_repetition.bin"       $ WithRepetition [1..5]
  , check "with_fixed.bin"            $ WithFixed (Fixed 16) (Signed $ Fixed (-123))
                                          (Fixed 4096) (Signed $ Fixed (-4096))
  , check "with_bytes.bin"            $ WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])
  , check "with_packing.bin"          $ WithPacking [1,2,3] [1,2,3]
  , check "all_packed_types.bin"      $ AllPackedTypes [1,2,3]
                                          [1,2,3]
                                          [-1,-2,-3]
                                          [-1,-2,-3]
                                          (fromList $ map Fixed [1..3])
                                          (fromList $ map Fixed [1..3])
                                          [1.0,2.0]
                                          [1.0,-1.0]
                                          (fromList $ map (Signed . Fixed) [1,2,3])
                                          (fromList $ map (Signed . Fixed) [1,2,3])
  , check "with_nesting_repeated.bin" $ WithNestingRepeated [NestedMsg "123abc" 123456 [1,2,3,4] [5,6,7,8],
                                          NestedMsg "abc123" 654321 [0,9,8,7] [6,5,4,3]]
  , check "with_nesting_repeated.bin" $ WithNestingRepeatedAbsent $ Nested $
                                          Just $ NestedMsg "abc123" 654321 [1,2,3,4,0,9,8,7] [5,6,7,8,6,5,4,3]
  , check "with_nesting_ints.bin"     $ WithNestingRepeatedInts $ Nested $ Just $ NestedInt 2 2
  ]
  where
    check fp = testCase fp . testParser (testFilesPfx <> fp) fromByteString

--------------------------------------------------------------------------------
-- Helpers

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
