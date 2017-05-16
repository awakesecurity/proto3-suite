{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           TestTypes
import           TestCodeGen
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Test.DocTest
import           Test.Tasty
import           Test.Tasty.HUnit (Assertion, (@?=), (@=?), testCase, assertBool)
import           Test.Tasty.QuickCheck (testProperty, (===))
import           Test.QuickCheck (Arbitrary, Property,
                                  arbitrary, counterexample, oneof)
import           Data.Int
import           Data.Maybe (fromJust)
import           Data.Word (Word64)
import qualified Data.Text.Lazy as TL
import           Data.Serialize.Get(runGet)
import           Data.Either (isRight)
import           Proto3.Suite
import           Proto3.Suite.DotProto as AST
import           Proto3.Wire
import           Proto3.Wire.Decode (ParseError)
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import           Proto3.Wire.Types as P
import           GHC.Exts (fromList)

main :: IO ()
main = do
  Test.DocTest.doctest
    [ "-isrc"
    , "src/Proto3/Suite/DotProto/Generate/JSON.hs"
    ]
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProperties, encodeUnitTests, decodeUnitTests,
                           parserUnitTests, dotProtoUnitTests, codeGenTests]

instance Arbitrary WireType where
  arbitrary = oneof $ map return [Varint, P.Fixed32, P.Fixed64, LengthDelimited]

qcInverses :: (Message a, Arbitrary a, Eq a, Show a) => a -> Property
qcInverses msg = msg === (decode $ encode msg)
  where decode x = case fromByteString x of
                    Left e -> error $ "got error parsing: " ++ show e
                    Right r -> r
        encode = BL.toStrict . toLazyByteString

deeplyNested :: String -> Int -> TestTree
deeplyNested name = go (qcInverses :: Trivial -> Property)
  where
    go :: (Message a, Named a, Arbitrary a, Eq a, Show a) => (a -> Property) -> Int -> TestTree
    go prop 0 = testProperty name prop
    go prop n = go (prop . fromJust . nested . unNestedAlways . unWrapped) (n - 1)

qcProperties :: TestTree
qcProperties = testGroup "QuickCheck properties"
  [ testProperty "decode inverts encode for trivial messages" $
    (qcInverses :: Trivial -> Property)

  , testProperty "decode inverts encode for multi-field messages" $
    (qcInverses :: MultipleFields -> Property)

  , testProperty "decode inverts encode for enum messages" $
    (qcInverses :: WithEnum -> Property)

  , testProperty "decode inverts encode for nested messages" $
    (qcInverses :: WithNesting -> Property)

  , testProperty "decode inverts encode for repeated field messages" $
    (qcInverses :: WithRepetition -> Property)

  , testProperty "decode inverts encode for messages with fixed fields" $
    (qcInverses :: WithFixed -> Property)

  , testProperty "decode inverts encode for messages with bytes fields" $
    (qcInverses :: WithBytes -> Property)

  , testProperty "decode inverts encode for all packed repeated types" $
    (qcInverses :: AllPackedTypes -> Property)

  , testProperty "decode inverts encode for signed integer types" $
    (qcInverses :: SignedInts -> Property)

  , testProperty "decode inverts encode for repeated messages" $
    (qcInverses :: WithNestingRepeated -> Property)

  , deeplyNested "decode inverts encode for deeply nested messages" 10000
  ]

encodeUnitTests :: TestTree
encodeUnitTests = testGroup "Encoding unit tests"
                  [encodeTrivialMessage,
                   encodeNegativeInt,
                   encodeMultipleFields,
                   encodeSignedInts,
                   encodeNestedMessage,
                   encodeEnumFirstAlternative,
                   encodeEnumSecondAlternative,
                   encodeRepetition,
                   encodeBytes,
                   --encodeNestedMaybe
                   encodeWithNestingRepeated
                   ]

checkEncoding :: Message a => FilePath -> a -> IO ()
checkEncoding fp x = do let ourEncoding = toLazyByteString x
                        referenceEncoding <- BL.readFile fp
                        ourEncoding @?= referenceEncoding

encodeTrivialMessage :: TestTree
encodeTrivialMessage = testCase
  "Encoding a trivial message matches the official implementation" $
  checkEncoding "test-files/trivial.bin" $ Trivial 123

encodeNegativeInt :: TestTree
encodeNegativeInt = testCase
  "Encoding a message with a negative int matches the official implementation" $
  checkEncoding "test-files/trivial_negative.bin" $ Trivial (-1)

encodeMultipleFields :: TestTree
encodeMultipleFields = testCase
  "Encoding a message with many fields matches the official implementation" $
  checkEncoding "test-files/multiple_fields.bin" $
  MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True

encodeSignedInts :: TestTree
encodeSignedInts = testCase
  "Encoding a message containing signed ints matches the official implementation" $
  checkEncoding "test-files/signedints.bin" (SignedInts (-42) (-84))

encodeNestedMessage :: TestTree
encodeNestedMessage = testCase
  "Encoding a message with nesting matches the official implementation" $
  checkEncoding "test-files/with_nesting.bin" $
  WithNesting $ Nested $ Just $ NestedMsg "123abc" 123456 [] []

encodeEnumFirstAlternative :: TestTree
encodeEnumFirstAlternative = testCase
  "Encoding an enum with case 0 matches the official implementation" $
  checkEncoding "test-files/with_enum0.bin" $
  WithEnum $ Enumerated (Right ENUM1)

encodeEnumSecondAlternative :: TestTree
encodeEnumSecondAlternative = testCase
  "Encoding an enum with case 1 matches the official implementation" $
  checkEncoding "test-files/with_enum1.bin" $
  WithEnum $ Enumerated (Right ENUM2)

encodeRepetition :: TestTree
encodeRepetition = testCase
  "Encoding a message with repetition matches the official implementation" $
  checkEncoding "test-files/with_repetition.bin" $ WithRepetition [1..5]

encodeBytes :: TestTree
encodeBytes = testCase
  "Encoding a message with bytes fields matches the official implementation" $
  checkEncoding "test-files/with_bytes.bin" $
  WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])

encodeWithNestingRepeated :: TestTree
encodeWithNestingRepeated = testCase
  "Encoding repeated embedded messages matches the official implementation" $
  checkEncoding "test-files/with_nesting_repeated.bin" $
  WithNestingRepeated [NestedMsg "123abc" 123456 [1,2,3,4] [5,6,7,8],
                       NestedMsg "abc123" 654321 [0,9,8,7] [6,5,4,3]]

decodeUnitTests :: TestTree
decodeUnitTests = testGroup "Decode unit tests"
                  [decodeKeyValsTrivial,
                   decodeKeyValsMultipleFields,
                   decodeKeyValsSignedInts,
                   decodeKeyValsNestedMessage,
                   decodeKeyValsEnumFirstAlternative,
                   decodeKeyValsEnumSecondAlternative,
                   decodeKeyValsRepetition]

decodeKeyValsTest :: FilePath -> IO ()
decodeKeyValsTest fp = do
  bs <- B.readFile fp
  let kvs = Decode.decodeWire bs
  assertBool "parsing failed" (isRight kvs)

decodeKeyValsTrivial :: TestTree
decodeKeyValsTrivial = testCase
  "Decoding a trivial message to a key/val list succeeds" $
  decodeKeyValsTest "test-files/trivial.bin"

decodeKeyValsMultipleFields :: TestTree
decodeKeyValsMultipleFields = testCase
  "Decoding a multi-field message to a key/val list succeeds" $
  decodeKeyValsTest "test-files/multiple_fields.bin"

decodeKeyValsSignedInts :: TestTree
decodeKeyValsSignedInts = testCase
  "Decoding a message containing signed ints to a key/val list succeeds" $
  decodeKeyValsTest "test-files/signedints.bin"

decodeKeyValsNestedMessage :: TestTree
decodeKeyValsNestedMessage = testCase
  "Decoding a nested message to a key/val list succeeds" $
  decodeKeyValsTest "test-files/with_nesting.bin"

decodeKeyValsEnumFirstAlternative :: TestTree
decodeKeyValsEnumFirstAlternative = testCase
  "Decoding an Enum (set to the first alternative) to a key/val list succeeds" $
  decodeKeyValsTest "test-files/with_enum0.bin"

decodeKeyValsEnumSecondAlternative :: TestTree
decodeKeyValsEnumSecondAlternative = testCase
  "Decoding an Enum (set to 2nd alternative) to a key/val list succeeds" $
  decodeKeyValsTest "test-files/with_enum1.bin"

decodeKeyValsRepetition :: TestTree
decodeKeyValsRepetition = testCase
  "Decoding a message with a repeated field to a key/val list succeeds" $
  decodeKeyValsTest "test-files/with_repetition.bin"

parserUnitTests :: TestTree
parserUnitTests = testGroup "Parsing unit tests"
                  [parseTrivial
                   ,parseMultipleFields
                   ,parseSignedInts
                   ,parseNestedMessage
                   ,parseEnumFirstAlternative
                   ,parseEnumSecondAlternative
                   ,parseRepetition
                   ,parseFixed
                   ,parseBytes
                   ,parsePackedUnpacked
                   ,parseAllPackedTypes
                   ,parseWithNestingRepeated
                   ,parseWithNestingRepeatedAbsent
                   ,parseWithNestingInt
                   ]

testParser :: (Show a, Eq a) =>
              FilePath
              -> (B.ByteString -> Either ParseError a) -> a -> IO ()
testParser fp p reference = do
  bs <- B.readFile fp
  case p bs of
    Left err -> error $ "Got error: " ++ show err
    Right ourResult -> ourResult @?= reference

parseTrivial :: TestTree
parseTrivial = testCase
  "Parsing a trivial message matches the official implementation" $
  testParser "test-files/trivial.bin" fromByteString $ Trivial 123

parseMultipleFields :: TestTree
parseMultipleFields = testCase
  "Parsing a message with multiple fields matches the official implementation" $
  testParser "test-files/multiple_fields.bin" fromByteString
  $ MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True

parseSignedInts :: TestTree
parseSignedInts = testCase
  "Parsing a message containing signed ints matches the official implementation" $
  testParser "test-files/signedints.bin" fromByteString $ SignedInts (-42) (-84)

parseNestedMessage :: TestTree
parseNestedMessage = testCase
  "Parsing a nested message matches the official implementation" $
  testParser "test-files/with_nesting.bin" fromByteString $
    WithNesting $ Nested $ Just $ NestedMsg "123abc" 123456 [] []

parseEnumFirstAlternative :: TestTree
parseEnumFirstAlternative = testCase
  "Parsing an enum (first case) message matches the official implementation" $
  testParser "test-files/with_enum0.bin" fromByteString $
    WithEnum $ Enumerated (Right ENUM1)

parseEnumSecondAlternative :: TestTree
parseEnumSecondAlternative = testCase
  "Parsing an enum (2nd case) message matches the official implementation" $
  testParser "test-files/with_enum1.bin" fromByteString $
    WithEnum $ Enumerated (Right ENUM2)

parseRepetition :: TestTree
parseRepetition = testCase
  "Parsing a message with a repeated field matches the official implementation"
  $
  testParser "test-files/with_repetition.bin" fromByteString $
    WithRepetition [1..5]

parseFixed :: TestTree
parseFixed = testCase
  "Parsing a message with fixed types matches the official implementation" $
  testParser "test-files/with_fixed.bin" fromByteString $
             WithFixed (Fixed 16) (Signed $ Fixed (-123))
                       (Fixed 4096) (Signed $ Fixed (-4096))

parseBytes :: TestTree
parseBytes = testCase
  "Parsing a message containing bytes matches the official implementation" $
  testParser "test-files/with_bytes.bin" fromByteString $
    WithBytes (BC.pack "abc") (fromList $ map BC.pack ["abc","123"])

parsePackedUnpacked :: TestTree
parsePackedUnpacked = testCase
  "Parsing packed and unpacked fields matches the official implementation" $
  testParser "test-files/with_packing.bin" fromByteString $
    WithPacking [1,2,3] [1,2,3]

parseAllPackedTypes :: TestTree
parseAllPackedTypes = testCase
  "Parsing all types of packed fields matches the official implementation" $
  testParser "test-files/all_packed_types.bin" fromByteString $
    AllPackedTypes [1,2,3]
                   [1,2,3]
                   [-1,-2,-3]
                   [-1,-2,-3]
                   (fromList $ map Fixed [1..3])
                   (fromList $ map Fixed [1..3])
                   [1.0,2.0]
                   [1.0,-1.0]
                   (fromList $ map (Signed . Fixed) [1,2,3])
                   (fromList $ map (Signed . Fixed) [1,2,3])

parseWithNestingRepeated :: TestTree
parseWithNestingRepeated = testCase
  "Parsing repeated embedded messages matches the official implementation" $
  testParser "test-files/with_nesting_repeated.bin" fromByteString $
  WithNestingRepeated [NestedMsg "123abc" 123456 [1,2,3,4] [5,6,7,8],
                       NestedMsg "abc123" 654321 [0,9,8,7] [6,5,4,3]]

parseWithNestingRepeatedAbsent :: TestTree
parseWithNestingRepeatedAbsent = testCase
  "Parsing repeated embedded messages when one is expected: correct merging" $
  testParser "test-files/with_nesting_repeated.bin" fromByteString $
  WithNestingRepeatedAbsent $ Nested $
    Just $ NestedMsg "abc123" 654321 [1,2,3,4,0,9,8,7] [5,6,7,8,6,5,4,3]

parseWithNestingInt :: TestTree
parseWithNestingInt = testCase
  "Embedded message merging works correctly when fields have default values" $
  testParser "test-files/with_nesting_ints.bin" fromByteString $
  WithNestingRepeatedInts $ Nested $ Just $ NestedInt 2 2

testDotProtoParse :: FilePath -> DotProto -> Assertion
testDotProtoParse file ast = do contents <- readFile file
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
  "Parsing a content-less file behaves as expected" $
  testDotProtoParse "test-files/trivial.proto" trivialDotProto

dotProtoPrintTrivial :: TestTree
dotProtoPrintTrivial = testCase
  "Printing a content-less DotProto behaves as expected" $
  testDotProtoPrint trivialDotProto "syntax = \"proto3\";"

dotProtoRoundtripTrivial :: TestTree
dotProtoRoundtripTrivial = testCase
  "Printing then parsing a content-less DotProto returns an empty DotProto" $
  testDotProtoRoundtrip trivialDotProto

dotProtoSimpleMessage :: DotProto
dotProtoSimpleMessage = DotProto [] [] DotProtoNoPackage [DotProtoMessage (Single "MessageTest")
                                                                          [DotProtoMessageField $ DotProtoField (fieldNumber 1)
                                                                                                                (Prim Int32)
                                                                                                                (Single "testfield")
                                                                                                                []
                                                                                                                Nothing ]]

dotProtoRoundtripSimpleMessage :: TestTree
dotProtoRoundtripSimpleMessage = testCase
  "Rountrip for a single, flat message" $
  testDotProtoRoundtrip dotProtoSimpleMessage

qcDotProtoRoundtrip :: TestTree
qcDotProtoRoundtrip = testProperty
  "Rountrip for a randomly-generated .proto AST" roundtrip
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
