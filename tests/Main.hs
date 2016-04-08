{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           TestTypes
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import           Test.Tasty
import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC
import           Test.QuickCheck
import qualified Data.Protobuf.Wire.Encode.Internal as Enc
import qualified Data.Protobuf.Wire.Decode.Internal as Dec
import           Data.Protobuf.Wire.Decode.Parser as P
import           Data.Protobuf.Wire.Shared
import           Data.Protobuf.Wire.Generic
import           Data.Int
import           Data.Word (Word64)
import qualified Data.Text.Lazy as TL
import           Data.Serialize.Get(runGet)
import           Data.Either (isRight)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProperties, encodeUnitTests, decodeUnitTests,
                           parserUnitTests]

instance Arbitrary WireType where
  arbitrary = oneof $ map return [Varint, Fixed32, Fixed64, LengthDelimited]

instance Arbitrary FieldNumber where
  arbitrary = liftA FieldNumber arbitrary

qcInverses :: (HasEncoding a, Arbitrary a, Eq a) => Parser a -> a -> Bool
qcInverses parser msg = msg == (decode $ encode msg)
  where decode x = case parse parser x of
                    Left e -> error $ "got error parsing: " ++ show e
                    Right r -> r
        encode = BL.toStrict . toLazyByteString

qcProperties = testGroup "QuickCheck properties"
  [QC.testProperty "fieldHeader encode/decode inverses" $
   \fieldnum -> \wt -> let encoded = BL.toStrict $ BB.toLazyByteString $
                                     Enc.fieldHeader fieldnum wt
                           decoded = case runGet Dec.getFieldHeader encoded of
                                     Left e -> error e
                                     Right x -> x
                           in (fieldnum, wt) == decoded,

   QC.testProperty "base128Varint encode/decode inverses" $
   \w64 -> let encode = BL.toStrict . BB.toLazyByteString . Enc.base128Varint
               decode bs = case runGet Dec.getBase128Varint bs of
                           Left e -> error e
                           Right x -> x
               in (w64 :: Word64) == (decode $ encode w64),

  QC.testProperty "decode inverts encode for trivial messages" $
  qcInverses trivialParser,

  QC.testProperty "decode inverts encode for multi-field messages" $
  qcInverses multipleFieldsParser,

  QC.testProperty "decode inverts encode for enum messages" $
  qcInverses withEnumParser,

  QC.testProperty "decode inverts encode for nested messages" $
  qcInverses withNestingParser,

  QC.testProperty "decode inverts encode for repeated field messages" $
  qcInverses withRepetitionParser,

  {- TODO: fixed field decoding types don't match encoding yet.
  QC.testProperty "decode inverts encode for messages with fixed fields" $
  qcInverses withFixedParser, -}

  QC.testProperty "decode inverts encode for messages with bytes fields" $
  qcInverses withBytesParser

  {- TODO: encoding of packed fields not implemented, I think.
  QC.testProperty "decode inverts encode for messages with packed fields" $
  qcInverses withPackingParser -}

  ]

encodeUnitTests :: TestTree
encodeUnitTests = testGroup "Encoding unit tests"
                  [encodeTrivialMessage,
                   encodeNegativeInt,
                   encodeMultipleFields,
                   encodeNestedMessage,
                   encodeEnumFirstAlternative,
                   encodeEnumSecondAlternative,
                   encodeRepetition]

checkEncoding :: HasEncoding a => FilePath -> a -> IO ()
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

encodeNestedMessage :: TestTree
encodeNestedMessage = testCase
  "Encoding a message with nesting matches the official implementation" $
  checkEncoding "test-files/with_nesting.bin" $
  WithNesting $ Nested "123abc" 123456

encodeEnumFirstAlternative :: TestTree
encodeEnumFirstAlternative = testCase
  "Encoding an enum with case 0 matches the official implementation" $
  checkEncoding "test-files/with_enum0.bin" $ WithEnum ENUM1

encodeEnumSecondAlternative :: TestTree
encodeEnumSecondAlternative = testCase
  "Encoding an enum with case 1 matches the official implementation" $
  checkEncoding "test-files/with_enum1.bin" $ WithEnum ENUM2

encodeRepetition :: TestTree
encodeRepetition = testCase
  "Encoding a message with repetition matches the official implementation" $
  checkEncoding "test-files/with_repetition.bin" $ WithRepetition [1..5]

decodeUnitTests :: TestTree
decodeUnitTests = testGroup "Decode unit tests"
                  [decodeKeyValsTrivial,
                   decodeKeyValsMultipleFields,
                   decodeKeyValsNestedMessage,
                   decodeKeyValsEnumFirstAlternative,
                   decodeKeyValsEnumSecondAlternative,
                   decodeKeyValsRepetition]

decodeKeyValsTest :: FilePath -> IO ()
decodeKeyValsTest fp = do
  bs <- B.readFile fp
  let kvs = runGet Dec.getTuples bs
  assertBool "parsing failed" (isRight kvs)

decodeKeyValsTrivial :: TestTree
decodeKeyValsTrivial = testCase
  "Decoding a trivial message to a key/val list succeeds" $
  decodeKeyValsTest "test-files/trivial.bin"

decodeKeyValsMultipleFields :: TestTree
decodeKeyValsMultipleFields = testCase
  "Decoding a multi-field message to a key/val list succeeds" $
  decodeKeyValsTest "test-files/multiple_fields.bin"

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
                   ,parseNestedMessage
                   ,parseEnumFirstAlternative
                   ,parseEnumSecondAlternative
                   ,parseRepetition
                   ,parseFixed
                   ,parseBytes
                   ,parsePackedUnpacked
                   ]

testParser :: (Show a, Eq a) => FilePath -> Parser a -> a -> IO ()
testParser fp parser reference = do
  bs <- B.readFile fp
  case parse parser bs of
    Left err -> error $ "Got error: " ++ show err
    Right ourResult -> ourResult @?= reference

parseTrivial :: TestTree
parseTrivial = testCase
  "Parsing a trivial message matches the official implementation" $
  testParser "test-files/trivial.bin" trivialParser $ Trivial 123

parseMultipleFields :: TestTree
parseMultipleFields = testCase
  "Parsing a message with multiple fields matches the official implementation" $
  testParser "test-files/multiple_fields.bin" multipleFieldsParser $
    MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!" True

parseNestedMessage :: TestTree
parseNestedMessage = testCase
  "Parsing a nested message matches the official implementation" $
  testParser "test-files/with_nesting.bin" withNestingParser $
    WithNesting $ Nested "123abc" 123456

parseEnumFirstAlternative :: TestTree
parseEnumFirstAlternative = testCase
  "Parsing an enum (first case) message matches the official implementation" $
  testParser "test-files/with_enum0.bin" withEnumParser $
    WithEnum ENUM1

parseEnumSecondAlternative :: TestTree
parseEnumSecondAlternative = testCase
  "Parsing an enum (2nd case) message matches the official implementation" $
  testParser "test-files/with_enum1.bin" withEnumParser $
    WithEnum ENUM2

parseRepetition :: TestTree
parseRepetition = testCase
  "Parsing a message with a repeated field matches the official implementation"
  $
  testParser "test-files/with_repetition.bin" withRepetitionParser $
    WithRepetition [1..5]

parseFixed :: TestTree
parseFixed = testCase
  "Parsing a message with fixed types matches the official implementation" $
  testParser "test-files/with_fixed.bin" withFixedParser $
             WithFixed 16 (-123) 4096 (-4096)

parseBytes :: TestTree
parseBytes = testCase
  "Parsing a message containing bytes matches the official implementation" $
  testParser "test-files/with_bytes.bin" withBytesParser $
    WithBytes (BC.pack "abc") (map BC.pack ["abc","123"])

parsePackedUnpacked :: TestTree
parsePackedUnpacked = testCase
  "Parsing packed and unpacked fields matches the official implementation" $
  testParser "test-files/with_packing.bin" withPackingParser $
    WithPacking ([1,2,3] :: [Int32]) ([1,2,3] :: [Int32])
