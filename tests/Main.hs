{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           TestTypes
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import           Test.Tasty
import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC
import           Test.QuickCheck
import qualified Data.Protobuf.Wire.Encode.Internal as Enc
import qualified Data.Protobuf.Wire.Decode.Internal as Dec
import           Data.Protobuf.Wire.Shared
import           Data.Protobuf.Wire.Generic
import           Data.Int
import           Data.Word (Word64)
import qualified Data.Text.Lazy as TL
import           Data.Serialize.Get(runGet)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [scProperties, unitTests]

instance Arbitrary WireType where
  arbitrary = oneof $ map return [Varint, Fixed32, Fixed64, LengthDelimited]

instance Arbitrary FieldNumber where
  arbitrary = liftA FieldNumber arbitrary

scProperties = testGroup "QuickCheck properties"
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
               in (w64 :: Word64) == (decode $ encode w64)]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
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
  MultipleFields 1.23 (-0.5) 123 1234567890 "Hello, world!"

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
