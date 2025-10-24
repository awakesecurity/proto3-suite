{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Test.Tasty
import Test.Tasty.HUnit (Assertion, (@?=), assertEqual, assertFailure, testCase)
import Control.Monad (when)
import qualified Data.Map as M
import Proto3.Suite
import qualified Proto3.Suite.JSONPB as JSONPB
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, showsTypeRep, typeOf, typeRep)
import GHC.Stack (HasCallStack)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO
import Text.Read (readEither)

import TestProto
import qualified TestProtoImport
import qualified TestProtoNegativeEnum
import qualified TestProtoOneof
import qualified TestProtoOneofImport
import qualified TestProtoOptional
import qualified TestProtoWrappers

data Format = Binary | Jsonpb
  deriving (Bounded, Enum, Eq, Read, Show)

main :: IO ()
main = do
  format <- getFormat
  let ?format = format
  putStr "\n"
  defaultMain tests

-- | In order to keep using 'defaultMain', which is convenient, we take
-- the format as an environment variable instead of a command line argument.
getFormat :: IO Format
getFormat = do
  maybeFormat <- lookupEnv "FORMAT"
  case maybeFormat of
    Nothing -> die $
      "Please export a value for the FORMAT environment variable: one of: " ++
      (intercalate ", " (map show ([minBound .. maxBound] :: [Format])))
    Just fstr -> case readEither fstr of
      Left _ -> die $ "Bad value of FORMAT environment variable: " ++ show fstr
      Right format -> pure format

tests, testCase1, testCase2, testCaseSignedInts, testCaseRepeatedSignedInts,
    testCase3, testCase4, testCase5, testCase6, testCase7, testCase8, testCase9,
    testCase10, testCase11, testCase12, testCase13, testCase14, testCase15,
    testCase16, testCase17, testCase18, testCase19,
    testCase_DoubleValue, testCase_FloatValue, testCase_Int64Value,
    testCase_UInt64Value, testCase_Int32Value, testCase_UInt32Value,
    testCase_BoolValue, testCase_StringValue, testCase_BytesValue,
    testCase_NegativeEnum, testCase_Optional,
    allTestsDone :: (?format :: Format) => TestTree
tests = testGroup
          ("Decode protobuf messages from Python: format " ++ show ?format)
          [ testCase1, testCase2, testCaseSignedInts, testCaseRepeatedSignedInts
          , testCase3, testCase4, testCase5, testCase6
          , testCase7, testCase8, testCase9, testCase10
          , testCase11, testCase12, testCase13, testCase14
          , testCase15, testCase16, testCase17, testCase18
          , testCase19
          , testCase_DoubleValue, testCase_FloatValue, testCase_Int64Value
          , testCase_UInt64Value, testCase_Int32Value, testCase_UInt32Value
          , testCase_BoolValue, testCase_StringValue, testCase_BytesValue
          , testCase_NegativeEnum, testCase_Optional
          , allTestsDone -- this should always run last
          ]

readProto ::
  forall a .
  (HasCallStack, Message a, JSONPB.FromJSONPB a, Typeable a, ?format :: Format) =>
  IO a
readProto = fmap snd readBytesAndProto

readBytesAndProto ::
  forall a .
  (HasCallStack, Message a, JSONPB.FromJSONPB a, Typeable a, ?format :: Format) =>
  IO (B.ByteString, a)
readBytesAndProto = do
    length <- readLn
    bs <- BC.hGet stdin length
    case parse bs of
      Left err -> assertFailure $
        "readProto @" ++
        showsTypeRep (typeRep (Proxy :: Proxy a)) " (?format = " ++
        show ?format ++ "): (" ++ show bs ++ ") " ++
        err
      Right  x -> pure (bs, x)
  where
    parse = case ?format of
      Binary -> Bifunctor.first show . fromByteString @a
      Jsonpb -> JSONPB.eitherDecode @a . BL.fromStrict

expect ::
  ( HasCallStack, Eq a, Message a, JSONPB.FromJSONPB a, Show a, Typeable a
  , ?format :: Format
  ) =>
  a ->
  Assertion
expect v = do
  (bs, x) <- readBytesAndProto
  assertEqual ("result of decoding a " ++ showsTypeRep (typeOf x) " from " ++ show bs) v x

testCaseInFormat :: (?format :: Format) => String -> Assertion -> TestTree
testCaseInFormat = testCase . (++ ": format " ++ show ?format)

testCase1 = testCaseInFormat "Trivial message" $
    do Trivial { .. } <- readProto
       trivialTrivialField @?= 0x7BADBEEF

testCase2 = testCaseInFormat "Multi-field message" $
    do MultipleFields { .. } <- readProto

       multipleFieldsMultiFieldDouble @?= 1.125
       multipleFieldsMultiFieldFloat  @?= 1e9
       multipleFieldsMultiFieldInt32  @?= 0x1135
       multipleFieldsMultiFieldInt64  @?= 0x7FFAFABADDEAFFA0
       multipleFieldsMultiFieldString @?= "Goodnight moon"
       multipleFieldsMultiFieldBool   @?= False

testCaseSignedInts = testCaseInFormat "Signed integer types" $
    do expect (SignedInts 0 0)
       expect (SignedInts 42 84)
       expect (SignedInts (-42) (-84))
       expect (SignedInts minBound minBound)
       expect (SignedInts maxBound maxBound)

testCaseRepeatedSignedInts = testCaseInFormat "Repeated signed integer types" $
    do expect (WithRepeatedSigned [] [])
       expect (WithRepeatedSigned [0] [0])
       expect (WithRepeatedSigned
                 [0, 42, -42, 0x3FFFFFFF, -0x40000000, maxBound, minBound]
                 [0, 84, -84, 0x3FFFFFFFFFFFFFFF, -0x4000000000000000, maxBound, minBound])

testCase3 = testCaseInFormat "Nested enumeration" $
    do WithEnum { withEnumEnumField = Enumerated a } <- readProto
       a @?= Right WithEnum_TestEnumENUM1

       WithEnum { withEnumEnumField = Enumerated b } <- readProto
       b @?= Right WithEnum_TestEnumENUM2

       WithEnum { withEnumEnumField = Enumerated c } <- readProto
       c @?= Right WithEnum_TestEnumENUM3

       WithEnum { withEnumEnumField = Enumerated d } <- readProto
       d @?= Left 0xBEEF

testCase4 = testCaseInFormat "Nested message" $
    do WithNesting { withNestingNestedMessage = a } <- readProto
       a @?= Just (WithNesting_Nested "testCase4 nestedField1" 0xABCD [] [])

       WithNesting { withNestingNestedMessage = b } <- readProto
       b @?= Nothing

       WithNesting { withNestingNestedMessage = c } <- readProto
       c @?= Just (WithNesting_Nested "" 0 [] [])

testCase5 = testCaseInFormat "Nested repeated message" $
    do WithNestingRepeated { withNestingRepeatedNestedMessages = a } <- readProto
       length a @?= 3
       let [a1, a2, a3] = a

       a1 @?= WithNestingRepeated_Nested "testCase5 nestedField1" 0xDCBA [1, 1, 2, 3, 5] [0xB, 0xABCD, 0xBADBEEF, 0x10203040]
       a2 @?= WithNestingRepeated_Nested "Hello world" 0x7FFFFFFF [0, 0, 0] []
       a3 @?= WithNestingRepeated_Nested "" 0 [] []

       WithNestingRepeated { withNestingRepeatedNestedMessages = b } <- readProto
       b @?= []

testCase6 = testCaseInFormat "Nested repeated int message" $
    do WithNestingRepeatedInts { withNestingRepeatedIntsNestedInts = a } <- readProto
       a @?= [ NestedInts 636513 619021 ]

       WithNestingRepeatedInts { withNestingRepeatedIntsNestedInts = b } <- readProto
       b @?= []

       WithNestingRepeatedInts { withNestingRepeatedIntsNestedInts = c } <- readProto
       c @?= [ NestedInts 636513 619021
             , NestedInts 423549 687069
             , NestedInts 545506 143731
             , NestedInts 193605 385360 ]

testCase7 = testCaseInFormat "Repeated int32 field" $
    do WithRepetition { withRepetitionRepeatedField1 = a } <- readProto
       a @?= []

       WithRepetition { withRepetitionRepeatedField1 = b } <- readProto
       b @?= [1..10000]

testCase8 = testCaseInFormat "Fixed-width integer types" $
    do WithFixed { .. } <- readProto
       withFixedFixed1 @?= 0
       withFixedFixed2 @?= 0
       withFixedFixed3 @?= 0
       withFixedFixed4 @?= 0

       WithFixed { .. } <- readProto
       withFixedFixed1 @?= maxBound
       withFixedFixed2 @?= maxBound
       withFixedFixed3 @?= maxBound
       withFixedFixed4 @?= maxBound

       WithFixed { .. } <- readProto
       withFixedFixed1 @?= minBound
       withFixedFixed2 @?= minBound
       withFixedFixed3 @?= minBound
       withFixedFixed4 @?= minBound

testCase9 = testCaseInFormat "Bytes fields" $
    do WithBytes { .. } <- readProto
       withBytesBytes1 @?= "\x00\x00\x00\x01\x02\x03\xFF\xFF\x00\x01"
       withBytesBytes2 @?= ["", "\x01", "\xAB\xBAhello", "\xBB"]

       WithBytes { .. } <- readProto
       withBytesBytes1 @?= "Hello world"
       withBytesBytes2 @?= []

       WithBytes { .. } <- readProto
       withBytesBytes1 @?= ""
       withBytesBytes2 @?= ["Hello", "\x00world", "\x00\x00"]

       WithBytes { .. } <- readProto
       withBytesBytes1 @?= ""
       withBytesBytes2 @?= []

testCase10 = testCaseInFormat "Packed and unpacked repeated types" $
    do WithPacking { .. } <- readProto
       withPackingPacking1 @?= []
       withPackingPacking2 @?= []

       WithPacking { .. } <- readProto
       withPackingPacking1 @?= [100, 2000, 300, 4000, 500, 60000, 7000]
       withPackingPacking2 @?= []

       WithPacking { .. } <- readProto
       withPackingPacking1 @?= []
       withPackingPacking2 @?= [100, 2000, 300, 4000, 500, 60000, 7000]

       WithPacking { .. } <- readProto
       withPackingPacking1 @?= [1, 2, 3, 4, 5]
       withPackingPacking2 @?= [5, 4, 3, 2, 1]

testCase11 = testCaseInFormat "All possible packed types" $
    do a <- readProto
       a @?= AllPackedTypes [] [] [] [] [] [] [] [] [] [] [] [] []

       b <- readProto
       b @?= AllPackedTypes [1] [2] [3] [4] [5] [6] [7] [8] [9] [10] [False] [efld0] [efld0]

       c <- readProto
       c @?= AllPackedTypes [1] [2] [-3] [-4] [5] [6] [-7] [-8] [-9] [-10] [True] [efld1, efld2] [efld1, efld2]

       d <- readProto
       d @?= AllPackedTypes [1..10000] [1..10000]
                            [1..10000] [1..10000]
                            [1..10000] [1..10000]
                            [1,1.125..10000] [1,1.125..10000]
                            [1..10000] [1..10000]
                            [False,True]
                            [efld0,efld1,efld2]
                            [efld0,efld1,efld2]
    where
      efld0 = Enumerated (Right EFLD0)
      efld1 = Enumerated (Right EFLD1)
      efld2 = Enumerated (Left 2)


testCase12 = testCaseInFormat "Message with out of order field numbers" $
    do OutOfOrderFields { .. } <- readProto
       outOfOrderFieldsField1 @?= []
       outOfOrderFieldsField2 @?= ""
       outOfOrderFieldsField3 @?= maxBound
       outOfOrderFieldsField4 @?= []

       OutOfOrderFields { .. } <- readProto
       outOfOrderFieldsField1 @?= [1,7..100]
       outOfOrderFieldsField2 @?= "This is a test"
       outOfOrderFieldsField3 @?= minBound
       outOfOrderFieldsField4 @?= ["This", "is", "a", "test"]

testCase13 = testCaseInFormat "Nested message with the same name as another package-level message" $
    do ShadowedMessage { .. } <- readProto
       shadowedMessageName  @?= "name"
       shadowedMessageValue @?= 0x7DADBEEF

       MessageShadower { .. } <- readProto
       messageShadowerName @?= "another name"
       -- Until <https://github.com/awakesecurity/proto3-suite/issues/206>
       -- is fixed, the Haskell JSONPB parser will fail to find the this
       -- field under its lowerCamelCase name.  Once the fix is available
       -- we can make the following verification unconditional:
       when (?format /= Jsonpb) $
         messageShadowerShadowedMessage @?= Just (MessageShadower_ShadowedMessage "name" "string value")

       MessageShadower_ShadowedMessage { .. } <- readProto
       messageShadower_ShadowedMessageName  @?= "another name"
       messageShadower_ShadowedMessageValue @?= "another string"

testCase14 = testCaseInFormat "Qualified name resolution" $
    do WithQualifiedName { .. } <- readProto
       withQualifiedNameQname1 @?= Just (ShadowedMessage "int value" 42)
       withQualifiedNameQname2 @?= Just (MessageShadower_ShadowedMessage "string value" "hello world")

testCase15 = testCaseInFormat "Imported message resolution" $
    do TestProtoImport.WithNesting { .. } <- readProto
       withNestingNestedMessage1 @?= Just (TestProtoImport.WithNesting_Nested 1 2)
       withNestingNestedMessage2 @?= Nothing

testCase16 = testCaseInFormat "Proper resolution of shadowed message names" $
    do UsingImported { .. } <- readProto
       usingImportedImportedNesting @?=
         Just (TestProtoImport.WithNesting
                 (Just (TestProtoImport.WithNesting_Nested 1 2))
                 (Just (TestProtoImport.WithNesting_Nested 3 4)))
       usingImportedLocalNesting @?= Just (WithNesting (Just (WithNesting_Nested "field" 0xBEEF [] [])))

testCase17 = testCaseInFormat "Oneof" $ do
    -- Read default values for oneof subfields
    do TestProtoOneof.Something{ .. } <- readProto
       somethingValue   @?= 1
       somethingAnother @?= 2
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneName "")
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 3
       somethingAnother @?= 4
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneSomeid 0)
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 5
       somethingAnother @?= 6
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneDummyMsg1
                                    (TestProtoOneof.DummyMsg 0))

    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 7
       somethingAnother @?= 8
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneDummyMsg2
                                    (TestProtoOneof.DummyMsg 0))

    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 9
       somethingAnother @?= 10
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneDummyEnum
                                    (Enumerated (Right TestProtoOneof.DummyEnumDUMMY0)))
    -- Read non-default values for oneof subfields
    do TestProtoOneof.Something{ .. } <- readProto
       somethingValue   @?= 1
       somethingAnother @?= 2
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneName "hello world")
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 3
       somethingAnother @?= 4
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneSomeid 42)
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 5
       somethingAnother @?= 6
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneDummyMsg1
                                    (TestProtoOneof.DummyMsg 66))
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 7
       somethingAnother @?= 8
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneDummyMsg2
                                    (TestProtoOneof.DummyMsg 67))
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 9
       somethingAnother @?= 10
       somethingPickOne @?= Just (TestProtoOneof.SomethingPickOneDummyEnum
                                    (Enumerated (Right TestProtoOneof.DummyEnumDUMMY1)))
    -- Read with oneof not set
    do TestProtoOneof.Something { .. } <- readProto
       somethingValue   @?= 11
       somethingAnother @?= 12
       somethingPickOne @?= Nothing

testCase18 = testCaseInFormat "Imported Oneof" $ do
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneDummyMsg1
                                     (TestProtoOneof.DummyMsg 0))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneDummyMsg1
                                     (TestProtoOneof.DummyMsg 68))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneWithOneof
                                     (TestProtoOneofImport.WithOneof Nothing))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneWithOneof
                                     (TestProtoOneofImport.WithOneof
                                        (Just (TestProtoOneofImport.WithOneofPickOneA ""))))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneWithOneof
                                     (TestProtoOneofImport.WithOneof
                                        (Just (TestProtoOneofImport.WithOneofPickOneB 0))))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneWithOneof
                                     (TestProtoOneofImport.WithOneof
                                        (Just (TestProtoOneofImport.WithOneofPickOneA "foo"))))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Just (TestProtoOneof.WithImportedPickOneWithOneof
                                     (TestProtoOneofImport.WithOneof
                                        (Just (TestProtoOneofImport.WithOneofPickOneB 19))))
  do TestProtoOneof.WithImported{ .. } <- readProto
     withImportedPickOne @?= Nothing

testCase19 = testCaseInFormat "Maps" $ do
  result <- readProto
  let wt = Just . WrappedTrivial . Just . Trivial
  let expected = MapTest
        { mapTestPrim = M.fromList [("foo", 1), ("bar", 42), ("baz", 1234567)]
        , mapTestTrivial = M.fromList $
            [(1, wt 1), (2, wt 42), (101, wt 1234567), (79, Just (WrappedTrivial Nothing))] ++
            (if ?format == Jsonpb then [] else [(80, Nothing)])
              -- The python implementation forbids serialising map entries
              -- with 'None' as the value (dynamic type error).  Even if we
              -- merely reference key 80, the generated Python code will create
              -- an explicit submessage whose fields have default values.
              -- For binary encodings we can work around this limitation
              -- by replacing maps with equivalent repeated key-value pair
              -- submessages, but for JSONPB those are not equivalent.
        , mapTestSigned = M.fromList [(1, 2), (3, 4), (5, 6)]
        }
  result @?= expected

testCase_DoubleValue = testCaseInFormat "DoubleValue" $ do
  let check x y z = expect $
        TestProtoWrappers.TestDoubleValue x y (fmap TestProtoWrappers.TestDoubleValuePickOneOne z)
  check Nothing [4.75, 0, -7.125] Nothing
  check (Just 3.5) [] (Just 0.0)
  check (Just (-3.5)) [0.0, 0.0, 0.0] (Just (-1.75))

testCase_FloatValue = testCaseInFormat "FloatValue" $ do
  let check x y z = expect $
        TestProtoWrappers.TestFloatValue x y (fmap TestProtoWrappers.TestFloatValuePickOneOne z)
  check Nothing [] (Just 0.0)
  check (Just 2.5) [1.75, 0, -5.125] Nothing
  check (Just (-2.5)) [0.0, 0.0, 0.0] (Just (-1.25))

testCase_Int64Value = testCaseInFormat "Int64Value" $ do
  let check x y z = expect $
        TestProtoWrappers.TestInt64Value x y (fmap TestProtoWrappers.TestInt64ValuePickOneOne z)
  check Nothing [1, 0, -5] Nothing
  check (Just 0) [] (Just 5)
  check (Just maxBound) [minBound, 0, maxBound] (Just minBound)
  check (Just (-1)) [0, maxBound, minBound] (Just maxBound)
  check (Just minBound) [0, 0, 0] (Just 0)

testCase_UInt64Value = testCaseInFormat "UInt64Value" $ do
  let check x y z = expect $
        TestProtoWrappers.TestUInt64Value x y (fmap TestProtoWrappers.TestUInt64ValuePickOneOne z)
  check Nothing [1, 0, 5] Nothing
  check (Just 0) [] (Just 5)
  check (Just maxBound) [minBound, 0, maxBound] (Just minBound)
  check (Just 1) [0, maxBound, minBound] (Just maxBound)
  check (Just minBound) [0, 0, 0] (Just 0)

testCase_Int32Value = testCaseInFormat "Int32Value" $ do
  let check x y z = expect $
        TestProtoWrappers.TestInt32Value x y (fmap TestProtoWrappers.TestInt32ValuePickOneOne z)
  check Nothing [1, 0, -5] Nothing
  check (Just 0) [] (Just 5)
  check (Just maxBound) [minBound, 0, maxBound] (Just minBound)
  check (Just (-1)) [0, maxBound, minBound] (Just maxBound)
  check (Just minBound) [0, 0, 0] (Just 0)

testCase_UInt32Value = testCaseInFormat "UInt32Value" $ do
  let check x y z = expect $
        TestProtoWrappers.TestUInt32Value x y (fmap TestProtoWrappers.TestUInt32ValuePickOneOne z)
  check Nothing [1, 0, 5] Nothing
  check (Just 0) [] (Just 5)
  check (Just maxBound) [minBound, 0, maxBound] (Just minBound)
  check (Just 1) [0, maxBound, minBound] (Just maxBound)
  check (Just minBound) [0, 0, 0] (Just 0)

testCase_BoolValue = testCaseInFormat "BoolValue" $ do
  let check x y z = expect $
        TestProtoWrappers.TestBoolValue x y (fmap TestProtoWrappers.TestBoolValuePickOneOne z)
  check Nothing [False, True] Nothing
  check (Just False) [] (Just True)
  check (Just True) [True, False] (Just False)

testCase_StringValue = testCaseInFormat "StringValue" $ do
  let check x y z = expect $
        TestProtoWrappers.TestStringValue x y (fmap TestProtoWrappers.TestStringValuePickOneOne z)
  check Nothing ["abc", "", "def"] Nothing
  check (Just "") [] (Just "xyz")
  check (Just "abc") ["", "", ""] (Just "")

testCase_BytesValue = testCaseInFormat "BytesValue" $ do
  let check x y z = expect $
        TestProtoWrappers.TestBytesValue x y (fmap TestProtoWrappers.TestBytesValuePickOneOne z)
  check Nothing ["012", "", "345"] Nothing
  check (Just "") [] (Just "789")
  check (Just "012") ["", "", ""] (Just "")

testCase_NegativeEnum = testCaseInFormat "NegativeEnum" $ do
  let w = TestProtoNegativeEnum.WithNegativeEnum . Enumerated . Right
  expect (w TestProtoNegativeEnum.NegativeEnumNEGATIVE_ENUM_0)
  expect (w TestProtoNegativeEnum.NegativeEnumNEGATIVE_ENUM_NEGATIVE_1)
  expect (w TestProtoNegativeEnum.NegativeEnumNEGATIVE_ENUM_1)
  expect (w TestProtoNegativeEnum.NegativeEnumNEGATIVE_ENUM_NEGATIVE_128)
  expect (w TestProtoNegativeEnum.NegativeEnumNEGATIVE_ENUM_128)

testCase_Optional = testCaseInFormat "Optional" $ do
  let check ::
        HasCallStack =>
        (TestProtoOptional.WithOptional -> TestProtoOptional.WithOptional) ->
        Assertion
      check f = expect (f (def @TestProtoOptional.WithOptional))
  check (\m -> m)
  check (\m -> m { TestProtoOptional.withOptionalOptionalDouble = Just 0
                 , TestProtoOptional.withOptionalOptionalFloat = Just 1.0 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalDouble = Just 2.0
                 , TestProtoOptional.withOptionalOptionalFloat = Just 0 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalInt32 = Just 0
                 , TestProtoOptional.withOptionalOptionalInt64 = Just -64 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalInt32 = Just -32
                 , TestProtoOptional.withOptionalOptionalInt64 = Just 0 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalUint32 = Just 0
                 , TestProtoOptional.withOptionalOptionalUint64 = Just 0xFFFFFFFFFFFFFFBF })
  check (\m -> m { TestProtoOptional.withOptionalOptionalUint32 = Just 0xFFFFFFDF
                 , TestProtoOptional.withOptionalOptionalUint64 = Just 0 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalSint32 = Just 0
                 , TestProtoOptional.withOptionalOptionalSint64 = Just -64 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalSint32 = Just -32
                 , TestProtoOptional.withOptionalOptionalSint64 = Just 0 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalFixed32 = Just 0
                 , TestProtoOptional.withOptionalOptionalFixed64 = Just 0xFFFFFFFFFFFFFFBF })
  check (\m -> m { TestProtoOptional.withOptionalOptionalFixed32 = Just 0xFFFFFFDF
                 , TestProtoOptional.withOptionalOptionalFixed64 = Just 0 })
  check (\m -> m { TestProtoOptional.withOptionalOptionalSfixed32 = Just 0
                 , TestProtoOptional.withOptionalOptionalSfixed64 = Just -64})
  check (\m -> m { TestProtoOptional.withOptionalOptionalSfixed32 = Just -32
                 , TestProtoOptional.withOptionalOptionalSfixed64 = Just 0})
  check (\m -> m { TestProtoOptional.withOptionalOptionalBool = Just False
                 , TestProtoOptional.withOptionalOptionalString = Just "abc"
                 , TestProtoOptional.withOptionalOptionalBytes = Just "xyz" })
  check (\m -> m { TestProtoOptional.withOptionalOptionalBool = Just True
                 , TestProtoOptional.withOptionalOptionalString = Just ""
                 , TestProtoOptional.withOptionalOptionalBytes = Just "" })
  check (\m -> m { TestProtoOptional.withOptionalOptionalEnum = Just (Enumerated (Right TestProtoOptional.EnumUNKNOWN))
                 , TestProtoOptional.withOptionalOptionalSubmessage = Just (TestProtoOptional.Submessage 123) })
  check (\m -> m { TestProtoOptional.withOptionalOptionalEnum = Just (Enumerated (Right TestProtoOptional.EnumCode55)) })


allTestsDone = testCaseInFormat "Receive end of test suite sentinel message" $
   do MultipleFields{..} <- readProto
      multipleFieldsMultiFieldString @?= "All tests complete"
