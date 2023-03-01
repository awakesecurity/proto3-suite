{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import Test.Tasty
import Test.Tasty.HUnit (Assertion, (@?=), (@=?), testCase)
import Control.Monad (when)
import qualified Data.Map as M
import Proto3.Suite
import qualified Proto3.Suite.JSONPB as JSONPB
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import GHC.Stack (HasCallStack)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO
import Text.Read (readEither)

import TestProto
import qualified TestProtoImport
import qualified TestProtoOneof
import qualified TestProtoOneofImport
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

tests, testCase1, testCase2, testCaseSignedInts, testCase3, testCase4,
    testCase5, testCase6, testCase7, testCase8, testCase9, testCase10,
    testCase11, testCase12, testCase13, testCase14, testCase15,
    testCase16, testCase17, testCase18, testCase19,
    testCase_DoubleValue, testCase_FloatValue, testCase_Int64Value,
    testCase_UInt64Value, testCase_Int32Value, testCase_UInt32Value,
    testCase_BoolValue, testCase_StringValue, testCase_BytesValue,
    allTestsDone :: (?format :: Format) => TestTree
tests = testGroup
          ("Decode protobuf messages from Python: format " ++ show ?format)
          [ testCase1, testCase2, testCaseSignedInts
          , testCase3, testCase4, testCase5, testCase6
          , testCase7, testCase8, testCase9, testCase10
          , testCase11, testCase12, testCase13, testCase14
          , testCase15, testCase16, testCase17, testCase18
          , testCase19
          , testCase_DoubleValue, testCase_FloatValue, testCase_Int64Value
          , testCase_UInt64Value, testCase_Int32Value, testCase_UInt32Value
          , testCase_BoolValue, testCase_StringValue, testCase_BytesValue
          , allTestsDone -- this should always run last
          ]

readProto :: (Message a, JSONPB.FromJSONPB a, ?format :: Format) => IO a
readProto = do
    length <- readLn
    res <- parse <$> BC.hGet stdin length
    case res of
      Left err -> fail ("readProto: " ++ err)
      Right  x -> pure x
  where
    parse = case ?format of
      Binary -> Bifunctor.first show . fromByteString
      Jsonpb -> JSONPB.eitherDecode . BL.fromStrict

expect ::
  ( HasCallStack, Eq a, Message a, JSONPB.FromJSONPB a, Show a
  , ?format :: Format
  ) =>
  a ->
  Assertion
expect v = (v @=?) =<< readProto

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
  let expected = MapTest{ mapTestPrim = M.fromList [("foo", 1),("bar", 42),("baz", 1234567)]
                        -- The python implementation forbids serialising map entries
                        -- with 'None' as the value (dynamic type error).
                        , mapTestTrivial = M.fromList [(1, wt 1),(2, wt 42),(101, wt 1234567), (79, Just (WrappedTrivial Nothing))]
                        , mapTestSigned = M.fromList [(1,2),(3,4),(5,6)]
                        }
  result @?= expected

testCase_DoubleValue = testCaseInFormat "DoubleValue" $ do
  let w = TestProtoWrappers.TestDoubleValue
  expect (w Nothing)
  expect (w (Just 3.5))
 
testCase_FloatValue = testCaseInFormat "FloatValue" $ do
  let w = TestProtoWrappers.TestFloatValue
  expect (w Nothing)
  expect (w (Just 2.5))

testCase_Int64Value = testCaseInFormat "Int64Value" $ do
  let w = TestProtoWrappers.TestInt64Value
  expect (w Nothing)
  expect (w (Just 0))
  expect (w (Just maxBound))
  expect (w (Just (-1)))
  expect (w (Just minBound))

testCase_UInt64Value = testCaseInFormat "UInt64Value" $ do
  let w = TestProtoWrappers.TestUInt64Value
  expect (w Nothing)
  expect (w (Just 0))
  expect (w (Just maxBound))

testCase_Int32Value = testCaseInFormat "Int32Value" $ do
  let w = TestProtoWrappers.TestInt32Value
  expect (w Nothing)
  expect (w (Just 0))
  expect (w (Just maxBound))
  expect (w (Just (-1)))
  expect (w (Just minBound))

testCase_UInt32Value = testCaseInFormat "UInt32Value" $ do
  let w = TestProtoWrappers.TestUInt32Value
  expect (w Nothing)
  expect (w (Just 0))
  expect (w (Just maxBound))

testCase_BoolValue = testCaseInFormat "BoolValue" $ do
  let w = TestProtoWrappers.TestBoolValue
  expect (w Nothing)
  expect (w (Just False))
  expect (w (Just True))

testCase_StringValue = testCaseInFormat "StringValue" $ do
  let w = TestProtoWrappers.TestStringValue
  expect (w Nothing)
  expect (w (Just ""))
  expect (w (Just "abc"))

testCase_BytesValue = testCaseInFormat "BytesValue" $ do
  let w = TestProtoWrappers.TestBytesValue
  expect (w Nothing)
  expect (w (Just ""))
  expect (w (Just "012"))


allTestsDone = testCaseInFormat "Receive end of test suite sentinel message" $
   do MultipleFields{..} <- readProto
      multipleFieldsMultiFieldString @?= "All tests complete"
