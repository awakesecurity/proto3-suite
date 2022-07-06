{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import           Proto3.Suite
import qualified Proto3.Suite.JSONPB as JSONPB
import           TestProto
import qualified TestProtoImport
import qualified TestProtoOneof
import qualified TestProtoOneofImport
import qualified TestProtoWrappers
import           Text.Read (readEither)
import           System.Environment (getArgs, getProgName)
import           System.Exit (die)

data Format = Binary | Jsonpb
  deriving (Eq, Read, Show)

outputMessage :: (Message a, JSONPB.ToJSONPB a, ?format :: Format) => a -> IO ()
outputMessage msg = putStrLn (show (BL.length encoded)) >> BL.putStr encoded
  where
    encoded = case ?format of
      Binary -> toLazyByteString msg
      Jsonpb -> JSONPB.encode JSONPB.jsonPBOptions msg

testCase1 :: (?format :: Format) => IO ()
testCase1 =
  let trivial = Trivial 0x7BADBEEF
  in outputMessage trivial

testCase2 :: (?format :: Format) => IO ()
testCase2 =
  do outputMessage (MultipleFields 1.125 1e9 0x1135 0x7FFAFABADDEAFFA0 "Goodnight moon" False)

testCaseSignedInts :: (?format :: Format) => IO ()
testCaseSignedInts =
  do outputMessage (SignedInts 0 0)
     outputMessage (SignedInts 42 84)
     outputMessage (SignedInts (-42) (-84))
     outputMessage (SignedInts minBound minBound)
     outputMessage (SignedInts maxBound maxBound)

testCase3 :: (?format :: Format) => IO ()
testCase3 =
  do outputMessage (WithEnum (Enumerated (Right WithEnum_TestEnumENUM1)))
     outputMessage (WithEnum (Enumerated (Right WithEnum_TestEnumENUM2)))
     outputMessage (WithEnum (Enumerated (Right WithEnum_TestEnumENUM3)))
     outputMessage (WithEnum (Enumerated (Left 0xBEEF)))

testCase4 :: (?format :: Format) => IO ()
testCase4 =
  do let nested = WithNesting_Nested "testCase4 nestedField1" 0xABCD [] []
     outputMessage (WithNesting (Just nested))
     outputMessage (WithNesting Nothing)

testCase5 :: (?format :: Format) => IO ()
testCase5 =
  do let nested1 = WithNestingRepeated_Nested "testCase5 nestedField1" 0xDCBA [1, 1, 2, 3, 5] [0xB, 0xABCD, 0xBADBEEF, 0x10203040]
         nested2 = WithNestingRepeated_Nested "Hello world" 0x7FFFFFFF [0, 0, 0] []
         nested3 = WithNestingRepeated_Nested "" 0x0 [] []

     outputMessage (WithNestingRepeated [nested1, nested2, nested3])
     outputMessage (WithNestingRepeated [])

testCase6 :: (?format :: Format) => IO ()
testCase6 =
  do let nested1 = NestedInts 636513 619021
         nested2 = NestedInts 423549 687069
         nested3 = NestedInts 545506 143731
         nested4 = NestedInts 193605 385360
     outputMessage (WithNestingRepeatedInts [nested1])
     outputMessage (WithNestingRepeatedInts [])
     outputMessage (WithNestingRepeatedInts [nested1, nested2, nested3, nested4])

testCase7 :: (?format :: Format) => IO ()
testCase7 =
  do outputMessage (WithRepetition [])
     outputMessage (WithRepetition [1..10000])

testCase8 :: (?format :: Format) => IO ()
testCase8 =
  do outputMessage (WithFixed 0 0 0 0)
     outputMessage (WithFixed maxBound maxBound maxBound maxBound)
     outputMessage (WithFixed minBound minBound minBound minBound)

testCase9 :: (?format :: Format) => IO ()
testCase9 =
  do outputMessage (WithBytes "\x00\x00\x00\x01\x02\x03\xFF\xFF\x0\x1"
                              ["", "\x01", "\xAB\xBAhello", "\xBB"])
     outputMessage (WithBytes "Hello world" [])
     outputMessage (WithBytes "" ["Hello", "\x00world", "\x00\x00"])
     outputMessage (WithBytes "" [])

testCase10 :: (?format :: Format) => IO ()
testCase10 =
  do outputMessage (WithPacking [] [])
     outputMessage (WithPacking [100, 2000, 300, 4000, 500, 60000, 7000] [])
     outputMessage (WithPacking [] [100, 2000, 300, 4000, 500, 60000, 7000])
     outputMessage (WithPacking [1, 2, 3, 4, 5] [5, 4, 3, 2, 1])

testCase11 :: (?format :: Format) => IO ()
testCase11 = do
  outputMessage $ AllPackedTypes [] [] [] [] [] [] [] [] [] [] [] [] []
  outputMessage $ AllPackedTypes [1] [2] [3] [4] [5] [6] [7] [8] [9] [10]
                                 [False][efld0] [efld0]
  outputMessage $ AllPackedTypes [1] [2] [-3] [-4] [5] [6] [-7] [-8] [-9] [-10]
                                 [True] [efld1, efld2] [efld1, efld2]
  outputMessage $ AllPackedTypes [1..10000] [1..10000]
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

testCase12 :: (?format :: Format) => IO ()
testCase12 =
  do outputMessage (OutOfOrderFields [] "" maxBound [])
     outputMessage (OutOfOrderFields [1,7..100] "This is a test" minBound ["This", "is", "a", "test"])

testCase13 :: (?format :: Format) => IO ()
testCase13 =
  do outputMessage (ShadowedMessage "name" 0x7DADBEEF)
     outputMessage (MessageShadower (Just (MessageShadower_ShadowedMessage "name" "string value")) "another name")
     outputMessage (MessageShadower_ShadowedMessage "another name" "another string")

testCase14 :: (?format :: Format) => IO ()
testCase14 =
  outputMessage (WithQualifiedName (Just (ShadowedMessage "int value" 42))
                                   (Just (MessageShadower_ShadowedMessage "string value" "hello world")))

testCase15 :: (?format :: Format) => IO ()
testCase15 =
  outputMessage
    TestProtoImport.WithNesting
      { TestProtoImport.withNestingNestedMessage1 =
          Just TestProtoImport.WithNesting_Nested
            { TestProtoImport.withNesting_NestedNestedField1 = 1
            , TestProtoImport.withNesting_NestedNestedField2 = 2
            }
      , TestProtoImport.withNestingNestedMessage2 = Nothing
      }

testCase16 :: (?format :: Format) => IO ()
testCase16 =
  outputMessage (UsingImported { usingImportedImportedNesting =
                                   Just (TestProtoImport.WithNesting
                                          (Just (TestProtoImport.WithNesting_Nested 1 2))
                                          (Just (TestProtoImport.WithNesting_Nested 3 4)))
                               , usingImportedLocalNesting =
                                   Just (WithNesting (Just (WithNesting_Nested "field" 0xBEEF [] []))) })

testCase17 :: (?format :: Format) => IO ()
testCase17 = do
  let emit v a p = outputMessage
                     TestProtoOneof.Something
                       { TestProtoOneof.somethingValue   = v
                       , TestProtoOneof.somethingAnother = a
                       , TestProtoOneof.somethingPickOne = p
                       }
  -- Send default values for oneof subfields
  emit 1 2  $ Just $ TestProtoOneof.SomethingPickOneName ""
  emit 3 4  $ Just $ TestProtoOneof.SomethingPickOneSomeid 0
  emit 5 6  $ Just $ TestProtoOneof.SomethingPickOneDummyMsg1 $ TestProtoOneof.DummyMsg 0
  emit 7 8  $ Just $ TestProtoOneof.SomethingPickOneDummyMsg2 $ TestProtoOneof.DummyMsg 0
  emit 9 10 $ Just $ TestProtoOneof.SomethingPickOneDummyEnum $ Enumerated $ Right $ TestProtoOneof.DummyEnumDUMMY0

  -- Send non-default values for oneof subfields
  emit 1 2  $ Just $ TestProtoOneof.SomethingPickOneName "hello world"
  emit 3 4  $ Just $ TestProtoOneof.SomethingPickOneSomeid 42
  emit 5 6  $ Just $ TestProtoOneof.SomethingPickOneDummyMsg1 $ TestProtoOneof.DummyMsg 66
  emit 7 8  $ Just $ TestProtoOneof.SomethingPickOneDummyMsg2 $ TestProtoOneof.DummyMsg 67
  emit 9 10 $ Just $ TestProtoOneof.SomethingPickOneDummyEnum $ Enumerated $ Right $ TestProtoOneof.DummyEnumDUMMY1

  -- Send with oneof not set
  emit 11 12 Nothing

testCase18 :: (?format :: Format) => IO ()
testCase18 = do
  let emit          = outputMessage . TestProtoOneof.WithImported
  let emitWithOneof = emit . Just . TestProtoOneof.WithImportedPickOneWithOneof . TestProtoOneofImport.WithOneof
  emit $ Just $ TestProtoOneof.WithImportedPickOneDummyMsg1 $ TestProtoOneof.DummyMsg 0
  emit $ Just $ TestProtoOneof.WithImportedPickOneDummyMsg1 $ TestProtoOneof.DummyMsg 68
  emitWithOneof Nothing
  emitWithOneof $ Just $ TestProtoOneofImport.WithOneofPickOneA ""
  emitWithOneof $ Just $ TestProtoOneofImport.WithOneofPickOneB 0
  emitWithOneof $ Just $ TestProtoOneofImport.WithOneofPickOneA "foo"
  emitWithOneof $ Just $ TestProtoOneofImport.WithOneofPickOneB 19
  emit Nothing

-- Python support for null mapped values in JSONPB format seems to be broken;
-- therefore we do not emit mapped Nothing values in that format.
testCase19 :: (?format :: Format) => IO ()
testCase19 = do
  let wt = Just . WrappedTrivial . Just . Trivial
  outputMessage MapTest{ mapTestPrim = M.fromList [("foo", 1),("bar", 42),("baz", 1234567)]
                       , mapTestTrivial = M.fromList $ [(1, wt 1),(2, wt 42),(101, wt 1234567)] ++ (if ?format == Jsonpb then [] else [(79, Nothing)])
                       , mapTestSigned = M.fromList [(1,2),(3,4),(5,6)]
                       }

testCase_DoubleValue :: (?format :: Format) => IO ()
testCase_DoubleValue = do
  let emit = outputMessage . TestProtoWrappers.TestDoubleValue
  emit Nothing
  emit (Just 3.5)

testCase_FloatValue :: (?format :: Format) => IO ()
testCase_FloatValue = do
  let emit = outputMessage . TestProtoWrappers.TestFloatValue
  emit Nothing
  emit (Just 2.5)

testCase_Int64Value :: (?format :: Format) => IO ()
testCase_Int64Value = do
  let emit = outputMessage . TestProtoWrappers.TestInt64Value
  emit Nothing
  emit (Just 0)
  emit (Just maxBound)
  emit (Just (-1))
  emit (Just minBound)

testCase_UInt64Value :: (?format :: Format) => IO ()
testCase_UInt64Value = do
  let emit = outputMessage . TestProtoWrappers.TestUInt64Value
  emit Nothing
  emit (Just 0)
  emit (Just maxBound)

testCase_Int32Value :: (?format :: Format) => IO ()
testCase_Int32Value = do
  let emit = outputMessage . TestProtoWrappers.TestInt32Value
  emit Nothing
  emit (Just 0)
  emit (Just maxBound)
  emit (Just (-1))
  emit (Just minBound)

testCase_UInt32Value :: (?format :: Format) => IO ()
testCase_UInt32Value = do
  let emit = outputMessage . TestProtoWrappers.TestUInt32Value
  emit Nothing
  emit (Just 0)
  emit (Just maxBound)

testCase_BoolValue :: (?format :: Format) => IO ()
testCase_BoolValue = do
  let emit = outputMessage . TestProtoWrappers.TestBoolValue
  emit Nothing
  emit (Just False)
  emit (Just True)

testCase_StringValue :: (?format :: Format) => IO ()
testCase_StringValue = do
  let emit = outputMessage . TestProtoWrappers.TestStringValue
  emit Nothing
  emit (Just "")
  emit (Just "abc")

testCase_BytesValue :: (?format :: Format) => IO ()
testCase_BytesValue = do
  let emit = outputMessage . TestProtoWrappers.TestBytesValue
  emit Nothing
  emit (Just "")
  emit (Just "012")


main :: IO ()
main = do
  args <- getArgs
  format <- case map readEither args of
    [Right f] -> pure f
    _ -> do
      n <- getProgName
      die $ "Usage: " ++ n ++ " (Binary|Jsonpb)"
  let ?format = format

  testCase1
  testCase2
  testCaseSignedInts
  testCase3
  testCase4
  testCase5
  testCase6
  testCase7
  testCase8
  testCase9
  testCase10
  testCase11
  testCase12
  testCase13
  testCase14

  -- Tests using imported messages
  testCase15
  testCase16

  -- Oneof tests
  testCase17
  testCase18

  -- Map tests
  testCase19

  -- Wrappers
  testCase_DoubleValue
  testCase_FloatValue
  testCase_Int64Value
  testCase_UInt64Value
  testCase_Int32Value
  testCase_UInt32Value
  testCase_BoolValue
  testCase_StringValue
  testCase_BytesValue

  outputMessage (MultipleFields 0 0 0 0 "All tests complete" False)
