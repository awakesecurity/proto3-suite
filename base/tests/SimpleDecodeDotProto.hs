{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit (Assertion, (@?=), (@=?), testCase)
import qualified Data.Map as M
import Proto3.Suite
import qualified Data.ByteString.Char8 as BC
import System.IO

import qualified TestProto
import qualified TestProtoImport
import qualified TestProtoOneof
import qualified TestProtoOneofImport

main :: IO ()
main = do putStr "\n"
          defaultMain tests

tests, testCase1, testCase2, testCaseSignedInts, testCase3, testCase4,
    testCase5, testCase6, testCase7, testCase8, testCase9, testCase10,
    testCase11, testCase12, testCase13, testCase14, testCase15, testCase16,
    testCase17, testCase18, testCase19,
    allTestsDone :: TestTree
tests = testGroup "Decode protobuf messages from Python"
          [  testCase1,  testCase2, testCaseSignedInts
          ,  testCase3,  testCase4,  testCase5,  testCase6
          ,  testCase7,  testCase8,  testCase9, testCase10
          , testCase11, testCase12, testCase13, testCase14
          , testCase15, testCase16, testCase17, testCase18
          , testCase19
          , allTestsDone -- this should always run last
          ]

readProto :: Message a => IO a
readProto = do len <- readLn
               res <- fromByteString <$> BC.hGet stdin len
               case res of
                 Left err -> fail ("readProto: " ++ show err)
                 Right  x -> pure x

expect :: (Eq a, Message a, Show a) => a -> Assertion
expect v = (v @=?) =<< readProto

testCase1  = testCase "Trivial message" $
    do TestProto.Trivial { .. } <- readProto
       trivialTrivialField @?= 0x7BADBEEF

testCase2  = testCase "Multi-field message" $
    do TestProto.MultipleFields { .. } <- readProto

       multipleFieldsMultiFieldDouble @?= 1.125
       multipleFieldsMultiFieldFloat  @?= 1e9
       multipleFieldsMultiFieldInt32  @?= 0x1135
       multipleFieldsMultiFieldInt64  @?= 0x7FFAFABADDEAFFA0
       multipleFieldsMultiFieldString @?= "Goodnight moon"
       multipleFieldsMultiFieldBool   @?= False

testCaseSignedInts = testCase "Signed integer types" $
    do expect (TestProto.SignedInts 0 0)
       expect (TestProto.SignedInts 42 84)
       expect (TestProto.SignedInts (-42) (-84))
       expect (TestProto.SignedInts minBound minBound)
       expect (TestProto.SignedInts maxBound maxBound)

testCase3  = testCase "Nested enumeration" $
    do TestProto.WithEnum { withEnumEnumField = Enumerated a } <- readProto
       a @?= Right TestProto.WithEnum_TestEnumENUM1

       TestProto.WithEnum { withEnumEnumField = Enumerated b } <- readProto
       b @?= Right TestProto.WithEnum_TestEnumENUM2

       TestProto.WithEnum { withEnumEnumField = Enumerated c } <- readProto
       c @?= Right TestProto.WithEnum_TestEnumENUM3

       TestProto.WithEnum { withEnumEnumField = Enumerated d } <- readProto
       d @?= Left 0xBEEF

testCase4  = testCase "Nested message" $
    do TestProto.WithNesting { withNestingNestedMessage = a } <- readProto
       a @?= Just
         (TestProto.WithNesting_Nested "testCase4 nestedField1" 0xABCD [] [])

       TestProto.WithNesting { withNestingNestedMessage = b } <- readProto
       b @?= Nothing

testCase5  = testCase "Nested repeated message" $
    do TestProto.WithNestingRepeated
         { withNestingRepeatedNestedMessages = a } <- readProto
       length a @?= 3
       let [a1, a2, a3] = a

       a1 @?= TestProto.WithNestingRepeated_Nested
         "testCase5 nestedField1" 0xDCBA [1, 1, 2, 3, 5]
         [0xB, 0xABCD, 0xBADBEEF, 0x10203040]
       a2 @?= TestProto.WithNestingRepeated_Nested
         "Hello world" 0x7FFFFFFF [0, 0, 0] []
       a3 @?= TestProto.WithNestingRepeated_Nested
         "" 0 [] []

       TestProto.WithNestingRepeated
         { withNestingRepeatedNestedMessages = b } <- readProto
       b @?= []

testCase6  = testCase "Nested repeated int message" $
    do TestProto.WithNestingRepeatedInts
         { withNestingRepeatedIntsNestedInts = a } <- readProto
       a @?= [ TestProto.NestedInts 636513 619021 ]

       TestProto.WithNestingRepeatedInts
         { withNestingRepeatedIntsNestedInts = b } <- readProto
       b @?= []

       TestProto.WithNestingRepeatedInts
         { withNestingRepeatedIntsNestedInts = c } <- readProto
       c @?= [ TestProto.NestedInts 636513 619021
             , TestProto.NestedInts 423549 687069
             , TestProto.NestedInts 545506 143731
             , TestProto.NestedInts 193605 385360 ]

testCase7  = testCase "Repeated int32 field" $
    do TestProto.WithRepetition
         { withRepetitionRepeatedField1 = a } <- readProto
       a @?= []

       TestProto.WithRepetition
         { withRepetitionRepeatedField1 = b } <- readProto
       b @?= [1..10000]

testCase8  = testCase "Fixed-width integer types" $
    do
       do TestProto.WithFixed { .. } <- readProto
          withFixedFixed1 @?= 0
          withFixedFixed2 @?= 0
          withFixedFixed3 @?= 0
          withFixedFixed4 @?= 0

       do TestProto.WithFixed { .. } <- readProto
          withFixedFixed1 @?= maxBound
          withFixedFixed2 @?= maxBound
          withFixedFixed3 @?= maxBound
          withFixedFixed4 @?= maxBound

       do TestProto.WithFixed { .. } <- readProto
          withFixedFixed1 @?= minBound
          withFixedFixed2 @?= minBound
          withFixedFixed3 @?= minBound
          withFixedFixed4 @?= minBound

testCase9  = testCase "Bytes fields" $
    do
       do TestProto.WithBytes { .. } <- readProto
          withBytesBytes1 @?= "\x00\x00\x00\x01\x02\x03\xFF\xFF\x00\x01"
          withBytesBytes2 @?= ["", "\x01", "\xAB\xBAhello", "\xBB"]

       do TestProto.WithBytes { .. } <- readProto
          withBytesBytes1 @?= "Hello world"
          withBytesBytes2 @?= []

       do TestProto.WithBytes { .. } <- readProto
          withBytesBytes1 @?= ""
          withBytesBytes2 @?= ["Hello", "\x00world", "\x00\x00"]

       do TestProto.WithBytes { .. } <- readProto
          withBytesBytes1 @?= ""
          withBytesBytes2 @?= []

testCase10 = testCase "Packed and unpacked repeated types" $
    do
       do TestProto.WithPacking { .. } <- readProto
          withPackingPacking1 @?= []
          withPackingPacking2 @?= []

       do TestProto.WithPacking { .. } <- readProto
          withPackingPacking1 @?= [100, 2000, 300, 4000, 500, 60000, 7000]
          withPackingPacking2 @?= []

       do
          TestProto.WithPacking { .. } <- readProto
          withPackingPacking1 @?= []
          withPackingPacking2 @?= [100, 2000, 300, 4000, 500, 60000, 7000]

       do TestProto.WithPacking { .. } <- readProto
          withPackingPacking1 @?= [1, 2, 3, 4, 5]
          withPackingPacking2 @?= [5, 4, 3, 2, 1]

testCase11 = testCase "All possible packed types" $
    do a <- readProto
       a @?= TestProto.AllPackedTypes [] [] [] [] [] [] [] [] [] [] [] [] []

       b <- readProto
       b @?= TestProto.AllPackedTypes
         [1] [2] [3] [4] [5] [6] [7] [8] [9] [10] [False] [efld0] [efld0]

       c <- readProto
       c @?= TestProto.AllPackedTypes
         [1] [2] [-3] [-4] [5] [6] [-7] [-8] [-9] [-10] [True] [efld1] [efld1]

       d <- readProto
       d @?= TestProto.AllPackedTypes
         [1..10000] [1..10000]
         [1..10000] [1..10000]
         [1..10000] [1..10000]
         [1,1.125..10000] [1,1.125..10000]
         [1..10000] [1..10000]
         [False,True]
         [efld0,efld1]
         [efld0,efld1]
    where
      efld0 = Enumerated (Right TestProto.EFLD0)
      efld1 = Enumerated (Right TestProto.EFLD1)


testCase12 = testCase "Message with out of order field numbers" $
    do
       do TestProto.OutOfOrderFields { .. } <- readProto
          outOfOrderFieldsField1 @?= []
          outOfOrderFieldsField2 @?= ""
          outOfOrderFieldsField3 @?= maxBound
          outOfOrderFieldsField4 @?= []

       do TestProto.OutOfOrderFields { .. } <- readProto
          outOfOrderFieldsField1 @?= [1,7..100]
          outOfOrderFieldsField2 @?= "This is a test"
          outOfOrderFieldsField3 @?= minBound
          outOfOrderFieldsField4 @?= ["This", "is", "a", "test"]

testCase13 = testCase "Nested message with the same name as another package-level message" $
    do TestProto.ShadowedMessage { .. } <- readProto
       shadowedMessageName  @?= "name"
       shadowedMessageValue @?= 0x7DADBEEF

       TestProto.MessageShadower { .. } <- readProto
       messageShadowerName @?= "another name"
       messageShadowerShadowedMessage @?=
         Just (TestProto.MessageShadower_ShadowedMessage "name" "string value")

       TestProto.MessageShadower_ShadowedMessage { .. } <- readProto
       messageShadower_ShadowedMessageName  @?= "another name"
       messageShadower_ShadowedMessageValue @?= "another string"

testCase14 = testCase "Qualified name resolution" $
    do TestProto.WithQualifiedName { .. } <- readProto
       withQualifiedNameQname1 @?=
         Just (TestProto.ShadowedMessage "int value" 42)
       withQualifiedNameQname2 @?=
         Just (TestProto.MessageShadower_ShadowedMessage
                 "string value" "hello world")

testCase15 = testCase "Imported message resolution" $
    do TestProtoImport.WithNesting { .. } <- readProto
       withNestingNestedMessage1 @?=
         Just (TestProtoImport.WithNesting_Nested 1 2)
       withNestingNestedMessage2 @?=
         Nothing

testCase16 = testCase "Proper resolution of shadowed message names" $
    do TestProto.UsingImported { .. } <- readProto
       usingImportedImportedNesting @?=
         Just (TestProtoImport.WithNesting
                 (Just (TestProtoImport.WithNesting_Nested 1 2))
                 (Just (TestProtoImport.WithNesting_Nested 3 4)))
       usingImportedLocalNesting @?=
         Just (TestProto.WithNesting
                 (Just (TestProto.WithNesting_Nested "field" 0xBEEF [] [])))

testCase17 = testCase "Oneof" $ do
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

testCase18 = testCase "Imported Oneof" $ do
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

testCase19 = testCase "Maps" $ do
  result <- readProto
  let wt = Just . TestProto.WrappedTrivial . Just . TestProto.Trivial
  let expected = TestProto.MapTest
        { mapTestPrim = M.fromList [("foo", 1),("bar", 42),("baz", 1234567)]
          -- The python implementation forbids serialising map entries
          -- with 'None' as the value (dynamic type error).
        , mapTestTrivial = M.fromList [(1, wt 1),(2, wt 42),(101, wt 1234567), (79, Just (TestProto.WrappedTrivial Nothing))]
        , mapTestSigned = M.fromList [(1,2),(3,4),(5,6)]
        }
  result @?= expected

allTestsDone = testCase "Receive end of test suite sentinel message" $
   do TestProto.MultipleFields{..} <- readProto
      multipleFieldsMultiFieldString @?= "All tests complete"
