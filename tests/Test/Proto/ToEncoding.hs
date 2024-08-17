{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Proto.ToEncoding
  ( ToEncoding(..)
  ) where

import Control.Category ((.))
import Prelude hiding ((.))

import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Vector
import qualified Proto3.Suite.Form.Encode as Form

import           TestProto
import qualified TestProtoImport
import qualified TestProtoOneof
import qualified TestProtoOneofImport
import qualified TestProtoWrappers

class ToEncoding a
  where
    toEncoding :: a -> Form.Encoding a

#if TYPE_LEVEL_FORMAT

instance ToEncoding Trivial
  where
    toEncoding (Trivial f1) = Form.messageFromFields @Trivial @'["trivialField"] $
      Form.leaf @"trivialField" f1

instance ToEncoding MultipleFields
  where
    toEncoding (MultipleFields f1 f2 f3 f4 f5 f6) = Form.messageFromFields $
      Form.leaf @"multiFieldDouble" f1 .
      Form.leaf @"multiFieldFloat" f2 .
      Form.leaf @"multiFieldInt32" f3 .
      Form.leaf @"multiFieldInt64" f4 .
      Form.leaf @"multiFieldString" f5 .
      Form.leaf @"multiFieldBool" f6

instance ToEncoding SignedInts
  where
    toEncoding (SignedInts f1 f2) = Form.messageFromFields $
      Form.leaf @"signed32" f1 .
      Form.leaf @"signed64" f2

instance ToEncoding WithEnum
  where
    toEncoding (WithEnum f1) = Form.messageFromFields $
      Form.leaf @"enumField" f1

instance ToEncoding WithNesting
  where
    toEncoding (WithNesting f1) = Form.messageFromFields $
      Form.optionalSubmessage @"nestedMessage" toEncoding f1

instance ToEncoding WithNesting_Nested
  where
    toEncoding (WithNesting_Nested f1 f2 f3 f4) = Form.messageFromFields $
      Form.leaf @"nestedField1" f1 .
      Form.leaf @"nestedField2" f2 .
      -- All of the following alternatives should work:
      case mod f2 3 of
        0 ->
          Form.leavesF @"nestedPacked" f3 .
          Form.leavesF @"nestedUnpacked" f4
        1 ->
          Form.leavesR @"nestedPacked" (reverse (toList f3)) .
          Form.leavesR @"nestedUnpacked" (reverse (toList f4))
        _ ->
          Form.leavesV @"nestedPacked" f3 .
          Form.leavesV @"nestedUnpacked" f4

instance ToEncoding WithNestingRepeated
  where
    toEncoding (WithNestingRepeated f1) = Form.messageFromFields $
      Form.submessagesV @"nestedMessages" toEncoding f1

instance ToEncoding WithNestingRepeated_Nested
  where
    toEncoding (WithNestingRepeated_Nested f1 f2 f3 f4) = Form.messageFromFields $
      Form.leaf @"nestedField1" f1 .
      Form.leaf @"nestedField2" f2 .
      Form.leavesV @"nestedPacked" f3 .
      Form.leavesV @"nestedUnpacked" f4

instance ToEncoding NestedInts
  where
    toEncoding (NestedInts f1 f2) = Form.messageFromFields $
      Form.leaf @"nestedInt1" f1 .
      Form.leaf @"nestedInt2" f2

instance ToEncoding WithNestingRepeatedInts
  where
    toEncoding (WithNestingRepeatedInts f1) = Form.messageFromFields $
      Form.submessagesV @"nestedInts" toEncoding f1

instance ToEncoding WithRepetition
  where
    toEncoding (WithRepetition f1) = Form.messageFromFields $
      Form.leavesV @"repeatedField1" f1

instance ToEncoding WithRepeatedSigned
  where
    toEncoding (WithRepeatedSigned f1 f2) = Form.messageFromFields $
      Form.leavesV @"r32" f1 .
      Form.leavesV @"r64" f2

instance ToEncoding WithFixed
  where
    toEncoding (WithFixed f1 f2 f3 f4) = Form.messageFromFields $
      Form.leaf @"fixed1" f1 .
      Form.leaf @"fixed2" f2 .
      Form.leaf @"fixed3" f3 .
      Form.leaf @"fixed4" f4

instance ToEncoding WithBytes
  where
    toEncoding (WithBytes f1 f2) = Form.messageFromFields $
      Form.leaf @"bytes1" f1 .
      Form.leavesV @"bytes2" f2

instance ToEncoding WithPacking
  where
    toEncoding (WithPacking f1 f2) = Form.messageFromFields $
      Form.leavesV @"packing1" f1 .
      Form.leavesV @"packing2" f2

instance ToEncoding AllPackedTypes
  where
    toEncoding (AllPackedTypes f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13) = Form.messageFromFields $
      Form.leavesV @"packedWord32" f1 .
      Form.leavesV @"packedWord64" f2 .
      Form.leavesV @"packedInt32" f3 .
      Form.leavesV @"packedInt64" f4 .
      Form.leavesV @"packedFixed32" f5 .
      Form.leavesV @"packedFixed64" f6 .
      Form.leavesV @"packedFloat" f7 .
      Form.leavesV @"packedDouble" f8 .
      Form.leavesV @"packedSFixed32" f9 .
      Form.leavesV @"packedSFixed64" f10 .
      Form.leavesV @"packedBool" f11 .
      Form.leavesV @"packedEnum" f12 .
      Form.leavesV @"unpackedEnum" f13

instance ToEncoding OutOfOrderFields
  where
    toEncoding (OutOfOrderFields f1 f2 f3 f4) = Form.messageFromFields $
      Form.leavesV @"field1" f1 .
      Form.leaf @"field2" f2 .
      Form.leaf @"field3" f3 .
      Form.leavesV @"field4" f4

instance ToEncoding ShadowedMessage
  where
    toEncoding (ShadowedMessage f1 f2) = Form.messageFromFields $
      Form.leaf @"name" f1 .
      Form.leaf @"value" f2

instance ToEncoding MessageShadower_ShadowedMessage
  where
    toEncoding (MessageShadower_ShadowedMessage f1 f2) = Form.messageFromFields $
      Form.leaf @"name" f1 .
      Form.leaf @"value" f2

instance ToEncoding MessageShadower
  where
    toEncoding (MessageShadower f1 f2) = Form.messageFromFields $
      Form.optionalSubmessage @"shadowed_message" toEncoding f1 .
      Form.leaf @"name" f2

instance ToEncoding WithQualifiedName
  where
    toEncoding (WithQualifiedName f1 f2) = Form.messageFromFields $
      Form.optionalSubmessage @"qname1" toEncoding f1 .
      Form.optionalSubmessage @"qname2" toEncoding f2

instance ToEncoding TestProtoImport.WithNesting_Nested
  where
    toEncoding (TestProtoImport.WithNesting_Nested f1 f2) = Form.messageFromFields $
      Form.leaf @"nestedField1" f1 .
      Form.leaf @"nestedField2" f2

instance ToEncoding TestProtoImport.WithNesting
  where
    toEncoding (TestProtoImport.WithNesting f1 f2) = Form.messageFromFields $
      Form.optionalSubmessage @"nestedMessage1" toEncoding f1 .
      Form.optionalSubmessage @"nestedMessage2" toEncoding f2

instance ToEncoding UsingImported
  where
    toEncoding (UsingImported f1 f2) = Form.messageFromFields $
      Form.optionalSubmessage @"importedNesting" toEncoding f1 .
      Form.optionalSubmessage @"localNesting" toEncoding f2

instance ToEncoding TestProtoOneof.DummyMsg
  where
    toEncoding (TestProtoOneof.DummyMsg f1) = Form.messageFromFields $
      Form.leaf @"dummy" f1

instance ToEncoding TestProtoOneofImport.AMessage
  where
    toEncoding (TestProtoOneofImport.AMessage f1 f2) = Form.messageFromFields $
      Form.leaf @"x" f1 .
      Form.leaf @"y" f2

instance ToEncoding TestProtoOneofImport.WithOneof
  where
    toEncoding (TestProtoOneofImport.WithOneof f1) = Form.messageFromFields $
      case f1 of
        Nothing ->
          Form.omitted @"pickOne"
        Just (TestProtoOneofImport.WithOneofPickOneA v) ->
          Form.leaf @"a" v
        Just (TestProtoOneofImport.WithOneofPickOneB v) ->
          Form.leaf @"b" v
        Just (TestProtoOneofImport.WithOneofPickOneC v) ->
          Form.submessage @"c" (toEncoding v)

instance ToEncoding TestProtoOneof.Something
  where
    toEncoding (TestProtoOneof.Something f1 f2 f3) = Form.messageFromFields $
      Form.leaf @"value" f1 .
      Form.leaf @"another" f2 .
      case f3 of
        Nothing ->
          Form.omitted @"pickOne"
        Just (TestProtoOneof.SomethingPickOneName v) ->
          Form.leaf @"name" v
        Just (TestProtoOneof.SomethingPickOneSomeid v) ->
          Form.leaf @"someid" v
        Just (TestProtoOneof.SomethingPickOneDummyMsg1 v) ->
          Form.submessage @"dummyMsg1" (toEncoding v)
        Just (TestProtoOneof.SomethingPickOneDummyMsg2 v) ->
          Form.submessage @"dummyMsg2" (toEncoding v)
        Just (TestProtoOneof.SomethingPickOneDummyEnum v) ->
          Form.leaf @"dummyEnum" v

instance ToEncoding TestProtoOneof.WithImported
  where
    toEncoding (TestProtoOneof.WithImported f1) = Form.messageFromFields $
      case f1 of
        Nothing ->
          Form.omitted @"pickOne"
        Just (TestProtoOneof.WithImportedPickOneDummyMsg1 v) ->
          Form.submessage @"dummyMsg1" (toEncoding v)
        Just (TestProtoOneof.WithImportedPickOneWithOneof v) ->
          Form.submessage @"withOneof" (toEncoding v)

instance ToEncoding TestProtoWrappers.TestDoubleValue
  where
    toEncoding (TestProtoWrappers.TestDoubleValue f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestFloatValue
  where
    toEncoding (TestProtoWrappers.TestFloatValue f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestInt64Value
  where
    toEncoding (TestProtoWrappers.TestInt64Value f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestUInt64Value
  where
    toEncoding (TestProtoWrappers.TestUInt64Value f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestInt32Value
  where
    toEncoding (TestProtoWrappers.TestInt32Value f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestUInt32Value
  where
    toEncoding (TestProtoWrappers.TestUInt32Value f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestBoolValue
  where
    toEncoding (TestProtoWrappers.TestBoolValue f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestStringValue
  where
    toEncoding (TestProtoWrappers.TestStringValue f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding TestProtoWrappers.TestBytesValue
  where
    toEncoding (TestProtoWrappers.TestBytesValue f1) = Form.messageFromFields $
      Form.wrapped @"wrapper" f1

instance ToEncoding WrappedTrivial
  where
    toEncoding (WrappedTrivial f1) = Form.messageFromFields $
      Form.optionalSubmessage @"trivial" toEncoding f1

instance ToEncoding MapTest
  where
    toEncoding (MapTest f1 f2 f3) = Form.messageFromFields $
        Form.submessagesF @"prim" assoc1 (M.toList f1) .
        Form.submessagesR @"trivial" assoc2 (M.toDescList f2) .
        Form.submessagesV @"signed" assoc3 (Data.Vector.fromList (M.toList f3))
      where
        assoc1 (k, v) = Form.messageFromFields $
          Form.leaf @"key" k .
          Form.leaf @"value" v
        assoc2 (k, v) = Form.messageFromFields $
          Form.leaf @"key" k .
          Form.optionalSubmessage @"value" toEncoding v
        assoc3 (k, v) = Form.messageFromFields $
          Form.leaf @"key" k .
          Form.leaf @"value" v

#endif
