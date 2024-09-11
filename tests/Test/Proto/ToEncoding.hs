{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Proto.ToEncoding
  ( Iterator(..)
  , ToEncoding(..)
  ) where

import Control.Category ((.))
import Prelude hiding ((.))

import Data.Foldable (toList)
import qualified Data.Functor.Identity as Functor (Identity(..))
import qualified Data.Map as M
import qualified Data.Vector
import qualified Proto3.Suite.Form as Form
import qualified Proto3.Suite.Form.Encode as FormE

import           TestProto
import qualified TestProtoImport
import qualified TestProtoOneof
import qualified TestProtoOneofImport
import qualified TestProtoWrappers

-- | Which kind of iteration we should test for repeated fields.
data Iterator
  = Identity
  | Forward
  | Reverse
  | Vector
  deriving stock (Bounded, Enum, Eq, Read, Show)

class ToEncoding a
  where
    toEncoding :: (?iterator :: Iterator) => a -> FormE.MessageEncoding a

#if TYPE_LEVEL_FORMAT

repeated ::
  forall name a b message names .
  ( ?iterator :: Iterator
  , FormE.Occupy message name names ~ names
  , FormE.Field name (Functor.Identity b) message
  , FormE.Field name (FormE.Forward b) message
  , FormE.Field name (FormE.Reverse b) message
  , FormE.Field name (FormE.Vector b) message
  ) =>
  (a -> b) ->
  Data.Vector.Vector a ->
  FormE.Prefix message names names
repeated f = case ?iterator of
  Identity -> FormE.foldPrefixes . FormE.Vector (FormE.field @name . Functor.Identity . f)
  Forward -> FormE.field @name . FormE.Forward f . toList
  Reverse -> FormE.field @name . FormE.Reverse f . reverse . toList
  Vector -> FormE.field @name . FormE.Vector f

associations ::
  forall name k v key value message names .
  ( Form.ProtoTypeOf message name ~ 'Form.Map key value
  , Form.RepetitionOf message name ~ 'Form.Repeated 'Form.Unpacked
  , ?iterator :: Iterator
  , FormE.Field name (Functor.Identity (FormE.MessageEncoding (Form.Association key value))) message
  , FormE.Field name (FormE.Forward (FormE.MessageEncoding (Form.Association key value))) message
  , FormE.Field name (FormE.Reverse (FormE.MessageEncoding (Form.Association key value))) message
  , FormE.Field name (FormE.Vector (FormE.MessageEncoding (Form.Association key value))) message
  , FormE.KnownFieldNumber message name
  ) =>
  ((k, v) -> FormE.MessageEncoding (Form.Association key value)) ->
  M.Map k v ->
  FormE.Prefix message names names
associations f = case ?iterator of
  Identity -> FormE.foldPrefixes . FormE.Forward (FormE.associations @name . Functor.Identity . f) . M.toAscList
  Forward -> FormE.associations @name . FormE.Forward f . M.toAscList
  Reverse -> FormE.associations @name . FormE.Reverse f . M.toDescList
  Vector -> FormE.associations @name . FormE.Vector f . Data.Vector.fromList . M.toAscList

instance ToEncoding Trivial
  where
    toEncoding (Trivial f1) = FormE.fieldsToMessage @Trivial @'["trivialField"] $
      FormE.field @"trivialField" f1

instance ToEncoding MultipleFields
  where
    toEncoding (MultipleFields f1 f2 f3 f4 f5 f6) = FormE.fieldsToMessage $
      FormE.field @"multiFieldDouble" f1 .
      FormE.field @"multiFieldFloat" f2 .
      FormE.field @"multiFieldInt32" f3 .
      FormE.field @"multiFieldInt64" f4 .
      FormE.field @"multiFieldString" f5 .
      FormE.field @"multiFieldBool" f6

instance ToEncoding SignedInts
  where
    toEncoding (SignedInts f1 f2) = FormE.fieldsToMessage $
      FormE.field @"signed32" f1 .
      FormE.field @"signed64" f2

instance ToEncoding WithEnum
  where
    toEncoding (WithEnum f1) = FormE.fieldsToMessage $
      FormE.field @"enumField" f1

instance ToEncoding WithNesting_Nested
  where
    toEncoding (WithNesting_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      FormE.field @"nestedField1" f1 .
      FormE.field @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoding WithNesting
  where
    toEncoding (WithNesting f1) = FormE.fieldsToMessage $
      FormE.field @"nestedMessage" (fmap @Maybe toEncoding f1)

instance ToEncoding WithNestingRepeated_Nested
  where
    toEncoding (WithNestingRepeated_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      FormE.field @"nestedField1" f1 .
      FormE.field @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoding WithNestingRepeated
  where
    toEncoding (WithNestingRepeated f1) = FormE.fieldsToMessage $
      repeated @"nestedMessages" toEncoding f1

instance ToEncoding NestedInts
  where
    toEncoding (NestedInts f1 f2) = FormE.fieldsToMessage $
      FormE.field @"nestedInt1" f1 .
      FormE.field @"nestedInt2" f2

instance ToEncoding WithNestingRepeatedInts
  where
    toEncoding (WithNestingRepeatedInts f1) = FormE.fieldsToMessage $
      repeated @"nestedInts" toEncoding f1

instance ToEncoding WithRepetition
  where
    toEncoding (WithRepetition f1) = FormE.fieldsToMessage $
      repeated @"repeatedField1" id f1

instance ToEncoding WithRepeatedSigned
  where
    toEncoding (WithRepeatedSigned f1 f2) = FormE.fieldsToMessage $
      repeated @"r32" id f1 .
      repeated @"r64" id f2

instance ToEncoding WithFixed
  where
    toEncoding (WithFixed f1 f2 f3 f4) = FormE.fieldsToMessage $
      FormE.field @"fixed1" f1 .
      FormE.field @"fixed2" f2 .
      FormE.field @"fixed3" f3 .
      FormE.field @"fixed4" f4

instance ToEncoding WithBytes
  where
    toEncoding (WithBytes f1 f2) = FormE.fieldsToMessage $
      FormE.field @"bytes1" f1 .
      repeated @"bytes2" id f2

instance ToEncoding WithPacking
  where
    toEncoding (WithPacking f1 f2) = FormE.fieldsToMessage $
      repeated @"packing1" id f1 .
      repeated @"packing2" id f2

instance ToEncoding AllPackedTypes
  where
    toEncoding (AllPackedTypes f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13) = FormE.fieldsToMessage $
      repeated @"packedWord32" id f1 .
      repeated @"packedWord64" id f2 .
      repeated @"packedInt32" id f3 .
      repeated @"packedInt64" id f4 .
      repeated @"packedFixed32" id f5 .
      repeated @"packedFixed64" id f6 .
      repeated @"packedFloat" id f7 .
      repeated @"packedDouble" id f8 .
      repeated @"packedSFixed32" id f9 .
      repeated @"packedSFixed64" id f10 .
      repeated @"packedBool" id f11 .
      repeated @"packedEnum" id f12 .
      repeated @"unpackedEnum" id f13

instance ToEncoding OutOfOrderFields
  where
    toEncoding (OutOfOrderFields f1 f2 f3 f4) = FormE.fieldsToMessage $
      repeated @"field1" id f1 .
      FormE.field @"field2" f2 .
      FormE.field @"field3" f3 .
      repeated @"field4" id f4

instance ToEncoding ShadowedMessage
  where
    toEncoding (ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      FormE.field @"name" f1 .
      FormE.field @"value" f2

instance ToEncoding MessageShadower_ShadowedMessage
  where
    toEncoding (MessageShadower_ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      FormE.field @"name" f1 .
      FormE.field @"value" f2

instance ToEncoding MessageShadower
  where
    toEncoding (MessageShadower f1 f2) = FormE.fieldsToMessage $
      FormE.field @"shadowed_message" (fmap @Maybe toEncoding f1) .
      FormE.field @"name" f2

instance ToEncoding WithQualifiedName
  where
    toEncoding (WithQualifiedName f1 f2) = FormE.fieldsToMessage $
      FormE.field @"qname1" (fmap @Maybe toEncoding f1) .
      FormE.field @"qname2" (fmap @Maybe toEncoding f2)

instance ToEncoding TestProtoImport.WithNesting_Nested
  where
    toEncoding (TestProtoImport.WithNesting_Nested f1 f2) = FormE.fieldsToMessage $
      FormE.field @"nestedField1" f1 .
      FormE.field @"nestedField2" f2

instance ToEncoding TestProtoImport.WithNesting
  where
    toEncoding (TestProtoImport.WithNesting f1 f2) = FormE.fieldsToMessage $
      FormE.field @"nestedMessage1" (fmap @Maybe toEncoding f1) .
      FormE.field @"nestedMessage2" (fmap @Maybe toEncoding f2)

instance ToEncoding UsingImported
  where
    toEncoding (UsingImported f1 f2) = FormE.fieldsToMessage $
      FormE.field @"importedNesting" (fmap @Maybe toEncoding f1) .
      FormE.field @"localNesting" (fmap @Maybe toEncoding f2)

instance ToEncoding TestProtoOneof.DummyMsg
  where
    toEncoding (TestProtoOneof.DummyMsg f1) = FormE.fieldsToMessage $
      FormE.field @"dummy" f1

instance ToEncoding TestProtoOneofImport.AMessage
  where
    toEncoding (TestProtoOneofImport.AMessage f1 f2) = FormE.fieldsToMessage $
      FormE.field @"x" f1 .
      FormE.field @"y" f2

instance ToEncoding TestProtoOneofImport.WithOneof
  where
    toEncoding (TestProtoOneofImport.WithOneof f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneofImport.WithOneofPickOneA v) ->
          FormE.field @"a" v
        Just (TestProtoOneofImport.WithOneofPickOneB v) ->
          FormE.field @"b" v
        Just (TestProtoOneofImport.WithOneofPickOneC v) ->
          FormE.field @"c" (toEncoding v)

instance ToEncoding TestProtoOneof.Something
  where
    toEncoding (TestProtoOneof.Something f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"value" f1 .
      FormE.field @"another" f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.SomethingPickOneName v) ->
          FormE.field @"name" v
        Just (TestProtoOneof.SomethingPickOneSomeid v) ->
          FormE.field @"someid" v
        Just (TestProtoOneof.SomethingPickOneDummyMsg1 v) ->
          FormE.field @"dummyMsg1" (toEncoding v)
        Just (TestProtoOneof.SomethingPickOneDummyMsg2 v) ->
          FormE.field @"dummyMsg2" (toEncoding v)
        Just (TestProtoOneof.SomethingPickOneDummyEnum v) ->
          FormE.field @"dummyEnum" v

instance ToEncoding TestProtoOneof.WithImported
  where
    toEncoding (TestProtoOneof.WithImported f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.WithImportedPickOneDummyMsg1 v) ->
          FormE.field @"dummyMsg1" (toEncoding v)
        Just (TestProtoOneof.WithImportedPickOneWithOneof v) ->
          FormE.field @"withOneof" (toEncoding v)

instance ToEncoding TestProtoWrappers.TestDoubleValue
  where
    toEncoding (TestProtoWrappers.TestDoubleValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestDoubleValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestFloatValue
  where
    toEncoding (TestProtoWrappers.TestFloatValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestFloatValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestInt64Value
  where
    toEncoding (TestProtoWrappers.TestInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt64ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestUInt64Value
  where
    toEncoding (TestProtoWrappers.TestUInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt64ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestInt32Value
  where
    toEncoding (TestProtoWrappers.TestInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt32ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestUInt32Value
  where
    toEncoding (TestProtoWrappers.TestUInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt32ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestBoolValue
  where
    toEncoding (TestProtoWrappers.TestBoolValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBoolValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestStringValue
  where
    toEncoding (TestProtoWrappers.TestStringValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestStringValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding TestProtoWrappers.TestBytesValue
  where
    toEncoding (TestProtoWrappers.TestBytesValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBytesValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoding WrappedTrivial
  where
    toEncoding (WrappedTrivial f1) = FormE.fieldsToMessage $
      FormE.field @"trivial" (fmap @Maybe toEncoding f1)

instance ToEncoding MapTest
  where
    toEncoding (MapTest f1 f2 f3) = FormE.fieldsToMessage $
        associations @"prim" assoc1 f1 .
        associations @"trivial" assoc2 f2 .
        associations @"signed" assoc3 f3
      where
        assoc1 (k, v) = FormE.fieldsToMessage $
          FormE.field @"key" k .
          FormE.field @"value" v

        assoc2 (k, v) = FormE.fieldsToMessage $
          FormE.field @"key" k .
          FormE.field @"value" (fmap @Maybe toEncoding v)

        assoc3 (k, v) = FormE.fieldsToMessage $
          FormE.field @"key" k .
          FormE.field @"value" v

#endif
