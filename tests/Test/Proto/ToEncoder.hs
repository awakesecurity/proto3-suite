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

module Test.Proto.ToEncoder
  ( Iterator(..)
  , ToEncoder(..)
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
import qualified TestProtoNegativeEnum
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

class ToEncoder a
  where
    toEncoder :: (?iterator :: Iterator) => a -> FormE.MessageEncoder a

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
  , FormE.Field name (Functor.Identity (FormE.MessageEncoder (Form.Association key value))) message
  , FormE.Field name (FormE.Forward (FormE.MessageEncoder (Form.Association key value))) message
  , FormE.Field name (FormE.Reverse (FormE.MessageEncoder (Form.Association key value))) message
  , FormE.Field name (FormE.Vector (FormE.MessageEncoder (Form.Association key value))) message
  , FormE.KnownFieldNumber message name
  ) =>
  ((k, v) -> FormE.MessageEncoder (Form.Association key value)) ->
  M.Map k v ->
  FormE.Prefix message names names
associations f = case ?iterator of
  Identity -> FormE.foldPrefixes . FormE.Forward (FormE.associations @name . Functor.Identity . f) . M.toAscList
  Forward -> FormE.associations @name . FormE.Forward f . M.toAscList
  Reverse -> FormE.associations @name . FormE.Reverse f . M.toDescList
  Vector -> FormE.associations @name . FormE.Vector f . Data.Vector.fromList . M.toAscList

instance ToEncoder Trivial
  where
    toEncoder (Trivial f1) = FormE.fieldsToMessage @Trivial @'["trivialField"] $
      FormE.field @"trivialField" f1

instance ToEncoder MultipleFields
  where
    toEncoder (MultipleFields f1 f2 f3 f4 f5 f6) = FormE.fieldsToMessage $
      FormE.field @"multiFieldDouble" f1 .
      FormE.field @"multiFieldFloat" f2 .
      FormE.field @"multiFieldInt32" f3 .
      FormE.field @"multiFieldInt64" f4 .
      FormE.field @"multiFieldString" f5 .
      FormE.field @"multiFieldBool" f6

instance ToEncoder SignedInts
  where
    toEncoder (SignedInts f1 f2) = FormE.fieldsToMessage $
      FormE.field @"signed32" f1 .
      FormE.field @"signed64" f2

instance ToEncoder WithEnum
  where
    toEncoder (WithEnum f1) = FormE.fieldsToMessage $
      FormE.field @"enumField" f1

instance ToEncoder WithNesting_Nested
  where
    toEncoder (WithNesting_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      FormE.field @"nestedField1" f1 .
      FormE.field @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoder WithNesting
  where
    toEncoder (WithNesting f1) = FormE.fieldsToMessage $
      FormE.field @"nestedMessage" (fmap @Maybe toEncoder f1)

instance ToEncoder WithNestingRepeated_Nested
  where
    toEncoder (WithNestingRepeated_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      FormE.field @"nestedField1" f1 .
      FormE.field @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoder WithNestingRepeated
  where
    toEncoder (WithNestingRepeated f1) = FormE.fieldsToMessage $
      repeated @"nestedMessages" toEncoder f1

instance ToEncoder NestedInts
  where
    toEncoder (NestedInts f1 f2) = FormE.fieldsToMessage $
      FormE.field @"nestedInt1" f1 .
      FormE.field @"nestedInt2" f2

instance ToEncoder WithNestingRepeatedInts
  where
    toEncoder (WithNestingRepeatedInts f1) = FormE.fieldsToMessage $
      repeated @"nestedInts" toEncoder f1

instance ToEncoder WithRepetition
  where
    toEncoder (WithRepetition f1) = FormE.fieldsToMessage $
      repeated @"repeatedField1" id f1

instance ToEncoder WithRepeatedSigned
  where
    toEncoder (WithRepeatedSigned f1 f2) = FormE.fieldsToMessage $
      repeated @"r32" id f1 .
      repeated @"r64" id f2

instance ToEncoder WithFixed
  where
    toEncoder (WithFixed f1 f2 f3 f4) = FormE.fieldsToMessage $
      FormE.field @"fixed1" f1 .
      FormE.field @"fixed2" f2 .
      FormE.field @"fixed3" f3 .
      FormE.field @"fixed4" f4

instance ToEncoder WithBytes
  where
    toEncoder (WithBytes f1 f2) = FormE.fieldsToMessage $
      FormE.field @"bytes1" f1 .
      repeated @"bytes2" id f2

instance ToEncoder WithPacking
  where
    toEncoder (WithPacking f1 f2) = FormE.fieldsToMessage $
      repeated @"packing1" id f1 .
      repeated @"packing2" id f2

instance ToEncoder AllPackedTypes
  where
    toEncoder (AllPackedTypes f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13) = FormE.fieldsToMessage $
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

instance ToEncoder OutOfOrderFields
  where
    toEncoder (OutOfOrderFields f1 f2 f3 f4) = FormE.fieldsToMessage $
      repeated @"field1" id f1 .
      FormE.field @"field2" f2 .
      FormE.field @"field3" f3 .
      repeated @"field4" id f4

instance ToEncoder ShadowedMessage
  where
    toEncoder (ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      FormE.field @"name" f1 .
      FormE.field @"value" f2

instance ToEncoder MessageShadower_ShadowedMessage
  where
    toEncoder (MessageShadower_ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      FormE.field @"name" f1 .
      FormE.field @"value" f2

instance ToEncoder MessageShadower
  where
    toEncoder (MessageShadower f1 f2) = FormE.fieldsToMessage $
      FormE.field @"shadowed_message" (fmap @Maybe toEncoder f1) .
      FormE.field @"name" f2

instance ToEncoder WithQualifiedName
  where
    toEncoder (WithQualifiedName f1 f2) = FormE.fieldsToMessage $
      FormE.field @"qname1" (fmap @Maybe toEncoder f1) .
      FormE.field @"qname2" (fmap @Maybe toEncoder f2)

instance ToEncoder TestProtoImport.WithNesting_Nested
  where
    toEncoder (TestProtoImport.WithNesting_Nested f1 f2) = FormE.fieldsToMessage $
      FormE.field @"nestedField1" f1 .
      FormE.field @"nestedField2" f2

instance ToEncoder TestProtoImport.WithNesting
  where
    toEncoder (TestProtoImport.WithNesting f1 f2) = FormE.fieldsToMessage $
      FormE.field @"nestedMessage1" (fmap @Maybe toEncoder f1) .
      FormE.field @"nestedMessage2" (fmap @Maybe toEncoder f2)

instance ToEncoder UsingImported
  where
    toEncoder (UsingImported f1 f2) = FormE.fieldsToMessage $
      FormE.field @"importedNesting" (fmap @Maybe toEncoder f1) .
      FormE.field @"localNesting" (fmap @Maybe toEncoder f2)

instance ToEncoder TestProtoOneof.DummyMsg
  where
    toEncoder (TestProtoOneof.DummyMsg f1) = FormE.fieldsToMessage $
      FormE.field @"dummy" f1

instance ToEncoder TestProtoOneofImport.AMessage
  where
    toEncoder (TestProtoOneofImport.AMessage f1 f2) = FormE.fieldsToMessage $
      FormE.field @"x" f1 .
      FormE.field @"y" f2

instance ToEncoder TestProtoOneofImport.WithOneof
  where
    toEncoder (TestProtoOneofImport.WithOneof f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneofImport.WithOneofPickOneA v) ->
          FormE.field @"a" v
        Just (TestProtoOneofImport.WithOneofPickOneB v) ->
          FormE.field @"b" v
        Just (TestProtoOneofImport.WithOneofPickOneC v) ->
          FormE.field @"c" (toEncoder v)

instance ToEncoder TestProtoOneof.Something
  where
    toEncoder (TestProtoOneof.Something f1 f2 f3) = FormE.fieldsToMessage $
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
          FormE.field @"dummyMsg1" (toEncoder v)
        Just (TestProtoOneof.SomethingPickOneDummyMsg2 v) ->
          FormE.field @"dummyMsg2" (toEncoder v)
        Just (TestProtoOneof.SomethingPickOneDummyEnum v) ->
          FormE.field @"dummyEnum" v

instance ToEncoder TestProtoOneof.WithImported
  where
    toEncoder (TestProtoOneof.WithImported f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.WithImportedPickOneDummyMsg1 v) ->
          FormE.field @"dummyMsg1" (toEncoder v)
        Just (TestProtoOneof.WithImportedPickOneWithOneof v) ->
          FormE.field @"withOneof" (toEncoder v)

instance ToEncoder TestProtoWrappers.TestDoubleValue
  where
    toEncoder (TestProtoWrappers.TestDoubleValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestDoubleValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestFloatValue
  where
    toEncoder (TestProtoWrappers.TestFloatValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestFloatValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestInt64Value
  where
    toEncoder (TestProtoWrappers.TestInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt64ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestUInt64Value
  where
    toEncoder (TestProtoWrappers.TestUInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt64ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestInt32Value
  where
    toEncoder (TestProtoWrappers.TestInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt32ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestUInt32Value
  where
    toEncoder (TestProtoWrappers.TestUInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt32ValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestBoolValue
  where
    toEncoder (TestProtoWrappers.TestBoolValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBoolValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestStringValue
  where
    toEncoder (TestProtoWrappers.TestStringValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestStringValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder TestProtoWrappers.TestBytesValue
  where
    toEncoder (TestProtoWrappers.TestBytesValue f1 f2 f3) = FormE.fieldsToMessage $
      FormE.field @"wrapper" (fmap Form.Wrap f1) .
      repeated @"many" Form.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBytesValuePickOneOne v) ->
          FormE.field @"one" (Form.Wrap v)

instance ToEncoder WrappedTrivial
  where
    toEncoder (WrappedTrivial f1) = FormE.fieldsToMessage $
      FormE.field @"trivial" (fmap @Maybe toEncoder f1)

instance ToEncoder MapTest
  where
    toEncoder (MapTest f1 f2 f3) = FormE.fieldsToMessage $
        associations @"prim" assoc1 f1 .
        associations @"trivial" assoc2 f2 .
        associations @"signed" assoc3 f3
      where
        assoc1 (k, v) = FormE.fieldsToMessage $
          FormE.field @"key" k .
          FormE.field @"value" v

        assoc2 (k, v) = FormE.fieldsToMessage $
          FormE.field @"key" k .
          FormE.field @"value" (fmap @Maybe toEncoder v)

        assoc3 (k, v) = FormE.fieldsToMessage $
          FormE.field @"key" k .
          FormE.field @"value" v

instance ToEncoder TestProtoNegativeEnum.WithNegativeEnum
  where
    toEncoder (TestProtoNegativeEnum.WithNegativeEnum f1) = FormE.fieldsToMessage $
      FormE.field @"v" f1

#endif
