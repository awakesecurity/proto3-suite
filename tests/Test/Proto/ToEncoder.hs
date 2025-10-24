{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Proto.ToEncoder
  ( Iterator(..)
  , Stripping(..)
  , ToEncoder(..)
  ) where

import Control.Category ((.))
import Prelude hiding ((.))

import Data.Foldable (toList)
import qualified Data.Functor.Identity as Functor (Identity(..))
import Data.Kind (Type)
import qualified Data.Map as M
import qualified Data.Vector
import qualified Proto3.Suite.Form as Form
import qualified Proto3.Suite.Form.Encode as FormE
import Proto3.Wire.Encode.Repeated (mapRepeated)

import           TestProto
import qualified TestProtoImport
import qualified TestProtoNegativeEnum
import qualified TestProtoOneof
import qualified TestProtoOneofImport
import qualified TestProtoOptional
import qualified TestProtoWrappers

-- | Which kind of iteration we should test for repeated fields.
data Iterator
  = Identity
  | Forward
  | Vector
  deriving stock (Bounded, Enum, Eq, Read, Show)

-- | Whether we should strip 'FormE.Wrap' from the types of the values to be contained
-- in standard wrappers; either approach should be supported for typically-used types.
data Stripping
  = Keep
  | Strip
  deriving stock (Bounded, Enum, Eq, Read, Show)

class ToEncoder a
  where
    toEncoder :: (?iterator :: Iterator, ?stripping :: Stripping) => a -> FormE.MessageEncoder a

#if TYPE_LEVEL_FORMAT

type family IsWrapped (a :: Type) :: Bool
  where
    IsWrapped (FormE.Wrap _) = 'True
    IsWrapped _ = 'False

class (IsWrapped a ~ w) =>
      Strip a b w | a w -> b
  where
    strip :: a -> b

instance (IsWrapped a ~ 'False) =>
         Strip a a 'False
  where
    strip = id

instance Strip (FormE.Wrap a) a 'True
  where
    strip = FormE.unwrap

implicit ::
  forall name a message names b .
  ( Strip a b (IsWrapped a)
  , FormE.Field name a message
  , FormE.Field name b message
  , ?stripping :: Stripping
  ) =>
  a ->
  FormE.FieldsEncoder message names (FormE.Occupy message name names)
implicit v = case ?stripping of
  Keep -> FormE.field @name @a v
  Strip -> FormE.field @name @b (strip v)

optional ::
  forall name a message names b .
  ( Strip a b (IsWrapped a)
  , FormE.Field name (Maybe a) message
  , FormE.Field name (Maybe b) message
  , ?stripping :: Stripping
  ) =>
  Maybe a ->
  FormE.FieldsEncoder message names (FormE.Occupy message name names)
optional v = case ?stripping of
  Keep -> FormE.field @name @(Maybe a) v
  Strip -> FormE.field @name @(Maybe b) (fmap @Maybe strip v)

repeated ::
  forall name a b c message names .
  ( ?iterator :: Iterator
  , ?stripping :: Stripping
  , Strip b c (IsWrapped b)
  , FormE.Occupy message name names ~ names
  , FormE.Field name (Functor.Identity b) message
  , FormE.Field name ([b]) message
  , FormE.Field name (Data.Vector.Vector b) message
  , FormE.Field name (Functor.Identity c) message
  , FormE.Field name ([c]) message
  , FormE.Field name (Data.Vector.Vector c) message
  ) =>
  (a -> b) ->
  Data.Vector.Vector a ->
  FormE.FieldsEncoder message names names
repeated f = case (?iterator, ?stripping) of
  (Identity, Keep) ->
    -- We use 'Identity' to indicate we should emit the field elements one at a time,
    -- without the potential for packing, though in any case not all types support packing.
    FormE.foldFieldsEncoders . fmap (FormE.field @name . Functor.Identity . f)
  (Identity, Strip) ->
    -- We use 'Identity' to indicate we should emit the field elements one at a time,
    -- without the potential for packing, though in any case not all types support packing.
    FormE.foldFieldsEncoders . fmap (FormE.field @name . Functor.Identity . strip . f)
  (Forward, Keep) ->
    FormE.field @name . map f . toList
  (Forward, Strip) ->
    FormE.field @name . map (strip . f) . toList
  (Vector, Keep) ->
    FormE.field @name . Data.Vector.map f
  (Vector, Strip) ->
    FormE.field @name . Data.Vector.map f

associations ::
  forall name k v key value message names .
  ( Form.ProtoTypeOf message name ~ 'Form.Map key value
  , Form.CardinalityOf message name ~ 'Form.Repeated 'Form.Unpacked
  , ?iterator :: Iterator
  , FormE.Field name (Functor.Identity (FormE.MessageEncoder (Form.Association key value))) message
  , FormE.Field name [FormE.MessageEncoder (Form.Association key value)] message
  , FormE.Field name (Data.Vector.Vector (FormE.MessageEncoder (Form.Association key value))) message
  , FormE.KnownFieldNumber message name
  ) =>
  ((k, v) -> FormE.MessageEncoder (Form.Association key value)) ->
  M.Map k v ->
  FormE.FieldsEncoder message names names
associations f = case ?iterator of
  Identity -> FormE.foldFieldsEncoders .
              mapRepeated (FormE.associations @name . Functor.Identity . f) .
              M.toAscList
  Forward -> FormE.associations @name . map f . M.toAscList
  Vector -> FormE.associations @name . Data.Vector.map f . Data.Vector.fromList . M.toAscList

instance ToEncoder Trivial
  where
    toEncoder (Trivial f1) = FormE.fieldsToMessage @Trivial @'["trivialField"] $
      implicit @"trivialField" f1

instance ToEncoder MultipleFields
  where
    toEncoder (MultipleFields f1 f2 f3 f4 f5 f6) = FormE.fieldsToMessage $
      implicit @"multiFieldDouble" f1 .
      implicit @"multiFieldFloat" f2 .
      implicit @"multiFieldInt32" f3 .
      implicit @"multiFieldInt64" f4 .
      implicit @"multiFieldString" f5 .
      implicit @"multiFieldBool" f6

instance ToEncoder SignedInts
  where
    toEncoder (SignedInts f1 f2) = FormE.fieldsToMessage $
      implicit @"signed32" f1 .
      implicit @"signed64" f2

instance ToEncoder WithEnum
  where
    toEncoder (WithEnum f1) = FormE.fieldsToMessage $
      implicit @"enumField" f1

instance ToEncoder WithNesting_Nested
  where
    toEncoder (WithNesting_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      implicit @"nestedField1" f1 .
      implicit @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoder WithNesting
  where
    toEncoder (WithNesting f1) = FormE.fieldsToMessage $
      optional @"nestedMessage" (fmap @Maybe toEncoder f1)

instance ToEncoder WithNestingRepeated_Nested
  where
    toEncoder (WithNestingRepeated_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      implicit @"nestedField1" f1 .
      implicit @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoder WithNestingRepeated
  where
    toEncoder (WithNestingRepeated f1) = FormE.fieldsToMessage $
      repeated @"nestedMessages" toEncoder f1

instance ToEncoder NestedInts
  where
    toEncoder (NestedInts f1 f2) = FormE.fieldsToMessage $
      implicit @"nestedInt1" f1 .
      implicit @"nestedInt2" f2

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
      implicit @"fixed1" f1 .
      implicit @"fixed2" f2 .
      implicit @"fixed3" f3 .
      implicit @"fixed4" f4

instance ToEncoder WithBytes
  where
    toEncoder (WithBytes f1 f2) = FormE.fieldsToMessage $
      implicit @"bytes1" f1 .
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
      implicit @"field2" f2 .
      implicit @"field3" f3 .
      repeated @"field4" id f4

instance ToEncoder ShadowedMessage
  where
    toEncoder (ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      implicit @"name" f1 .
      implicit @"value" f2

instance ToEncoder MessageShadower_ShadowedMessage
  where
    toEncoder (MessageShadower_ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      implicit @"name" f1 .
      implicit @"value" f2

instance ToEncoder MessageShadower
  where
    toEncoder (MessageShadower f1 f2) = FormE.fieldsToMessage $
      optional @"shadowed_message" (fmap @Maybe toEncoder f1) .
      implicit @"name" f2

instance ToEncoder WithQualifiedName
  where
    toEncoder (WithQualifiedName f1 f2) = FormE.fieldsToMessage $
      optional @"qname1" (fmap @Maybe toEncoder f1) .
      optional @"qname2" (fmap @Maybe toEncoder f2)

instance ToEncoder TestProtoImport.WithNesting_Nested
  where
    toEncoder (TestProtoImport.WithNesting_Nested f1 f2) = FormE.fieldsToMessage $
      implicit @"nestedField1" f1 .
      implicit @"nestedField2" f2

instance ToEncoder TestProtoImport.WithNesting
  where
    toEncoder (TestProtoImport.WithNesting f1 f2) = FormE.fieldsToMessage $
      optional @"nestedMessage1" (fmap @Maybe toEncoder f1) .
      optional @"nestedMessage2" (fmap @Maybe toEncoder f2)

instance ToEncoder UsingImported
  where
    toEncoder (UsingImported f1 f2) = FormE.fieldsToMessage $
      optional @"importedNesting" (fmap @Maybe toEncoder f1) .
      optional @"localNesting" (fmap @Maybe toEncoder f2)

instance ToEncoder TestProtoOneof.DummyMsg
  where
    toEncoder (TestProtoOneof.DummyMsg f1) = FormE.fieldsToMessage $
      implicit @"dummy" f1

instance ToEncoder TestProtoOneofImport.AMessage
  where
    toEncoder (TestProtoOneofImport.AMessage f1 f2) = FormE.fieldsToMessage $
      implicit @"x" f1 .
      implicit @"y" f2

instance ToEncoder TestProtoOneofImport.WithOneof
  where
    toEncoder (TestProtoOneofImport.WithOneof f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneofImport.WithOneofPickOneA v) ->
          optional @"a" (Just v)
        Just (TestProtoOneofImport.WithOneofPickOneB v) ->
          optional @"b" (Just v)
        Just (TestProtoOneofImport.WithOneofPickOneC v) ->
          optional @"c" (Just (toEncoder v))

instance ToEncoder TestProtoOneof.Something
  where
    toEncoder (TestProtoOneof.Something f1 f2 f3) = FormE.fieldsToMessage $
      implicit @"value" f1 .
      implicit @"another" f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.SomethingPickOneName v) ->
          optional @"name" (Just v)
        Just (TestProtoOneof.SomethingPickOneSomeid v) ->
          optional @"someid" (Just v)
        Just (TestProtoOneof.SomethingPickOneDummyMsg1 v) ->
          optional @"dummyMsg1" (Just (toEncoder v))
        Just (TestProtoOneof.SomethingPickOneDummyMsg2 v) ->
          optional @"dummyMsg2" (Just (toEncoder v))
        Just (TestProtoOneof.SomethingPickOneDummyEnum v) ->
          optional @"dummyEnum" (Just v)

instance ToEncoder TestProtoOneof.WithImported
  where
    toEncoder (TestProtoOneof.WithImported f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.WithImportedPickOneDummyMsg1 v) ->
          optional @"dummyMsg1" (Just (toEncoder v))
        Just (TestProtoOneof.WithImportedPickOneWithOneof v) ->
          optional @"withOneof" (Just (toEncoder v))

instance ToEncoder TestProtoWrappers.TestDoubleValue
  where
    toEncoder (TestProtoWrappers.TestDoubleValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestDoubleValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestFloatValue
  where
    toEncoder (TestProtoWrappers.TestFloatValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestFloatValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestInt64Value
  where
    toEncoder (TestProtoWrappers.TestInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt64ValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestUInt64Value
  where
    toEncoder (TestProtoWrappers.TestUInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt64ValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestInt32Value
  where
    toEncoder (TestProtoWrappers.TestInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt32ValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestUInt32Value
  where
    toEncoder (TestProtoWrappers.TestUInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt32ValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestBoolValue
  where
    toEncoder (TestProtoWrappers.TestBoolValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBoolValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestStringValue
  where
    toEncoder (TestProtoWrappers.TestStringValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestStringValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder TestProtoWrappers.TestBytesValue
  where
    toEncoder (TestProtoWrappers.TestBytesValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBytesValuePickOneOne v) ->
          optional @"one" (Just (FormE.Wrap v))

instance ToEncoder WrappedTrivial
  where
    toEncoder (WrappedTrivial f1) = FormE.fieldsToMessage $
      optional @"trivial" (fmap @Maybe toEncoder f1)

instance ToEncoder MapTest
  where
    toEncoder (MapTest f1 f2 f3) = FormE.fieldsToMessage $
        associations @"prim" assoc1 f1 .
        associations @"trivial" assoc2 f2 .
        associations @"signed" assoc3 f3
      where
        assoc1 (k, v) = FormE.fieldsToMessage $
          implicit @"key" k .
          implicit @"value" v

        assoc2 (k, v) = FormE.fieldsToMessage $
          implicit @"key" k .
          optional @"value" (fmap @Maybe toEncoder v)

        assoc3 (k, v) = FormE.fieldsToMessage $
          implicit @"key" k .
          implicit @"value" v

instance ToEncoder TestProtoNegativeEnum.WithNegativeEnum
  where
    toEncoder (TestProtoNegativeEnum.WithNegativeEnum f1) = FormE.fieldsToMessage $
      implicit @"v" f1

instance ToEncoder TestProtoOptional.Submessage
  where
    toEncoder (TestProtoOptional.Submessage f1) = FormE.fieldsToMessage $
      implicit @"someField" f1

instance ToEncoder TestProtoOptional.WithOptional
  where
    toEncoder (TestProtoOptional.WithOptional
                 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17
              ) = FormE.fieldsToMessage $
      optional @"optionalDouble" f1 .
      optional @"optionalFloat" f2 .
      optional @"optionalInt32" f3 .
      optional @"optionalInt64" f4 .
      optional @"optionalUint32" f5 .
      optional @"optionalUint64" f6 .
      optional @"optionalSint32" f7 .
      optional @"optionalSint64" f8 .
      optional @"optionalFixed32" f9 .
      optional @"optionalFixed64" f10 .
      optional @"optionalSfixed32" f11 .
      optional @"optionalSfixed64" f12 .
      optional @"optionalBool" f13 .
      optional @"optionalString" f14 .
      optional @"optionalBytes" f15 .
      optional @"optionalEnum" f16 .
      optional @"optionalSubmessage" (fmap @Maybe toEncoder f17)

#endif
