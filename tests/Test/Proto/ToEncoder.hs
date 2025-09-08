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

unitary ::
  forall name a message names b .
  ( Strip a b (IsWrapped a)
  , FormE.Field name a message
  , FormE.Field name b message
  , ?stripping :: Stripping
  ) =>
  a ->
  FormE.FieldsEncoder message names (FormE.Occupy message name names)
unitary v = case ?stripping of
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
      unitary @"trivialField" f1

instance ToEncoder MultipleFields
  where
    toEncoder (MultipleFields f1 f2 f3 f4 f5 f6) = FormE.fieldsToMessage $
      unitary @"multiFieldDouble" f1 .
      unitary @"multiFieldFloat" f2 .
      unitary @"multiFieldInt32" f3 .
      unitary @"multiFieldInt64" f4 .
      unitary @"multiFieldString" f5 .
      unitary @"multiFieldBool" f6

instance ToEncoder SignedInts
  where
    toEncoder (SignedInts f1 f2) = FormE.fieldsToMessage $
      unitary @"signed32" f1 .
      unitary @"signed64" f2

instance ToEncoder WithEnum
  where
    toEncoder (WithEnum f1) = FormE.fieldsToMessage $
      unitary @"enumField" f1

instance ToEncoder WithNesting_Nested
  where
    toEncoder (WithNesting_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      unitary @"nestedField1" f1 .
      unitary @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoder WithNesting
  where
    toEncoder (WithNesting f1) = FormE.fieldsToMessage $
      optional @"nestedMessage" (fmap @Maybe toEncoder f1)

instance ToEncoder WithNestingRepeated_Nested
  where
    toEncoder (WithNestingRepeated_Nested f1 f2 f3 f4) = FormE.fieldsToMessage $
      unitary @"nestedField1" f1 .
      unitary @"nestedField2" f2 .
      repeated @"nestedPacked" id f3 .
      repeated @"nestedUnpacked" id f4

instance ToEncoder WithNestingRepeated
  where
    toEncoder (WithNestingRepeated f1) = FormE.fieldsToMessage $
      repeated @"nestedMessages" toEncoder f1

instance ToEncoder NestedInts
  where
    toEncoder (NestedInts f1 f2) = FormE.fieldsToMessage $
      unitary @"nestedInt1" f1 .
      unitary @"nestedInt2" f2

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
      unitary @"fixed1" f1 .
      unitary @"fixed2" f2 .
      unitary @"fixed3" f3 .
      unitary @"fixed4" f4

instance ToEncoder WithBytes
  where
    toEncoder (WithBytes f1 f2) = FormE.fieldsToMessage $
      unitary @"bytes1" f1 .
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
      unitary @"field2" f2 .
      unitary @"field3" f3 .
      repeated @"field4" id f4

instance ToEncoder ShadowedMessage
  where
    toEncoder (ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      unitary @"name" f1 .
      unitary @"value" f2

instance ToEncoder MessageShadower_ShadowedMessage
  where
    toEncoder (MessageShadower_ShadowedMessage f1 f2) = FormE.fieldsToMessage $
      unitary @"name" f1 .
      unitary @"value" f2

instance ToEncoder MessageShadower
  where
    toEncoder (MessageShadower f1 f2) = FormE.fieldsToMessage $
      optional @"shadowed_message" (fmap @Maybe toEncoder f1) .
      unitary @"name" f2

instance ToEncoder WithQualifiedName
  where
    toEncoder (WithQualifiedName f1 f2) = FormE.fieldsToMessage $
      optional @"qname1" (fmap @Maybe toEncoder f1) .
      optional @"qname2" (fmap @Maybe toEncoder f2)

instance ToEncoder TestProtoImport.WithNesting_Nested
  where
    toEncoder (TestProtoImport.WithNesting_Nested f1 f2) = FormE.fieldsToMessage $
      unitary @"nestedField1" f1 .
      unitary @"nestedField2" f2

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
      unitary @"dummy" f1

instance ToEncoder TestProtoOneofImport.AMessage
  where
    toEncoder (TestProtoOneofImport.AMessage f1 f2) = FormE.fieldsToMessage $
      unitary @"x" f1 .
      unitary @"y" f2

instance ToEncoder TestProtoOneofImport.WithOneof
  where
    toEncoder (TestProtoOneofImport.WithOneof f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneofImport.WithOneofPickOneA v) ->
          unitary @"a" v
        Just (TestProtoOneofImport.WithOneofPickOneB v) ->
          unitary @"b" v
        Just (TestProtoOneofImport.WithOneofPickOneC v) ->
          unitary @"c" (toEncoder v)

instance ToEncoder TestProtoOneof.Something
  where
    toEncoder (TestProtoOneof.Something f1 f2 f3) = FormE.fieldsToMessage $
      unitary @"value" f1 .
      unitary @"another" f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.SomethingPickOneName v) ->
          unitary @"name" v
        Just (TestProtoOneof.SomethingPickOneSomeid v) ->
          unitary @"someid" v
        Just (TestProtoOneof.SomethingPickOneDummyMsg1 v) ->
          unitary @"dummyMsg1" (toEncoder v)
        Just (TestProtoOneof.SomethingPickOneDummyMsg2 v) ->
          unitary @"dummyMsg2" (toEncoder v)
        Just (TestProtoOneof.SomethingPickOneDummyEnum v) ->
          unitary @"dummyEnum" v

instance ToEncoder TestProtoOneof.WithImported
  where
    toEncoder (TestProtoOneof.WithImported f1) = FormE.fieldsToMessage $
      case f1 of
        Nothing ->
          FormE.omitted
        Just (TestProtoOneof.WithImportedPickOneDummyMsg1 v) ->
          unitary @"dummyMsg1" (toEncoder v)
        Just (TestProtoOneof.WithImportedPickOneWithOneof v) ->
          unitary @"withOneof" (toEncoder v)

instance ToEncoder TestProtoWrappers.TestDoubleValue
  where
    toEncoder (TestProtoWrappers.TestDoubleValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestDoubleValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestFloatValue
  where
    toEncoder (TestProtoWrappers.TestFloatValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestFloatValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestInt64Value
  where
    toEncoder (TestProtoWrappers.TestInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt64ValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestUInt64Value
  where
    toEncoder (TestProtoWrappers.TestUInt64Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt64ValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestInt32Value
  where
    toEncoder (TestProtoWrappers.TestInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestInt32ValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestUInt32Value
  where
    toEncoder (TestProtoWrappers.TestUInt32Value f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestUInt32ValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestBoolValue
  where
    toEncoder (TestProtoWrappers.TestBoolValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBoolValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestStringValue
  where
    toEncoder (TestProtoWrappers.TestStringValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestStringValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

instance ToEncoder TestProtoWrappers.TestBytesValue
  where
    toEncoder (TestProtoWrappers.TestBytesValue f1 f2 f3) = FormE.fieldsToMessage $
      optional @"wrapper" (fmap FormE.Wrap f1) .
      repeated @"many" FormE.Wrap f2 .
      case f3 of
        Nothing ->
          FormE.omitted
        Just (TestProtoWrappers.TestBytesValuePickOneOne v) ->
          unitary @"one" (FormE.Wrap v)

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
          unitary @"key" k .
          unitary @"value" v

        assoc2 (k, v) = FormE.fieldsToMessage $
          unitary @"key" k .
          optional @"value" (fmap @Maybe toEncoder v)

        assoc3 (k, v) = FormE.fieldsToMessage $
          unitary @"key" k .
          unitary @"value" v

instance ToEncoder TestProtoNegativeEnum.WithNegativeEnum
  where
    toEncoder (TestProtoNegativeEnum.WithNegativeEnum f1) = FormE.fieldsToMessage $
      unitary @"v" f1

#endif
