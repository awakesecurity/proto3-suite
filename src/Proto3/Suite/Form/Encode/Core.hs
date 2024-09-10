{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Proto3.Suite.Form.Encode.Core
  ( MessageEncoding(..)
  , toLazyByteString
  , Prefix(..)
  , Distinct
  , fieldsToMessage
  , Occupy
  , omitted
  , KnownFieldNumber
  , fieldNumber
  , Field(..)
  , FieldImpl(..)
  , PrimitiveField(..)
  , PackedPrimitives(..)
  , Forward(..)
  , Reverse(..)
  , Vector(..)
  , codeFromEnumerated
  , foldPrefixF
  , foldPrefixR
  , foldPrefixV
  , instantiatePackableField
  , instantiateStringOrBytesField
  ) where

import Control.Category (Category(..))
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Vector.Generic qualified
import Data.Word (Word32, Word64)
import GHC.Exts (Constraint, Proxy#, proxy#)
import GHC.TypeLits (ErrorMessage(..), KnownNat, Symbol, TypeError, natVal')
import Language.Haskell.TH qualified as TH
import Prelude hiding ((.), id)
import Proto3.Suite.Class (HasDefault(..), Primitive(..), zigZagEncode)
import Proto3.Suite.Form
         (Association, NumberOf, OneOfOf, Wrapper, Packing(..), Presence(..), RecoverProtoType,
          Repetition(..), RepetitionOf, ProtoType(..), ProtoTypeOf, Wrap(..))
import Proto3.Suite.Types (Enumerated(..), Fixed(..), Signed(..))
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Types (FieldNumber(..))

-- | Annotates 'Encode.MessageBuilder' with the type of protobuf message it encodes.
-- Prefix a tag and length to turn the message into a submessage of a larger message.
newtype MessageEncoding (a :: Type) = UnsafeMessageEncoding
  { untypedMessageEncoding :: Encode.MessageBuilder }

type role MessageEncoding nominal

-- | Serialize a message (or portion thereof) as a lazy 'BL.ByteString'.
toLazyByteString :: forall a . MessageEncoding a -> BL.ByteString
toLazyByteString = Encode.toLazyByteString . untypedMessageEncoding

-- | A 'Category' on builders that prefix zero or more fields to a message.
-- Use '.' to accumulate prefixes to create an 'MessageEncoding' for a whole message.
--
-- The first type parameter specifies the type of message being built.
--
-- The second and third type parameters list the names of some subset
-- of the oneofs and non-repeatable non-oneof fields possible within
-- the type of message specified by the first type parameter.  The
-- third type parameter must be a suffix of the second, and limits
-- the oneofs and non-repeatable non-oneof fields that may occur in
-- any builder which follows the contained builder.  The first parameter
-- prefixes the names of the oneofs and non-repeatable non-oneof
-- fields that /might/ be written by the contained builder.
--
-- If a name ends up listed more than once, that will eventually
-- be detected as a compilation error; see type family 'Distinct'.
--
-- Note that this type system permits multiple blocks of the same
-- packed repeated field.  Though that would be less compact than
-- a single block, it is allowed by the protobuf standard.
newtype Prefix (message :: Type) (possible :: [Symbol]) (following :: [Symbol]) =
  UnsafePrefix { untypedPrefix :: Encode.MessageBuilder }

type role Prefix nominal nominal nominal

-- | The 'Category' on prefixes of a particular type of
-- message whose '.' is '<>' on the contained builders.
--
-- Note that '.' preserves the requirements
-- on the type parameters of 'Prefix'.
instance Category (Prefix message)
  where
    id = UnsafePrefix mempty
    f . g = UnsafePrefix (untypedPrefix f <> untypedPrefix g)

-- | Yields a satisfied constraint if the given list of names contains
-- no duplicates after first filtering out the names of repeated fields
-- and replacing the names of @oneof@ fields with their @oneof@ names.
-- Otherwise raises a compilation error mentioning the repeated names.
type Distinct (message :: Type) (names :: [Symbol]) =
  DistinctCheck message (RepeatedNames (OccupiedOnly message names))

type family DistinctCheck (message :: Type) (repeated :: [k]) :: Constraint
  where
    DistinctCheck _ '[] = ()
    DistinctCheck message repeated = TypeError
      ( 'ShowType message ':<>: 'Text " forbids repetition of:"
        ':$$: 'ShowType repeated )

-- | Given a list of names, returns the non-repeating list
-- of names that occur more than once in the given list.
type family RepeatedNames (names :: [k]) :: [k]
  where
    RepeatedNames (name ': names) = RepeatedNames1 name names (Omits name names)
    RepeatedNames '[] = '[]

type family RepeatedNames1 (name :: k) (names :: [k]) (omits :: Bool) :: [k]
  where
    RepeatedNames1 _ names 'True = RepeatedNames names
    RepeatedNames1 name names 'False = name ': RepeatedNames (Strip name names)

-- | Is the given name absent from the given list of names?
type family Omits (name :: k) (names :: [k]) :: Bool
  where
    Omits name (name ': names) = 'False
    Omits name (_ ': names) = Omits name names
    Omits name '[] = 'True

-- | Strips all occurrences of the given name, leaving behind all other name occurrences.
type family Strip (name :: k) (names :: [k]) :: [k]
  where
    Strip name (name ': names) = Strip name names
    Strip name (other ': names) = other ': Strip name names
    Strip name '[] = '[]

-- | Filters out the repeated field names and replaces @oneof@ fields with their @oneof@ names.
--
-- We do this in case 'omitted' is used to introduce the names of repeated fields
-- or fields that are contained within @oneof@s; see the explanatory comments there.
type family OccupiedOnly (message :: Type) (names :: [Symbol]) :: [Symbol]
  where
    OccupiedOnly message (name ': names) =
      OccupiedOnly1 message name names (RepetitionOf message name)
    OccupiedOnly _ '[] =
      '[]

type family OccupiedOnly1 (message :: Type) (name :: Symbol) (names :: [Symbol])
                          (repetition :: Repetition) :: [Symbol]
  where
    OccupiedOnly1 message name names ('Singular _) =
      name ': OccupiedOnly message names
    OccupiedOnly1 message name names 'Alternative =
      OneOfOf message name ': OccupiedOnly message names
    OccupiedOnly1 message name names ('Repeated _) =
      OccupiedOnly message names

-- | Relabels a prefix of fields of a message as an encoding for
-- the message as a whole (though without any tag or length that
-- would make it a submessage).
fieldsToMessage ::
  forall (message :: Type) (names :: [Symbol]) .
  Distinct message names =>
  Prefix message '[] names ->
  MessageEncoding message
fieldsToMessage = UnsafeMessageEncoding . untypedPrefix

type Occupy (message :: Type) (name :: Symbol) (names :: [Symbol]) =
  Occupy1 message name names (RepetitionOf message name)

type family Occupy1 (message :: Type) (name :: Symbol) (names :: [Symbol])
                    (repetition :: Repetition) :: [Symbol]
  where
    Occupy1 message name names ('Singular _) = name ': names
    Occupy1 message name names 'Alternative = OneOfOf message name ': names
    Occupy1 message name names ('Repeated _) = names

-- | Uses an empty encoding for the @oneof@s and non-@oneof@ message fields
-- that appear in the final type parameter of 'Prefix' but not the previous
-- type parameter, thereby implicitly emitting their default values.
--
-- This function is not always required, but becomes necessary when
-- there are two code paths, one of which may write non-repeatable
-- fields and one of which leaves some implicitly defaulted.  This
-- function reconciles the types of the two code paths.
--
-- This function should not be used to introduce the name of a field
-- that is repeated or belongs to @oneof@, because doing so could
-- confuse the reader.  But such misuse is unlikely to be accidental,
-- and the 'Distinct' constraint compensates by ignoring repeated
-- fields and replacing @oneof@ fields with their @oneof@ names.
omitted ::
  forall (message :: Type) (names :: [Symbol]) (moreNames :: [Symbol]) .
  Prefix message names moreNames
omitted = UnsafePrefix mempty

type KnownFieldNumber (message :: Type) (name :: Symbol) = KnownNat (NumberOf message name)

-- | The term expressing the field number within the specified message type of the named field.
fieldNumber :: forall message name . KnownFieldNumber message name => FieldNumber
fieldNumber = FieldNumber (fromInteger (natVal' (proxy# :: Proxy# (NumberOf message name))))

-- | Provides a way to encode a field with the given name from a given type.
-- That name is interpreted in the context of a given type of message.
class Field (name :: Symbol) (a :: Type) (message :: Type)
  where
    -- | Encodes the named field from the given value.
    --
    -- If the field is neither repeated nor part of a oneof, then its default
    -- value is represented implicitly--that is, it encodes to zero octets.
    --
    -- Use @TypeApplications@ to specify the field name.
    --
    -- You may also need to specify the argument type, for
    -- example when passing an integer or string literal.
    --
    -- If the argument type is too verbose, then we recommend
    -- wrapping calls to 'field' in helper functions select it
    -- automatically based on particular use cases.  Examples:
    -- `Proto3.Suite.Form.Encode.message`,
    -- `Proto3.Suite.Form.Encode.associations`.
    --
    -- NOTE: The implementation of this method always delegates to 'fieldImpl'.
    field :: forall names . a -> Prefix message names (Occupy message name names)

instance FieldImpl name a message (RepetitionOf message name) (ProtoTypeOf message name) =>
         Field name a message
  where
    field = fieldImpl @name
    {-# INLINE field #-}

-- | Like @'Field' name a message@ but with additional type
-- parameters that are determined by the original ones.
-- These additional type parameters decide encoding details.
class ( RepetitionOf message name ~ repetition
      , ProtoTypeOf message name ~ protoType
      ) =>
      FieldImpl (name :: Symbol) (a :: Type) (message :: Type)
                (repetition :: Repetition) (protoType :: ProtoType)
  where
    fieldImpl :: forall names . a -> Prefix message names (Occupy message name names)

-- | Populated non-repeated submessage not inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Singular 'Optional
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (MessageEncoding inner) outer ('Singular 'Optional) ('Message inner)
  where
    fieldImpl = UnsafePrefix . Encode.embedded (fieldNumber @outer @name) . untypedMessageEncoding
    {-# INLINE fieldImpl #-}

-- | Optional non-repeated submessage not inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Singular 'Optional
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Maybe (MessageEncoding inner)) outer ('Singular 'Optional) ('Message inner)
  where
    fieldImpl = maybe omitted (field @name)
    {-# INLINE fieldImpl #-}

-- | Submessage inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Alternative
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (MessageEncoding inner) outer 'Alternative ('Message inner)
  where
    fieldImpl = UnsafePrefix . Encode.embedded (fieldNumber @outer @name) . untypedMessageEncoding
    {-# INLINE fieldImpl #-}

-- | Single submessage within a repeated submessage field.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Identity (MessageEncoding inner)) outer ('Repeated 'Unpacked) ('Message inner)
  where
    fieldImpl =
      UnsafePrefix . Encode.embedded (fieldNumber @outer @name) .
      untypedMessageEncoding . runIdentity
    {-# INLINE fieldImpl #-}

-- | Repeated submessage, given as a 'Foldable' sequence in the correct order.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Forward (MessageEncoding inner)) outer ('Repeated 'Unpacked) ('Message inner)
  where
    fieldImpl (Forward f xs) = foldPrefixF (\x -> field @name (Identity (f x))) xs
    {-# INLINE fieldImpl #-}

-- | Repeated submessage, given as a 'Foldable' sequence in reverse order.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Reverse (MessageEncoding inner)) outer ('Repeated 'Unpacked) ('Message inner)
  where
    fieldImpl (Reverse f xs) = foldPrefixR (\x -> field @name (Identity (f x))) xs
    {-# INLINE fieldImpl #-}

-- | Repeated submessage, given as a `Data.Vector.Generic.Vector` in the correct order.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Vector (MessageEncoding inner)) outer ('Repeated 'Unpacked) ('Message inner)
  where
    fieldImpl (Vector f xs) = foldPrefixV (\x -> field @name (Identity (f x))) xs
    {-# INLINE fieldImpl #-}

-- | One of many key-value pairs within a protobuf map field.
instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Identity (MessageEncoding (Association key value)))
                   message ('Repeated 'Unpacked) ('Map key value)
  where
    fieldImpl (Identity e) =
      UnsafePrefix (Encode.embedded (fieldNumber @message @name) (untypedMessageEncoding e))
    {-# INLINE fieldImpl #-}

-- | Key-value pairs of a protobuf map field, given as a 'Foldable' sequence in the correct order.
instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Forward (MessageEncoding (Association key value)))
                   message ('Repeated 'Unpacked) ('Map key value)
  where
    fieldImpl (Forward f xs) = foldPrefixF (\x -> field @name (Identity (f x))) xs
    {-# INLINE fieldImpl #-}

-- | Key-value pairs of a protobuf map field, given as a 'Foldable' sequence in reverse order.
instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Reverse (MessageEncoding (Association key value)))
                   message ('Repeated 'Unpacked) ('Map key value)
  where
    fieldImpl (Reverse f xs) = foldPrefixR (\x -> field @name (Identity (f x))) xs
    {-# INLINE fieldImpl #-}

-- | Key-value pairs of a protobuf map field, given as
-- a `Data.Vector.Generic.Vector` in the correct order.
instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Vector (MessageEncoding (Association key value)))
                   message ('Repeated 'Unpacked) ('Map key value)
  where
    fieldImpl (Vector f xs) = foldPrefixV (\x -> field @name (Identity (f x))) xs
    {-# INLINE fieldImpl #-}

-- | Optional non-repeated wrapped scalar not inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Singular 'Optional
         , ProtoTypeOf outer name ~ 'Message (Wrapper protoType)
         , KnownFieldNumber outer name
         , Field "value" b (Wrapper protoType)
         ) =>
         FieldImpl name (Maybe (Wrap b)) outer ('Singular 'Optional) ('Message (Wrapper protoType))
  where
    fieldImpl m =
      field @name @(Maybe (MessageEncoding (Wrapper protoType)))
        (fmap @Maybe (\(Wrap x) -> fieldsToMessage (field @"value" @b x)) m)
    {-# INLINE fieldImpl #-}

-- | Wrap scalar inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Alternative
         , ProtoTypeOf outer name ~ 'Message (Wrapper protoType)
         , KnownFieldNumber outer name
         , Field "value" b (Wrapper protoType)
         ) =>
         FieldImpl name (Wrap b) outer 'Alternative ('Message (Wrapper protoType))
  where
    fieldImpl (Wrap x) =
      field @name @(MessageEncoding (Wrapper protoType)) (fieldsToMessage (field @"value" @b x))
    {-# INLINE fieldImpl #-}

-- | Single wrapped scalar within a repeated field.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Wrapper protoType)
         , KnownFieldNumber outer name
         , Field "value" b (Wrapper protoType)
         ) =>
         FieldImpl name (Identity (Wrap b)) outer
                   ('Repeated 'Unpacked) ('Message (Wrapper protoType))
  where
    fieldImpl = field @name @(Identity (MessageEncoding (Wrapper protoType))) .
                fmap @Identity (\(Wrap x) -> fieldsToMessage (field @"value" @b x))
    {-# INLINE fieldImpl #-}

-- | Repeated wrapped scalar, given as a 'Foldable' sequence in the correct order.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Wrapper protoType)
         , KnownFieldNumber outer name
         , Field "value" b (Wrapper protoType)
         ) =>
         FieldImpl name (Forward (Wrap b)) outer
                   ('Repeated 'Unpacked) ('Message (Wrapper protoType))
  where
    fieldImpl = field @name @(Forward (MessageEncoding (Wrapper protoType))) .
                fmap @Forward (\(Wrap x) -> fieldsToMessage (field @"value" @b x))
    {-# INLINE fieldImpl #-}

-- | Repeated wrapped scalar, given as a 'Foldable' sequence in reverse order.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Wrapper protoType)
         , KnownFieldNumber outer name
         , Field "value" b (Wrapper protoType)
         ) =>
         FieldImpl name (Reverse (Wrap b)) outer
                   ('Repeated 'Unpacked) ('Message (Wrapper protoType))
  where
    fieldImpl = field @name @(Reverse (MessageEncoding (Wrapper protoType))) .
                fmap @Reverse (\(Wrap x) -> fieldsToMessage (field @"value" @b x))
    {-# INLINE fieldImpl #-}

-- | Repeated wrapped scalar, given as a `Data.Vector.Generic.Vector` in the correct order.
instance ( RepetitionOf outer name ~ 'Repeated 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Wrapper protoType)
         , KnownFieldNumber outer name
         , Field "value" b (Wrapper protoType)
         ) =>
         FieldImpl name (Vector (Wrap b)) outer ('Repeated 'Unpacked) ('Message (Wrapper protoType))
  where
    fieldImpl = field @name @(Vector (MessageEncoding (Wrapper protoType))) .
                fmap @Vector (\(Wrap x) -> fieldsToMessage (field @"value" @b x))
    {-# INLINE fieldImpl #-}

-- | Any encoding of the first type can be decoded as the second without
-- changing semantics.  This relation is more strict than the compatibilities
-- listed in <https://protobuf.dev/programming-guides/proto3/#updating>.
--
-- We do not use this class for message or map fields.  It exists for
-- integer compatibility checking, though for uniformity it extends
-- to any scalar.  Currently we disallow use of @string@ encodings
-- as @bytes@ encodings for fear of accidental misuse, and though we
-- allow use of @bool@ encodings as non-ZigZag integers as specified
-- by protobuf documentation, we do not yet exploit that compatibility.
class CompatibleScalar (encodeType :: ProtoType) (decodeType :: ProtoType)

instance CompatibleScalar 'Int32 'Int32
instance CompatibleScalar 'Int32 'Int64
instance CompatibleScalar 'Int64 'Int64
instance CompatibleScalar 'SInt32 'SInt32
instance CompatibleScalar 'SInt32 'SInt64
instance CompatibleScalar 'SInt64 'SInt64
instance CompatibleScalar 'UInt32 'UInt32
instance CompatibleScalar 'UInt32 'Int64
instance CompatibleScalar 'UInt32 'UInt64
instance CompatibleScalar 'UInt64 'UInt64
instance CompatibleScalar 'Fixed32 'Fixed32
instance CompatibleScalar 'Fixed64 'Fixed64
instance CompatibleScalar 'SFixed32 'SFixed32
instance CompatibleScalar 'SFixed64 'SFixed64
instance CompatibleScalar 'String 'String
instance CompatibleScalar 'Bytes 'Bytes
instance CompatibleScalar 'Bool 'Bool
instance CompatibleScalar 'Bool 'Int32
instance CompatibleScalar 'Bool 'Int64
instance CompatibleScalar 'Bool 'UInt32
instance CompatibleScalar 'Bool 'UInt64
instance CompatibleScalar 'Float 'Float
instance CompatibleScalar 'Double 'Double
instance ee ~ ed => CompatibleScalar ('Enumeration ee) ('Enumeration ed)

-- | Handles encoding of primitive types except for packed repeated fields,
-- which are handled by 'PackedPrimitives'.
--
-- This implementation detail should be invisible to package clients.
class ( RepetitionOf message name ~ repetition
      , ProtoTypeOf message name ~ protoType
      ) =>
      PrimitiveField (name :: Symbol) (a :: Type) (message :: Type)
                     (repetition :: Repetition) (protoType :: ProtoType)
  where
    primitiveField ::
      forall names . a -> Prefix message names (Occupy message name names)

instance ( RepetitionOf message name ~ 'Singular 'Implicit
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , HasDefault a
         , Primitive a
         ) =>
         PrimitiveField name a message ('Singular 'Implicit) protoType
  where
    primitiveField value
      | isDefault value = UnsafePrefix mempty
      | otherwise = UnsafePrefix (encodePrimitive (fieldNumber @message @name) value)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Singular 'Optional
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name a message ('Singular 'Optional) protoType
  where
    primitiveField value = UnsafePrefix (encodePrimitive (fieldNumber @message @name) value)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Alternative
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name a message 'Alternative protoType
  where
    primitiveField value = UnsafePrefix (encodePrimitive (fieldNumber @message @name) value)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Identity a) message ('Repeated 'Unpacked) protoType
  where
    primitiveField (Identity value) =
      UnsafePrefix (encodePrimitive (fieldNumber @message @name) value)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Forward a) message ('Repeated 'Unpacked) protoType
  where
    primitiveField (Forward f xs) = foldPrefixF (primitiveField @name . Identity . f) xs
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Reverse a) message ('Repeated 'Unpacked) protoType
  where
    primitiveField (Reverse f xs) = foldPrefixR (primitiveField @name . Identity . f) xs
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Vector a) message ('Repeated 'Unpacked) protoType
  where
    primitiveField (Vector f xs) = foldPrefixV (primitiveField @name . Identity . f) xs
    {-# INLINE primitiveField #-}

-- | NOTE: This instance produces the same encoding as for @[packed=false]@,
-- because packing does not improve efficiency for length-one sequences, and
-- because the protobuf specification requires parsers to handle that format.
instance ( RepetitionOf message name ~ 'Repeated 'Packed
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Identity a) message ('Repeated 'Packed) protoType
  where
    primitiveField (Identity value) =
      UnsafePrefix (encodePrimitive (fieldNumber @message @name) value)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Packed
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , PackedPrimitives a
         ) =>
         PrimitiveField name (Forward a) message ('Repeated 'Packed) protoType
  where
    primitiveField (Forward f xs)
      | null xs = UnsafePrefix mempty
      | otherwise = UnsafePrefix (packedPrimitivesF f (fieldNumber @message @name) xs)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Packed
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , PackedPrimitives a
         ) =>
         PrimitiveField name (Reverse a) message ('Repeated 'Packed) protoType
  where
    primitiveField (Reverse f xs)
      | null xs = UnsafePrefix mempty
      | otherwise = UnsafePrefix (packedPrimitivesR f (fieldNumber @message @name) xs)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Repeated 'Packed
         , ProtoTypeOf message name ~ protoType
         , CompatibleScalar (RecoverProtoType a) protoType
         , KnownFieldNumber message name
         , PackedPrimitives a
         ) =>
         PrimitiveField name (Vector a) message ('Repeated 'Packed) protoType
  where
    primitiveField (Vector f xs)
      | Data.Vector.Generic.null xs = UnsafePrefix mempty
      | otherwise = UnsafePrefix (packedPrimitivesV f (fieldNumber @message @name) xs)
    {-# INLINE primitiveField #-}

-- | Handles encoding of packed repeated fields.
--
-- This implementation detail should be invisible to package clients.
class PackedPrimitives (a :: Type)
  where
    -- | Encode the elements of the given foldable container as a packed repeated field.
    --
    -- It is ASSUMED that the caller already excluded the empty-sequence case.
    --
    -- NOTE: 'packedPrimitivesR' is probably faster than 'packedPrimitivesF'
    -- because 'Encode.MessageBuilder' encodes in reverse.
    packedPrimitivesF :: Foldable f => (b -> a) -> FieldNumber -> f b -> Encode.MessageBuilder

    -- | Like 'packedPrimitivesF' but reverses the order of the elements.
    --
    -- NOTE: 'packedPrimitivesR' is probably faster than 'packedPrimitivesF'
    -- because 'Encode.MessageBuilder' encodes in reverse.
    packedPrimitivesR :: Foldable f => (b -> a) -> FieldNumber -> f b -> Encode.MessageBuilder

    -- | A faster but more specialized variant of 'packedPrimitivesF'.
    packedPrimitivesV ::
      Data.Vector.Generic.Vector v b => (b -> a) -> FieldNumber -> v b -> Encode.MessageBuilder

instance PackedPrimitives Bool
  where
    packedPrimitivesF = Encode.packedBoolsF
    packedPrimitivesR = Encode.packedBoolsR
    packedPrimitivesV = Encode.packedBoolsV

instance PackedPrimitives Word64
  where
    packedPrimitivesF = Encode.packedVarintsF
    packedPrimitivesR = Encode.packedVarintsR
    packedPrimitivesV = Encode.packedVarintsV

instance PackedPrimitives Word32
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Word32 @Word64 . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Word32 @Word64 . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fromIntegral @Word32 @Word64 . f)
    {-# INLINE packedPrimitivesV #-}

-- | NOTE: Converts to 'Word64' as before encoding, which is correct.
-- To quote <https://protobuf.dev/programming-guides/encoding/#signed-ints>,
-- "The intN types encode negative numbers as two’s complement,
-- which means that, as unsigned, 64-bit integers, they have their highest
-- bit set.  As a result, this means that all ten bytes must be used."
instance PackedPrimitives Int32
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Int32 @Word64 . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Int32 @Word64 . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fromIntegral @Int32 @Word64 . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives Int64
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Int64 @Word64 . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Int64 @Word64 . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fromIntegral @Int64 @Word64 . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Signed Int32)
  where
    packedPrimitivesF f = packedPrimitivesF (zigZagEncode . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (zigZagEncode . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (zigZagEncode . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Signed Int64)
  where
    packedPrimitivesF f = packedPrimitivesF (zigZagEncode . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (zigZagEncode . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (zigZagEncode . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Fixed Word32)
  where
    packedPrimitivesF f = Encode.packedFixed32F (fixed . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = Encode.packedFixed32R (fixed . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = Encode.packedFixed32V (fixed . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Fixed Word64)
  where
    packedPrimitivesF f = Encode.packedFixed64F (fixed . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = Encode.packedFixed64R (fixed . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = Encode.packedFixed64V (fixed . f)
    {-# INLINE packedPrimitivesV #-}

instance PackedPrimitives (Signed (Fixed Int32))
  where
    packedPrimitivesF f = packedPrimitivesF (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesV #-}

fixed32ToUnsigned :: Signed (Fixed Int32) -> Fixed Word32
fixed32ToUnsigned = Fixed . fromIntegral @Int32 @Word32 . fixed . signed
{-# INLINE fixed32ToUnsigned #-}

instance PackedPrimitives (Signed (Fixed Int64))
  where
    packedPrimitivesF f = packedPrimitivesF (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesV #-}

fixed64ToUnsigned :: Signed (Fixed Int64) -> Fixed Word64
fixed64ToUnsigned = Fixed . fromIntegral @Int64 @Word64 . fixed . signed
{-# INLINE fixed64ToUnsigned #-}

instance PackedPrimitives Float
  where
    packedPrimitivesF = Encode.packedFloatsF
    packedPrimitivesR = Encode.packedFloatsR
    packedPrimitivesV = Encode.packedFloatsV

instance PackedPrimitives Double
  where
    packedPrimitivesF = Encode.packedDoublesF
    packedPrimitivesR = Encode.packedDoublesR
    packedPrimitivesV = Encode.packedDoublesV

instance ProtoEnum e => PackedPrimitives (Enumerated e)
  where
    packedPrimitivesF f = packedPrimitivesF (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesR #-}

    packedPrimitivesV f = packedPrimitivesV (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesV #-}

-- | Pass through those values that are outside the enum range;
-- this is for forward compatibility as enumerations are extended.
codeFromEnumerated :: ProtoEnum e => Enumerated e -> Int32
codeFromEnumerated = either id fromProtoEnum . enumerated
{-# INLINE codeFromEnumerated #-}

data Forward a = forall f b . Foldable f => Forward (b -> a) (f b)

instance Functor Forward
  where
    fmap g (Forward f xs) = Forward (g . f) xs

data Reverse a = forall f b . Foldable f => Reverse (b -> a) (f b)

instance Functor Reverse
  where
    fmap g (Reverse f xs) = Reverse (g . f) xs

data Vector a = forall v b . Data.Vector.Generic.Vector v b => Vector (b -> a) (v b)

instance Functor Vector
  where
    fmap g (Vector f xs) = Vector (g . f) xs

-- | Applies a function that yields the encoding of repeatable field(s)
-- at every element of a collection and combines the results in order.
--
-- For efficiency, consider using 'foldPrefixR' or 'foldPrefixV' instead.
foldPrefixF ::
  forall t a message names .
  Foldable t =>
  (a -> Prefix message names names) ->
  t a ->
  Prefix message names names
foldPrefixF f =
  UnsafePrefix . Encode.etaMessageBuilder (foldr ((<>) . untypedPrefix . f) mempty)
{-# INLINE foldPrefixF #-}

-- | Applies a function that yields the encoding of repeatable field(s)
-- at every element of a collection and combines the results in /reverse/ order.
--
-- For vectors, consider 'foldPrefixV'.
foldPrefixR ::
  forall t a message names .
  Foldable t =>
  (a -> Prefix message names names) ->
  t a ->
  Prefix message names names
foldPrefixR f =
  UnsafePrefix . Encode.etaMessageBuilder (foldr (flip (<>) . untypedPrefix . f) mempty)
{-# INLINE foldPrefixR #-}

-- | Equivalent to 'foldPrefixF' specialized to vectors, but iterates right to left for efficiency.
foldPrefixV ::
  forall v a message names .
  Data.Vector.Generic.Vector v a =>
  (a -> Prefix message names names) ->
  v a ->
  Prefix message names names
foldPrefixV f =
  UnsafePrefix . Encode.vectorMessageBuilder @v @a (untypedPrefix . f)
{-# INLINE foldPrefixV #-}

instantiatePackableField :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q TH.Exp -> TH.Q [TH.Dec]
instantiatePackableField protoType elementType conversion =
  [d|

    instance ( RepetitionOf message name ~ 'Singular 'Implicit
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name $elementType message ('Singular 'Implicit) $protoType
      where
        fieldImpl x = primitiveField @name ($conversion x)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Singular 'Optional
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name $elementType message ('Singular 'Optional) $protoType
      where
        fieldImpl x = primitiveField @name ($conversion x)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Alternative
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name $elementType message 'Alternative $protoType
      where
        fieldImpl x = primitiveField @name ($conversion x)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Identity $elementType) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl (Identity x) = primitiveField @name (Identity ($conversion x))
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Forward $elementType) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl (Forward f xs) = primitiveField @name (Forward ($conversion . f) xs)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Reverse $elementType) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl (Reverse f xs) = primitiveField @name (Reverse ($conversion . f) xs)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Vector $elementType) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl (Vector f xs) = primitiveField @name (Vector ($conversion . f) xs)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Identity $elementType) message ('Repeated 'Packed) $protoType
      where
        fieldImpl (Identity x) = primitiveField @name (Identity ($conversion x))
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Forward $elementType) message ('Repeated 'Packed) $protoType
      where
        fieldImpl (Forward f xs) = primitiveField @name (Forward ($conversion . f) xs)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Reverse $elementType) message ('Repeated 'Packed) $protoType
      where
        fieldImpl (Reverse f xs) = primitiveField @name (Reverse ($conversion . f) xs)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Vector $elementType) message ('Repeated 'Packed) $protoType
      where
        fieldImpl (Vector f xs) = primitiveField @name (Vector ($conversion . f) xs)
        {-# INLINE fieldImpl #-}

    |]

instantiateStringOrBytesField :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q [TH.Dec]
instantiateStringOrBytesField protoType elementTC =
  [d|

    instance ( RepetitionOf message name ~ 'Singular 'Implicit
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , HasDefault ($elementTC a)
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name a message ('Singular 'Implicit) $protoType
      where
        fieldImpl x = primitiveField @name (coerce @a @($elementTC a) x)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Singular 'Optional
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , HasDefault ($elementTC a)
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name a message ('Singular 'Optional) $protoType
      where
        fieldImpl x = primitiveField @name (coerce @a @($elementTC a) x)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Alternative
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name a message 'Alternative $protoType
      where
        fieldImpl = primitiveField @name @($elementTC a) . coerce @a @($elementTC a)
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Identity a) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl = primitiveField @name . coerce @(Identity a) @(Identity ($elementTC a))
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Forward a) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl = primitiveField @name . coerce @(Forward a) @(Forward ($elementTC a))
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Reverse a) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl = primitiveField @name . coerce @(Reverse a) @(Reverse ($elementTC a))
        {-# INLINE fieldImpl #-}

    instance ( RepetitionOf message name ~ 'Repeated 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Vector a) message ('Repeated 'Unpacked) $protoType
      where
        fieldImpl = primitiveField @name . coerce @(Vector a) @(Vector ($elementTC a))
        {-# INLINE fieldImpl #-}

    |]