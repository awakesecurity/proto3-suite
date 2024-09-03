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
  ( Encoding(..)
  , toLazyByteString
  , Prefix(..)
  , Distinct
  , messageFromFields
  , Occupy
  , omitted
  , KnownFieldNumber
  , fieldNumber
  , Field
  , field
  , FieldImpl
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
         (Association, NumberOf, OneOfOf, Optional, RecoverProtoType,
          Repetition(..), RepetitionOf, ProtoType(..), ProtoTypeOf, Wrapped(..))
import Proto3.Suite.Types (Enumerated(..), Fixed(..), Signed(..))
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Types (FieldNumber(..))

-- | Annotates 'Encode.MessageBuilder' with the type of protobuf message it encodes.
-- Prefix a tag and length to turn the message into a submessage of a larger message.
newtype Encoding (a :: Type) = UnsafeEncoding { untypedEncoding :: Encode.MessageBuilder }

type role Encoding nominal

-- | Serialize a message (or portion thereof) as a lazy 'BL.ByteString'.
toLazyByteString :: forall a . Encoding a -> BL.ByteString
toLazyByteString = Encode.toLazyByteString . untypedEncoding

-- | A 'Category' on builders that prefix zero or more fields to a message.
-- Use '.' to accumulate prefixes to create an 'Encoding' for a whole message.
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
    OccupiedOnly1 message name names 'Singular = name ': OccupiedOnly message names
    OccupiedOnly1 message name names 'OneOf = OneOfOf message name ': OccupiedOnly message names
    OccupiedOnly1 message name names 'Unpacked = OccupiedOnly message names
    OccupiedOnly1 message name names 'Packed = OccupiedOnly message names

-- | Relabels a prefix of fields of a message as an encoding for
-- the message as a whole (though without any tag or length that
-- would make it a submessage).
messageFromFields ::
  forall (message :: Type) (names :: [Symbol]) .
  Distinct message names =>
  Prefix message '[] names ->
  Encoding message
messageFromFields = UnsafeEncoding . untypedPrefix

type Occupy (message :: Type) (name :: Symbol) (names :: [Symbol]) =
  Occupy1 message name names (RepetitionOf message name)

type family Occupy1 (message :: Type) (name :: Symbol) (names :: [Symbol])
                    (repetition :: Repetition) :: [Symbol]
  where
    Occupy1 message name names 'Singular = name ': names
    Occupy1 message name names 'OneOf = OneOfOf message name ': names
    Occupy1 message name names 'Unpacked = names
    Occupy1 message name names 'Packed = names

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
type Field (name :: Symbol) (a :: Type) (message :: Type) =
  FieldImpl name a message (RepetitionOf message name) (ProtoTypeOf message name)

-- | @'Field' name a message@ but with additional type
-- parameters that are determined by the original ones.
class ( RepetitionOf message name ~ repetition
      , ProtoTypeOf message name ~ protoType
      ) =>
      FieldImpl (name :: Symbol) (a :: Type) (message :: Type)
                (repetition :: Repetition) (protoType :: ProtoType)
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
    field :: forall names . a -> Prefix message names (Occupy message name names)

-- | Populated non-repeated submessage not inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Singular
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Encoding inner) outer 'Singular ('Message inner)
  where
    field = UnsafePrefix . Encode.embedded (fieldNumber @outer @name) . untypedEncoding
    {-# INLINE field #-}

-- | Optional non-repeated submessage not inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Singular
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Maybe (Encoding inner)) outer 'Singular ('Message inner)
  where
    field = maybe omitted (field @name)
    {-# INLINE field #-}

-- | Submessage inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'OneOf
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Encoding inner) outer 'OneOf ('Message inner)
  where
    field = UnsafePrefix . Encode.embedded (fieldNumber @outer @name) . untypedEncoding
    {-# INLINE field #-}

-- | Single submessage within a repeated submessage field.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Identity (Encoding inner)) outer 'Unpacked ('Message inner)
  where
    field =
      UnsafePrefix . Encode.embedded (fieldNumber @outer @name) . untypedEncoding . runIdentity
    {-# INLINE field #-}

-- | Repeated submessage, given as a 'Foldable' sequence in the correct order.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Forward (Encoding inner)) outer 'Unpacked ('Message inner)
  where
    field (Forward f xs) = foldPrefixF (field @name . Identity . f) xs
    {-# INLINE field #-}

-- | Repeated submessage, given as a 'Foldable' sequence in reverse order.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Reverse (Encoding inner)) outer 'Unpacked ('Message inner)
  where
    field (Reverse f xs) = foldPrefixR (field @name . Identity . f) xs
    {-# INLINE field #-}

-- | Repeated submessage, given as a `Data.Vector.Generic.Vector` in the correct order.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message inner
         , KnownFieldNumber outer name
         ) =>
         FieldImpl name (Vector (Encoding inner)) outer 'Unpacked ('Message inner)
  where
    field (Vector f xs) = foldPrefixV (field @name . Identity . f) xs
    {-# INLINE field #-}

-- | One of many key-value pairs within a protobuf map field.
instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Identity (Encoding (Association key value))) message 'Unpacked
                   ('Map key value)
  where
    field =
      UnsafePrefix . Encode.embedded (fieldNumber @message @name) . untypedEncoding . runIdentity
    {-# INLINE field #-}

-- | Key-value pairs of a protobuf map field, given as a 'Foldable' sequence in the correct order.
instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Forward (Encoding (Association key value))) message 'Unpacked
                   ('Map key value)
  where
    field (Forward f xs) = foldPrefixF (field @name . Identity . f) xs
    {-# INLINE field #-}

-- | Key-value pairs of a protobuf map field, given as a 'Foldable' sequence in reverse order.
instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Reverse (Encoding (Association key value))) message 'Unpacked
                   ('Map key value)
  where
    field (Reverse f xs) = foldPrefixR (field @name . Identity . f) xs
    {-# INLINE field #-}

-- | Key-value pairs of a protobuf map field, given as
-- a `Data.Vector.Generic.Vector` in the correct order.
instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Map key value
         , KnownFieldNumber message name
         ) =>
         FieldImpl name (Vector (Encoding (Association key value))) message 'Unpacked ('Map key value)
  where
    field (Vector f xs) = foldPrefixV (field @name . Identity . f) xs
    {-# INLINE field #-}

-- | Optional non-repeated wrapped scalar not inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'Singular
         , ProtoTypeOf outer name ~ 'Message (Optional protoType)
         , KnownFieldNumber outer name
         , FieldImpl "value" b (Optional protoType) 'Singular wrappedProtoType
         ) =>
         FieldImpl name (Maybe (Wrapped b)) outer 'Singular ('Message (Optional protoType))
  where
    field = field @name @(Maybe (Encoding (Optional protoType))) .
            fmap @Maybe (messageFromFields . field @"value" @b . wrapped)
    {-# INLINE field #-}

-- | Wrapped scalar inside of a @oneof@.
instance ( RepetitionOf outer name ~ 'OneOf
         , ProtoTypeOf outer name ~ 'Message (Optional protoType)
         , KnownFieldNumber outer name
         , FieldImpl "value" b (Optional protoType) 'Singular wrappedProtoType
         ) =>
         FieldImpl name (Wrapped b) outer 'OneOf ('Message (Optional protoType))
  where
    field = field @name @(Encoding (Optional protoType)) .
            messageFromFields . field @"value" @b . wrapped
    {-# INLINE field #-}

-- | Single wrapped scalar within a repeated field.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Optional protoType)
         , KnownFieldNumber outer name
         , FieldImpl "value" b (Optional protoType) 'Singular wrappedProtoType
         ) =>
         FieldImpl name (Identity (Wrapped b)) outer 'Unpacked ('Message (Optional protoType))
  where
    field = field @name @(Identity (Encoding (Optional protoType))) .
            fmap @Identity (messageFromFields . field @"value" @b . wrapped)
    {-# INLINE field #-}

-- | Repeated wrapped scalar, given as a 'Foldable' sequence in the correct order.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Optional protoType)
         , KnownFieldNumber outer name
         , FieldImpl "value" b (Optional protoType) 'Singular wrappedProtoType
         ) =>
         FieldImpl name (Forward (Wrapped b)) outer 'Unpacked ('Message (Optional protoType))
  where
    field = field @name @(Forward (Encoding (Optional protoType))) .
            fmap @Forward (messageFromFields . field @"value" @b . wrapped)
    {-# INLINE field #-}

-- | Repeated wrapped scalar, given as a 'Foldable' sequence in reverse order.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Optional protoType)
         , KnownFieldNumber outer name
         , FieldImpl "value" b (Optional protoType) 'Singular wrappedProtoType
         ) =>
         FieldImpl name (Reverse (Wrapped b)) outer 'Unpacked ('Message (Optional protoType))
  where
    field = field @name @(Reverse (Encoding (Optional protoType))) .
            fmap @Reverse (messageFromFields . field @"value" @b . wrapped)
    {-# INLINE field #-}

-- | Repeated wrapped scalar, given as a `Data.Vector.Generic.Vector` in the correct order.
instance ( RepetitionOf outer name ~ 'Unpacked
         , ProtoTypeOf outer name ~ 'Message (Optional protoType)
         , KnownFieldNumber outer name
         , FieldImpl "value" b (Optional protoType) 'Singular wrappedProtoType
         ) =>
         FieldImpl name (Vector (Wrapped b)) outer 'Unpacked ('Message (Optional protoType))
  where
    field = field @name @(Vector (Encoding (Optional protoType))) .
            fmap @Vector (messageFromFields . field @"value" @b . wrapped)
    {-# INLINE field #-}

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

instance ( RepetitionOf message name ~ 'Singular
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , HasDefault a
         , Primitive a
         ) =>
         PrimitiveField name a message 'Singular protoType
  where
    primitiveField value
      | isDefault value = UnsafePrefix mempty
      | otherwise = UnsafePrefix (encodePrimitive (fieldNumber @message @name) value)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'OneOf
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name a message 'OneOf protoType
  where
    primitiveField = UnsafePrefix . encodePrimitive (fieldNumber @message @name)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Identity a) message 'Unpacked protoType
  where
    primitiveField = UnsafePrefix . encodePrimitive (fieldNumber @message @name) . runIdentity
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Forward a) message 'Unpacked protoType
  where
    primitiveField (Forward f xs) = foldPrefixF (primitiveField @name . Identity . f) xs
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Reverse a) message 'Unpacked protoType
  where
    primitiveField (Reverse f xs) = foldPrefixR (primitiveField @name . Identity . f) xs
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Vector a) message 'Unpacked protoType
  where
    primitiveField (Vector f xs) = foldPrefixV (primitiveField @name . Identity . f) xs
    {-# INLINE primitiveField #-}

-- | NOTE: This instance produces the same encoding as for @[packed=false]@,
-- because packing does not improve efficiency for length-one sequences, and
-- because the protobuf specification requires parsers to handle that format.
instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , Primitive a
         ) =>
         PrimitiveField name (Identity a) message 'Packed protoType
  where
    primitiveField = UnsafePrefix . encodePrimitive (fieldNumber @message @name) . runIdentity
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , PackedPrimitives a
         ) =>
         PrimitiveField name (Forward a) message 'Packed protoType
  where
    primitiveField (Forward f xs)
      | null xs = UnsafePrefix mempty
      | otherwise = UnsafePrefix (packedPrimitivesF f (fieldNumber @message @name) xs)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , PackedPrimitives a
         ) =>
         PrimitiveField name (Reverse a) message 'Packed protoType
  where
    primitiveField (Reverse f xs)
      | null xs = UnsafePrefix mempty
      | otherwise = UnsafePrefix (packedPrimitivesR f (fieldNumber @message @name) xs)
    {-# INLINE primitiveField #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ protoType
         , RecoverProtoType a ~ protoType
         , KnownFieldNumber message name
         , PackedPrimitives a
         ) =>
         PrimitiveField name (Vector a) message 'Packed protoType
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

    instance ( RepetitionOf message name ~ 'Singular
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name $elementType message 'Singular $protoType
      where
        field = primitiveField @name . $conversion
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'OneOf
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name $elementType message 'OneOf $protoType
      where
        field = primitiveField @name . $conversion
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Identity $elementType) message 'Unpacked $protoType
      where
        field = primitiveField @name . fmap @Identity $conversion
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Forward $elementType) message 'Unpacked $protoType
      where
        field (Forward f xs) = primitiveField @name (Forward ($conversion . f) xs)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Reverse $elementType) message 'Unpacked $protoType
      where
        field (Reverse f xs) = primitiveField @name (Reverse ($conversion . f) xs)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Vector $elementType) message 'Unpacked $protoType
      where
        field (Vector f xs) = primitiveField @name (Vector ($conversion . f) xs)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Identity $elementType) message 'Packed $protoType
      where
        field = primitiveField @name . fmap @Identity $conversion
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Forward $elementType) message 'Packed $protoType
      where
        field (Forward f xs) = primitiveField @name (Forward ($conversion . f) xs)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Reverse $elementType) message 'Packed $protoType
      where
        field (Reverse f xs) = primitiveField @name (Reverse ($conversion . f) xs)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Packed
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             ) =>
             FieldImpl name (Vector $elementType) message 'Packed $protoType
      where
        field (Vector f xs) = primitiveField @name (Vector ($conversion . f) xs)
        {-# INLINE field #-}

    |]

instantiateStringOrBytesField :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q [TH.Dec]
instantiateStringOrBytesField protoType elementTC =
  [d|

    instance ( RepetitionOf message name ~ 'Singular
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , HasDefault ($elementTC a)
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name a message 'Singular $protoType
      where
        field = primitiveField @name . coerce @a @($elementTC a)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'OneOf
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name a message 'OneOf $protoType
      where
        field = primitiveField @name @($elementTC a) . coerce @a @($elementTC a)
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Identity a) message 'Unpacked $protoType
      where
        field = primitiveField @name . coerce @(Identity a) @(Identity ($elementTC a))
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Forward a) message 'Unpacked $protoType
      where
        field = primitiveField @name . coerce @(Forward a) @(Forward ($elementTC a))
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Reverse a) message 'Unpacked $protoType
      where
        field = primitiveField @name . coerce @(Reverse a) @(Reverse ($elementTC a))
        {-# INLINE field #-}

    instance ( RepetitionOf message name ~ 'Unpacked
             , ProtoTypeOf message name ~ $protoType
             , KnownFieldNumber message name
             , Primitive ($elementTC a)
             ) =>
             FieldImpl name (Vector a) message 'Unpacked $protoType
      where
        field = primitiveField @name . coerce @(Vector a) @(Vector ($elementTC a))
        {-# INLINE field #-}

    |]
