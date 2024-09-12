{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
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
  , RawField(..)
  , Forward(..)
  , Reverse(..)
  , Vector(..)
  , FoldBuilders(..)
  , codeFromEnumerated
  , Reflection(..)
  , instantiatePackableField
  , instantiateStringOrBytesField
  ) where

import Control.Category (Category(..))
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Vector.Generic qualified
import Data.Word (Word32, Word64)
import GHC.Exts (Constraint, Proxy#, TYPE, proxy#)
import GHC.TypeLits (ErrorMessage(..), KnownNat, Symbol, TypeError, natVal')
import Language.Haskell.TH qualified as TH
import Prelude hiding ((.), id)
import Proto3.Suite.Class
         (HasDefault(..), MessageField, Primitive(..), encodeMessageField, zigZagEncode)
import Proto3.Suite.Form
         (Association, MessageFieldType, NumberOf, Omission(..), OneOfOf,
          Packing(..), RecoverProtoType, Repetition(..), RepetitionOf,
          ProtoType(..), ProtoTypeOf, Wrap(..), Wrapper)
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
    OccupiedOnly1 message name names ('Singular 'Alternative) =
      OneOfOf message name ': OccupiedOnly message names
    OccupiedOnly1 message name names ('Singular 'Implicit) =
      name ': OccupiedOnly message names
    OccupiedOnly1 message name names 'Optional =
      name ': OccupiedOnly message names
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
    Occupy1 message name names ('Singular 'Alternative) = OneOfOf message name ': names
    Occupy1 message name names ('Singular 'Implicit) = name ': names
    Occupy1 message name names 'Optional = name ': names
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
type Field :: Symbol -> forall {r} . TYPE r -> Type -> Constraint
class Field name a message
  where
    -- | Encodes the named field from the given value.
    --
    -- If the field is neither repeated nor part of a oneof, then its default
    -- value is represented implicitly--that is, it encodes to zero octets.
    --
    -- Use @TypeApplications@ to specify the field name as the first type parameter.
    --
    -- You may also need to specify the argument type as the second type parameter
    -- when the argument expression is polymorphic.  For example, when passing
    -- an integer literal, or a string literal if you use @OverloadedStrings@.
    -- (The runtime representation of the argument type is found by unification.)
    --
    -- If the argument type is too verbose, then we recommend
    -- wrapping calls to 'field' in helper functions select it
    -- automatically based on particular use cases.  Examples:
    -- `Proto3.Suite.Form.Encode.message`,
    -- `Proto3.Suite.Form.Encode.associations`.
    --
    -- See also 'field'.
    field :: forall names . a -> Prefix message names (Occupy message name names)

instance forall (name :: Symbol)
#if MIN_VERSION_ghc(9,4,0)
                r (a :: TYPE r)
#else
                (a :: Type)
                  -- Regarding the call to @coerce@, GHC 9.2.8 would say:
                  -- "Cannot use function with levity-polymorphic arguments".
                  -- So we just drop support for unlifted arguments until GHC 9.4.
#endif
                (message :: Type) .
         ( KnownFieldNumber message name
         , RawField (RepetitionOf message name) (ProtoTypeOf message name) a
         ) =>
         Field name a message
  where
    field :: forall names . a -> Prefix message names (Occupy message name names)
    field = coerce
      @(a -> Encode.MessageBuilder)
      @(a -> Prefix message names (Occupy message name names))
      (rawField @(RepetitionOf message name) @(ProtoTypeOf message name) @a
                   (fieldNumber @message @name))
      -- Implementation Note: Using the newtype constructor would require us
      -- to bind a variable of kind @TYPE r@, which is runtime-polymorphic.
      -- By using a coercion we avoid runtime polymorphism restrictions.
    {-# INLINE field #-}

type RawField :: Repetition -> ProtoType -> forall {r} . TYPE r -> Constraint
class RawField repetition protoType a
  where
    -- | Encodes a message field with the
    -- given number from the given value.
    --
    -- If the field is neither @repeated@, nor @optional@, nor
    -- part of a @oneof@, then its default value is represented
    -- implicitly--that is, it encodes to zero octets.
    --
    -- Use @TypeApplications@ to specify
    -- the repetition and protobuf type.
    --
    -- If you apply this method to a polymorphic expression,
    -- such as a literal value, then you may need to choose
    -- a particular type with @TypeApplications@ or @::@.
    --
    -- If the argument type is too verbose, then we recommend
    -- wrapping calls to 'field' in helper functions select it
    -- automatically based on particular use cases.  Examples:
    -- `Proto3.Suite.Form.Encode.message`,
    -- `Proto3.Suite.Form.Encode.associations`.
    rawField :: FieldNumber -> a -> Encode.MessageBuilder

instance (omission ~ 'Alternative) =>
         RawField ('Singular omission) ('Message inner) (MessageEncoding inner)
  where
    rawField !fn e = Encode.embedded fn (untypedMessageEncoding e)
    {-# INLINE rawField #-}

instance RawField 'Optional ('Message inner) (Maybe (MessageEncoding inner))
  where
    rawField !fn = foldMap (Encode.embedded fn . untypedMessageEncoding)
    {-# INLINE rawField #-}

instance ( packing ~ 'Unpacked
         , FoldBuilders t
         ) =>
         RawField ('Repeated packing) ('Message inner) (t (MessageEncoding inner))
  where
    rawField !fn es = foldBuilders (Encode.embedded fn . untypedMessageEncoding <$> es)
    {-# INLINE rawField #-}

instance ( repetition ~ 'Repeated 'Unpacked
         , FoldBuilders t
         ) =>
         RawField repetition ('Map key value) (t (MessageEncoding (Association key value)))
  where
    rawField !fn es = foldBuilders (Encode.embedded fn . untypedMessageEncoding <$> es)
    {-# INLINE rawField #-}

instance ( omission ~ 'Alternative
         , RawField ('Singular 'Implicit) protoType a
         ) =>
         RawField ('Singular omission) ('Message (Wrapper protoType)) (Wrap a)
  where
    rawField !fn (Wrap x) =
      rawField @('Singular omission) @('Message (Wrapper protoType))
        fn (fieldsToMessage @(Wrapper protoType) (field @"value" @a x))
    {-# INLINE rawField #-}

instance RawField ('Singular 'Implicit) protoType a =>
         RawField 'Optional ('Message (Wrapper protoType)) (Maybe (Wrap a))
  where
    rawField !fn m =
      rawField @'Optional @('Message (Wrapper protoType))
        fn (fmap @Maybe (\(Wrap x) -> fieldsToMessage @(Wrapper protoType) (field @"value" @a x)) m)
    {-# INLINE rawField #-}

instance (packing ~ 'Unpacked, FoldBuilders t, RawField ('Singular 'Implicit) protoType a) =>
         RawField ('Repeated packing) ('Message (Wrapper protoType)) (t (Wrap a))
  where
    rawField !fn es =
      rawField @('Repeated packing) @('Message (Wrapper protoType))
        fn (fmap @t (\(Wrap x) -> fieldsToMessage @(Wrapper protoType) (field @"value" @a x)) es)
    {-# INLINE rawField #-}

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

-- | Implements 'RawField' for scalar types.
--
-- This implementation detail should be invisible to package clients.
type EncodeScalarField :: Repetition -> ProtoType -> forall {r} . TYPE r -> Constraint
class EncodeScalarField repetition protoType a
  where
    encodeScalarField :: FieldNumber -> a -> Encode.MessageBuilder

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , Primitive a
         ) =>
         EncodeScalarField ('Singular 'Alternative) protoType a
  where
    encodeScalarField = encodePrimitive
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , HasDefault a
         , Primitive a
         ) =>
         EncodeScalarField ('Singular 'Implicit) protoType a
  where
    encodeScalarField !fn x
      | isDefault x = mempty
      | otherwise = encodePrimitive fn x
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , Primitive a
         ) =>
         EncodeScalarField 'Optional protoType (Maybe a)
  where
    encodeScalarField !fn = maybe mempty (encodePrimitive fn)
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , FoldBuilders t
         , Primitive a
         ) =>
         EncodeScalarField ('Repeated 'Unpacked) protoType (t a)
  where
    encodeScalarField !fn xs = foldBuilders (encodePrimitive fn <$> xs)
    {-# INLINE encodeScalarField #-}

-- | Ignores the preference for packed format because there is exactly one element,
-- and therefore packed format would be more verbose.  Conforming parsers must
-- accept both packed and unpacked primitives regardless of packing preference.
instance ( CompatibleScalar (RecoverProtoType a) protoType
         , Primitive a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Identity a)
  where
    encodeScalarField !fn (Identity x) = encodePrimitive fn x
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , PackedPrimitives a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Forward a)
  where
    encodeScalarField !fn (Forward f xs)
      | null xs = mempty
      | otherwise = packedPrimitivesF f fn xs
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , PackedPrimitives a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Reverse a)
  where
    encodeScalarField !fn (Reverse f xs)
      | null xs = mempty
      | otherwise = packedPrimitivesR f fn xs
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , PackedPrimitives a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Vector a)
  where
    encodeScalarField !fn (Vector f xs)
      | Data.Vector.Generic.null xs = mempty
      | otherwise = packedPrimitivesV f fn xs
    {-# INLINE encodeScalarField #-}

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

-- | Inhabited by type constructors providing an efficient way to fold over
-- contained builders and an 'fmap' that is efficient when followed by that
-- operation.  For example, 'Vector' does not immediately create a new vector
-- when 'fmap' is applied; instead it tracks the overall mapping to be applied,
-- then applies it to each element during the fold.
class Functor t =>
      FoldBuilders t
  where
    foldBuilders :: t Encode.MessageBuilder -> Encode.MessageBuilder

    foldPrefixes ::
      forall message names . t (Prefix message names names) -> Prefix message names names
    default foldPrefixes ::
      forall message names .
      Coercible (t Encode.MessageBuilder) (t (Prefix message names names)) =>
      t (Prefix message names names) -> Prefix message names names
    foldPrefixes = coerce (foldBuilders @t)
    {-# INLINE foldPrefixes #-}

instance FoldBuilders Identity
  where
    foldBuilders = coerce
    {-# INLINE foldBuilders #-}

instance FoldBuilders Forward
  where
    -- | Concatenates the specified encodings in order.
    --
    -- For efficiency, consider using 'Reverse' or 'Vector' instead.
    foldBuilders (Forward f xs) = Encode.etaMessageBuilder (foldr ((<>) . f) mempty) xs
    {-# INLINE foldBuilders #-}

instance FoldBuilders Reverse
  where
    -- | Concatenates the specified encodings in the /reverse/ order specified by the argument.
    --
    -- For vectors consider 'Vector', but do not create a temporary vector just for that purpose.
    foldBuilders (Reverse f xs) = Encode.etaMessageBuilder (foldr (flip (<>) . f) mempty) xs
    {-# INLINE foldBuilders #-}

instance FoldBuilders Vector
  where
    -- | Produces the same result as would the 'Forward' instance, but exploits
    -- the vector representation to iterates right to left for efficiency.
    foldBuilders (Vector f xs) = Encode.vectorMessageBuilder f xs
    {-# INLINE foldBuilders #-}

-- | Pass through those values that are outside the enum range;
-- this is for forward compatibility as enumerations are extended.
codeFromEnumerated :: ProtoEnum e => Enumerated e -> Int32
codeFromEnumerated = either id fromProtoEnum . enumerated
{-# INLINE codeFromEnumerated #-}

-- | Signals that the argument to 'field' should be treated
-- as a reflection in Haskell of a protobuf construct, both
-- in its type and in its value.
--
-- For example, if the type argument is generated from a protobuf
-- message definition, then 'field' will encode the message whose
-- fields are given by the Haskell data type inside of this @newtype@.
--
-- Repeated fields must be supplied as an appropriately-typed sequence.
--
-- For this @newtype@, 'Field' delegates to 'MessageField' and has
-- its performance characteristics.  The creation of temporary
-- reflections of protobuf messages may decrease efficiency
-- in some cases.  However, you may find this @newtype@ useful
-- where a mix of techniques is needed, either for compatibility
-- or during a gradual transition to use of 'Field'.
newtype Reflection a = Reflection a

instance ( MessageFieldType repetition protoType a
         , MessageField a
         ) =>
         RawField repetition protoType (Reflection a)
  where
    rawField = coerce (encodeMessageField @a)
    {-# INLINE rawField #-}

instantiatePackableField :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q TH.Exp -> TH.Q [TH.Dec]
instantiatePackableField protoType elementType conversion =
  [d|

    instance RawField ('Singular 'Alternative) $protoType $elementType
      where
        rawField !fn x =
          encodeScalarField @('Singular 'Alternative) @($protoType) fn ($conversion x)
        {-# INLINE rawField #-}

    instance RawField ('Singular 'Implicit) $protoType $elementType
      where
        rawField !fn x =
          encodeScalarField @('Singular 'Implicit) @($protoType) fn ($conversion x)
        {-# INLINE rawField #-}

    instance RawField 'Optional $protoType (Maybe $elementType)
      where
        rawField !fn x =
          encodeScalarField @'Optional @($protoType) fn (fmap $conversion x)
        {-# INLINE rawField #-}

    instance FoldBuilders t =>
             RawField ('Repeated 'Unpacked) $protoType (t $elementType)
      where
        rawField !fn xs =
          encodeScalarField @('Repeated 'Unpacked) @($protoType) fn (fmap $conversion xs)
        {-# INLINE rawField #-}

    instance RawField ('Repeated 'Packed) $protoType (Identity $elementType)
      where
        rawField !fn xs =
          encodeScalarField @('Repeated 'Packed) @($protoType) fn (fmap $conversion xs)
        {-# INLINE rawField #-}

    instance RawField ('Repeated 'Packed) $protoType (Forward $elementType)
      where
        rawField !fn xs =
          encodeScalarField @('Repeated 'Packed) @($protoType) fn (fmap $conversion xs)
        {-# INLINE rawField #-}

    instance RawField ('Repeated 'Packed) $protoType (Reverse $elementType)
      where
        rawField !fn xs =
          encodeScalarField @('Repeated 'Packed) @($protoType) fn (fmap $conversion xs)
        {-# INLINE rawField #-}

    instance RawField ('Repeated 'Packed) $protoType (Vector $elementType)
      where
        rawField !fn xs =
          encodeScalarField @('Repeated 'Packed) @($protoType) fn (fmap $conversion xs)
        {-# INLINE rawField #-}

    |]

instantiateStringOrBytesField :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q [TH.Dec]
instantiateStringOrBytesField protoType elementTC =
  [d|

    instance forall a .
             Primitive ($elementTC a) =>
             RawField ('Singular 'Alternative) $protoType a
      where
        rawField !fn x =
          encodeScalarField @('Singular 'Alternative) @($protoType) fn (coerce @a @($elementTC a) x)
        {-# INLINE rawField #-}

    instance forall a .
             ( HasDefault ($elementTC a)
             , Primitive ($elementTC a)
             ) =>
             RawField ('Singular 'Implicit) $protoType a
      where
        rawField !fn x =
          encodeScalarField @('Singular 'Implicit) @($protoType) fn (coerce @a @($elementTC a) x)
        {-# INLINE rawField #-}

    instance forall a .
             Primitive ($elementTC a) =>
             RawField 'Optional $protoType (Maybe a)
      where
        rawField !fn x =
          encodeScalarField @'Optional @($protoType)
            fn (coerce @(Maybe a) @(Maybe ($elementTC a)) x)
        {-# INLINE rawField #-}

    instance forall t a .
             ( FoldBuilders t
             , Primitive ($elementTC a)
             ) =>
             RawField ('Repeated 'Unpacked) $protoType (t a)
      where
        rawField !fn xs =
          encodeScalarField @('Repeated 'Unpacked) @($protoType)
            fn (coerce @a @($elementTC a) <$> xs)
        {-# INLINE rawField #-}

    |]
