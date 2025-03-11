{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- | Implementation details of "Proto3.Suite.Form.Encode" that
-- must be kept separate for the sake of @TemplateHaskell@.
module Proto3.Suite.Form.Encode.Core
  ( MessageEncoder(..)
  , toLazyByteString
  , etaMessageEncoder
  , MessageEncoding
  , cacheMessageEncoder
  , cachedMessageEncoding
  , Prefix(..)
  , etaPrefix
  , Fields
  , cachePrefix
  , cachedFields
  , Distinct
  , DistinctCheck
  , RepeatedNames
  , RepeatedNames1
  , Omits
  , Strip
  , OccupiedOnly
  , OccupiedOnly1
  , fieldsToMessage
  , Occupy
  , Occupy1
  , NameSublist
  , omitted
  , KnownFieldNumber
  , fieldNumber
  , Field(..)
  , FieldForm(..)
  , Wrap(..)
  , codeFromEnumerated
  , instantiatePackableField
  , instantiateStringOrBytesField
  ) where

import Control.Category (Category(..))
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Traversable (for)
import Data.Word (Word32, Word64)
import GHC.Exts (Constraint, Proxy#, TYPE, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage(..), KnownNat, Symbol, TypeError, natVal')
import Language.Haskell.TH qualified as TH
import Prelude hiding ((.), id)
import Proto3.Suite.Class (HasDefault(..), Primitive(..), zigZagEncode)
import Proto3.Suite.Form
         (Association, NumberOf, Omission(..), OneOfOf,
          Packing(..), RecoverProtoType, Repetition(..), RepetitionOf,
          ProtoType(..), ProtoTypeOf, Wrapper)
import Proto3.Suite.Form.Encode.Repeated (FoldBuilders(..), Forward(..), Reverse(..), ReverseN(..))
import Proto3.Suite.Types (Enumerated(..), Fixed(..), Signed(..))
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Types (FieldNumber(..))

-- | Annotates 'Encode.MessageBuilder' with the type of protobuf message it encodes.
-- Prefix a tag and length to turn the message into a submessage of a larger message.
newtype MessageEncoder (message :: Type) = UnsafeMessageEncoder
  { untypedMessageEncoder :: Encode.MessageBuilder }

type role MessageEncoder nominal

-- | Serialize a message (or portion thereof) as a lazy 'BL.ByteString'.
toLazyByteString :: forall message . MessageEncoder message -> BL.ByteString
toLazyByteString = Encode.toLazyByteString . untypedMessageEncoder

-- | Like 'Encode.etaMessageBuilder' but for 'MessageEncoder'.
etaMessageEncoder :: forall a message . (a -> MessageEncoder message) -> a -> MessageEncoder message
etaMessageEncoder = coerce (Encode.etaMessageBuilder @a)

-- | The octet sequence that would be emitted by
-- some 'MessageEncoder' having the same type parameter.
--
-- See also: 'cacheMessageEncoder'
newtype MessageEncoding (message :: Type) =
  UnsafeMessageEncoding { untypedMessageEncoding :: B.ByteString }

type role MessageEncoding nominal

-- | Precomputes the octet sequence that would be written by the given 'MessageEncoder'.
-- Do this only if you expect to reuse that specific octet sequence repeatedly.
--
-- @'cachedMessageEncoding' . 'cacheMessageEncoder'@ is functionally equivalent to @id@
-- but has different performance characteristics.
--
-- See also: 'cachePrefix'.
cacheMessageEncoder :: MessageEncoder message -> MessageEncoding message
cacheMessageEncoder =
  UnsafeMessageEncoding . BL.toStrict . Encode.toLazyByteString . untypedMessageEncoder

-- | Encodes a precomputed 'MessageEncoder' by copying octets from a memory buffer.
-- See 'cacheMessageEncoder'.
--
-- See also: 'cachedFields'
cachedMessageEncoding :: MessageEncoding message -> MessageEncoder message
cachedMessageEncoding = UnsafeMessageEncoder . Encode.unsafeFromByteString . untypedMessageEncoding
{-# INLINE cachedMessageEncoding #-}

-- | A 'Category' on builders that prefix zero or more fields to a message.
-- Use '.' to accumulate prefixes to create an 'MessageEncoder' for a whole message.
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
--
-- See also: 'cachePrefix'
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

-- | Like 'Encode.etaMessageBuilder' but for 'Prefix'.
etaPrefix ::
  forall a message possible following .
  (a -> Prefix message possible following) ->
  a -> Prefix message possible following
etaPrefix = coerce (Encode.etaMessageBuilder @a)

-- | The octet sequence that would be prefixed by some 'Prefix' having the same type parameters.
newtype Fields (message :: Type) (possible :: [Symbol]) (following :: [Symbol]) =
  UnsafeFields { untypedFields :: B.ByteString }

type role Fields nominal nominal nominal

-- | Precomputes the octet sequence that would be written by the given 'Prefix'.
-- Do this only if you expect to reuse that specific octet sequence repeatedly.
--
-- @'cachedFields' . 'cachePrefix'@ is functionally equivalent to @id@
-- but has different performance characteristics.
--
-- See also: 'cacheMessageEncoder'
cachePrefix :: Prefix message possible following -> Fields message possible following
cachePrefix = UnsafeFields . BL.toStrict . Encode.toLazyByteString . untypedPrefix

-- | Encodes a precomputed 'Prefix' by copying octets from a memory buffer.
-- See 'cachePrefix'.
--
-- See also: 'cachedMessageEncoding'.
cachedFields :: Fields message possible following -> Prefix message possible following
cachedFields = UnsafePrefix . Encode.unsafeFromByteString . untypedFields
{-# INLINE cachedFields #-}

-- | Yields a satisfied constraint if the given list of names contains
-- no duplicates after first filtering out the names of repeated fields
-- and replacing the names of @oneof@ fields with their @oneof@ names.
-- Otherwise raises a compilation error mentioning the repeated names.
type Distinct (message :: Type) (names :: [Symbol]) =
  DistinctCheck message (RepeatedNames (OccupiedOnly message names))

-- | Reports nonempty output of 'RepeatedNames' as applied to the result of 'OccupiedOnly'.
--
-- This type family is an implementation detail of 'Distinct'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family DistinctCheck (message :: Type) (repeated :: [k]) :: Constraint
  where
    DistinctCheck _ '[] = ()
    DistinctCheck message repeated = TypeError
      ( 'ShowType message ':<>: 'Text " forbids repetition of:"
        ':$$: 'ShowType repeated )

-- | Given a list of names, returns the non-repeating list
-- of names that occur more than once in the given list.
--
-- This type family is an implementation detail of 'Distinct'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family RepeatedNames (names :: [k]) :: [k]
  where
    RepeatedNames (name ': names) = RepeatedNames1 name names (Omits name names)
    RepeatedNames '[] = '[]

-- | Helps to implement 'RepeatedNames'.
--
-- This type family is an implementation detail of 'Distinct'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family RepeatedNames1 (name :: k) (names :: [k]) (omits :: Bool) :: [k]
  where
    RepeatedNames1 _ names 'True = RepeatedNames names
    RepeatedNames1 name names 'False = name ': RepeatedNames (Strip name names)

-- | Is the given name absent from the given list of names?
--
-- This type family is an implementation detail of 'RepeatedNames'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family Omits (name :: k) (names :: [k]) :: Bool
  where
    Omits name (name ': names) = 'False
    Omits name (_ ': names) = Omits name names
    Omits name '[] = 'True

-- | Strips all occurrences of the given name, leaving behind all other name occurrences.
--
-- This type family is an implementation detail of 'RepeatedNames'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family Strip (name :: k) (names :: [k]) :: [k]
  where
    Strip name (name ': names) = Strip name names
    Strip name (other ': names) = other ': Strip name names
    Strip name '[] = '[]

-- | Filters out the repeated field names and replaces @oneof@ fields with their @oneof@ names.
--
-- We do this in case 'omitted' is used to introduce the names of repeated fields
-- or fields that are contained within @oneof@s; see the explanatory comments there.
--
-- This type family is an implementation detail of 'Distinct'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family OccupiedOnly (message :: Type) (names :: [Symbol]) :: [Symbol]
  where
    OccupiedOnly message (name ': names) =
      OccupiedOnly1 message name names (RepetitionOf message name)
    OccupiedOnly _ '[] =
      '[]

-- | Helps to implement 'OccupiedOnly'.
--
-- This type family is an implementation detail of 'Distinct'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
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
  MessageEncoder message
fieldsToMessage = UnsafeMessageEncoder . untypedPrefix

-- | Among the names of the given message, prefixes
-- the given name with the following exceptions:
--
-- * Names of repeatable fields are not prefixed;
--   there is no need to avoid their repetition.
--
-- * When a field of a @oneof@ is named, the name
--   of the entire @oneof@ is prefixed in order to
--   prevent further emission of any of its fields.
--
type Occupy (message :: Type) (name :: Symbol) (names :: [Symbol]) =
  Occupy1 message name names (RepetitionOf message name)

-- | Helps to implement 'Occupy'.
--
-- This type family is an implementation detail of 'Occupy'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family Occupy1 (message :: Type) (name :: Symbol) (names :: [Symbol])
                    (repetition :: Repetition) :: [Symbol]
  where
    Occupy1 message name names ('Singular 'Alternative) = OneOfOf message name ': names
    Occupy1 message name names ('Singular 'Implicit) = name ': names
    Occupy1 message name names 'Optional = name ': names
    Occupy1 message name names ('Repeated _) = names

-- | The constraint that 'moreNames' is 'names', but possibly with additional
-- names inserted.  For simplicity, reordering is not currently allowed.
type family NameSublist (names :: [Symbol]) (moreNames :: [Symbol]) :: Constraint
  where
    NameSublist '[] _ = (() :: Constraint)
    NameSublist (n ': ns) (n ': ms) = NameSublist ns ms
    NameSublist ns (_ ': ms) = NameSublist ns ms
    NameSublist (n ': _) '[] = TypeError
      ( 'Text "NameSublist: name disappeared: " ':<>: 'ShowType n )

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
  NameSublist names moreNames =>
  Prefix message names moreNames
omitted = UnsafePrefix mempty

-- | The constraint that provides the (term-level) field number corresponding
-- to the field of the given message type having the given (type-level) name.
type KnownFieldNumber (message :: Type) (name :: Symbol) = KnownNat (NumberOf message name)

-- | The term expressing the field number within the specified message type of the named field.
fieldNumber :: forall message name . KnownFieldNumber message name => FieldNumber
fieldNumber = FieldNumber (fromInteger (natVal' (proxy# :: Proxy# (NumberOf message name))))

-- | Provides a way to encode a field with the given name from a given type.
-- That name is interpreted in the context of a given type of message.
--
-- More than one argument type may be supported for any given field;
-- see further discussion in the comments for 'FieldForm', to which
-- this type class delegates after determining the repetition and
-- protobuf type of the field in question.
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
    -- If the argument type is too verbose, or should be chosen automatically
    -- in order to avoid accidentally using a narrower type that cannot represent
    -- all possible field values, then we recommend wrapping call to 'field' in
    -- a helper function in order to automatically select the argument type best
    -- suited to your particular use case.  Examples:
    -- `Proto3.Suite.Form.Encode.message`,
    -- `Proto3.Suite.Form.Encode.associations`,
    -- `Proto3.Suite.Form.Encode.number`.
    --
    -- See also 'fieldForm'.
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
         , FieldForm (RepetitionOf message name) (ProtoTypeOf message name) a
         ) =>
         Field name a message
  where
    field :: forall names . a -> Prefix message names (Occupy message name names)
    field = coerce
      @(a -> Encode.MessageBuilder)
      @(a -> Prefix message names (Occupy message name names))
      (fieldForm @(RepetitionOf message name) @(ProtoTypeOf message name) @a
                 proxy# proxy# (fieldNumber @message @name))
      -- Implementation Note: Using the newtype constructor would require us
      -- to bind a variable of kind @TYPE r@, which is runtime-polymorphic.
      -- By using a coercion we avoid runtime polymorphism restrictions.
    {-# INLINE field #-}

-- | Implements 'Field' for all fields having the specified repetition,
-- protobuf type, and type of argument to be encoded within that field.
--
-- Argument Type:
--
-- For any given repetition and protobuf type there may be multiple
-- instances of this class for different types of argument.  For example,
-- a field of protobuf type @sint64@ may be encoded from any of the Haskell
-- types `Data.Int.Int8`, `Data.Int.Word8`, `Data.Int.Int16`, `Data.Int.Word16`,
-- `Data.Int.Int32`, `Data.Int.Word32`, or `Data.Int.Int64`.  Note that this
-- library does /not/ provide an instance for `Data.Int.Word64` because its
-- values greater than or equal to @2 ^ 63@ cannot be represented by @sint64@.
--
-- As another example, for fields of submessage type @m@ there are type
-- class instances for both @'MessageEncoder' m@ and @'MessageEncoding' m@
-- (wrapped in a suitable container if repeated or outside of a @oneof@).
--
-- Arguments for optional fields are often expressed using 'Maybe',
-- and arguments for repeated fields are often expressed in using
-- 'Forward', 'Reverse', and 'Reverse'--the choice is up to the user.
--
-- Of course, if you add instances of this type class then please be
-- sure to consider how they might overlap with existing instances,
-- and try to avoid overly broad instances that might cause ambiguity.
--
-- However, this library does provide general instances for 'Optional'
-- and @'Repeated' 'Unpacked'@ that delegate to instances for
-- @'Singular' 'Alternative'@ for the same protobuf type.
--
-- Design Note:
--
-- Importantly, the type parameters of this type class do not mention
-- the message type, field name, or field number, thus allowing it to
-- be broadly applicable to all message types.
--
-- Furthermore, type class 'Field' has a general-purpose definition
-- that need not be specialized for particular message types: one that
-- makes use of 'KnownFieldNumber', 'ProtoTypeOf', and this type class
-- (though in order to simplify usage, 'Field' is a full type class,
-- not a mere constraint alias with a related function).
--
-- In this way the only message-specific instances are of type classes
-- defined in "Proto3.Suite.Form", which declare message format without
-- specifying any policy regarding how to efficiently encode or which
-- Haskell types may be encoded.
type FieldForm :: Repetition -> ProtoType -> forall {r} . TYPE r -> Constraint
class FieldForm repetition protoType a
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
    fieldForm :: Proxy# repetition -> Proxy# protoType -> FieldNumber -> a -> Encode.MessageBuilder

instance FieldForm ('Singular 'Alternative) protoType a =>
         FieldForm 'Optional protoType (Maybe a)
  where
    fieldForm _ ty !fn me =
      foldMap @Maybe (fieldForm (proxy# :: Proxy# ('Singular 'Alternative)) ty fn) me
    {-# INLINE fieldForm #-}

instance ( FoldBuilders t
         , FieldForm ('Singular 'Alternative) protoType a
         ) =>
         FieldForm ('Repeated 'Unpacked) protoType (t a)
  where
    fieldForm _ ty !fn es =
      foldBuilders (fieldForm (proxy# :: Proxy# ('Singular 'Alternative)) ty fn <$> es)
    {-# INLINE fieldForm #-}

instance (omission ~ 'Alternative) =>
         FieldForm ('Singular omission) ('Message inner) (MessageEncoder inner)
  where
    fieldForm _ _ !fn e = Encode.embedded fn (untypedMessageEncoder e)
    {-# INLINE fieldForm #-}

instance (omission ~ 'Alternative) =>
         FieldForm ('Singular omission) ('Message inner) (MessageEncoding inner)
  where
    fieldForm rep ty !fn e = fieldForm rep ty fn (cachedMessageEncoding e)
    {-# INLINE fieldForm #-}

-- | This instance is rather artificial because maps are automatically
-- repeated and unpacked, with no option to specify a single key-value
-- pair as a field of a @oneof@.  Hence the code generator should never
-- directly make use of this instance, but it will do so indirectly via
-- the general instance for repeated unpacked fields, which will then
-- delegate to this instance.
instance (omission ~ 'Alternative) =>
         FieldForm ('Singular omission) ('Map key value) (MessageEncoder (Association key value))
  where
    fieldForm _ _ !fn a = Encode.embedded fn (untypedMessageEncoder a)
    {-# INLINE fieldForm #-}

-- | This instance is rather artificial because maps are automatically
-- repeated and unpacked, with no option to specify a single key-value
-- pair as a field of a @oneof@.  Hence the code generator should never
-- directly make use of this instance, but it will do so indirectly via
-- the general instance for repeated unpacked fields, which will then
-- delegate to this instance.
instance (omission ~ 'Alternative) =>
         FieldForm ('Singular omission) ('Map key value) (MessageEncoding (Association key value))
  where
    fieldForm rep ty !fn e = fieldForm rep ty fn (cachedMessageEncoding e)
    {-# INLINE fieldForm #-}

-- | Indicates that a wrapper type should be emitted by encoding its field,
-- as opposed to by supplying 'MessageEncoder' for the wrapper as a whole.
--
-- We also provide specific instances of 'FieldForm' that avoid the need
-- to 'Wrap' certain commonly-used argument types, such as 'Int32'.
--
-- See also 'Wrapper'.
newtype Wrap (a :: Type) = Wrap { unwrap :: a }
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving newtype (Bounded, Enum, Eq, Fractional, Integral, Ord, Num, Read, Real, Show)

instance ( omission ~ 'Alternative
         , FieldForm ('Singular 'Implicit) protoType a
         ) =>
         FieldForm ('Singular omission) ('Message (Wrapper protoType)) (Wrap a)
  where
    fieldForm rep ty !fn (Wrap x) =
      fieldForm rep ty fn (fieldsToMessage @(Wrapper protoType) (field @"value" x))
    {-# INLINE fieldForm #-}

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

-- | Implements 'FieldForm' for scalar types.
--
-- This implementation detail should be invisible to package clients.
type EncodeScalarField :: Repetition -> ProtoType -> forall {r} . TYPE r -> Constraint
class EncodeScalarField repetition protoType a
  where
    encodeScalarField ::
      Proxy# repetition -> Proxy# protoType -> FieldNumber -> a -> Encode.MessageBuilder

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , Primitive a
         ) =>
         EncodeScalarField ('Singular 'Alternative) protoType a
  where
    encodeScalarField _ _ = encodePrimitive
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , HasDefault a
         , Primitive a
         ) =>
         EncodeScalarField ('Singular 'Implicit) protoType a
  where
    encodeScalarField _ _ !fn x
      | isDefault x = mempty
      | otherwise = encodePrimitive fn x
    {-# INLINE encodeScalarField #-}

-- | Ignores the preference for packed format because there is exactly one element,
-- and therefore packed format would be more verbose.  Conforming parsers must
-- accept both packed and unpacked primitives regardless of packing preference.
instance ( CompatibleScalar (RecoverProtoType a) protoType
         , Primitive a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Identity a)
  where
    encodeScalarField _ _ !fn (Identity x) = encodePrimitive fn x
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , PackedPrimitives a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Forward a)
  where
    encodeScalarField _ _ !fn xs
      | null xs = mempty
      | otherwise = packedPrimitivesF id fn xs
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , PackedPrimitives a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (Reverse a)
  where
    encodeScalarField _ _ !fn (Reverse xs)
      | null xs = mempty
      | otherwise = packedPrimitivesR id fn xs
    {-# INLINE encodeScalarField #-}

instance ( CompatibleScalar (RecoverProtoType a) protoType
         , PackedPrimitives a
         ) =>
         EncodeScalarField ('Repeated 'Packed) protoType (ReverseN a)
  where
    encodeScalarField _ _ !fn (ReverseN count xs)
      | null xs = mempty
      | otherwise = unsafePackedPrimitivesRN id fn count xs
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

    -- | A faster but more specialized variant of 'packedPrimitivesR'.
    --
    -- Unsafe because the predicted number of elements must be at
    -- least the actual number in order to avoid a crash, and because
    -- overapproximation leads to overallocation of the output buffer.
    unsafePackedPrimitivesRN ::
      Foldable f => (b -> a) -> FieldNumber -> Int -> f b -> Encode.MessageBuilder

instance PackedPrimitives Bool
  where
    packedPrimitivesF = Encode.packedBoolsF
    packedPrimitivesR = Encode.packedBoolsR
    unsafePackedPrimitivesRN = Encode.unsafePackedBoolsRN

instance PackedPrimitives Word64
  where
    packedPrimitivesF = Encode.packedVarintsF
    packedPrimitivesR = Encode.packedVarintsR
    unsafePackedPrimitivesRN f !fn _ = Encode.packedVarintsR f fn

instance PackedPrimitives Word32
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Word32 @Word64 . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Word32 @Word64 . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (fromIntegral @Word32 @Word64 . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

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

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (fromIntegral @Int32 @Word64 . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

instance PackedPrimitives Int64
  where
    packedPrimitivesF f = packedPrimitivesF (fromIntegral @Int64 @Word64 . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fromIntegral @Int64 @Word64 . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (fromIntegral @Int64 @Word64 . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

instance PackedPrimitives (Signed Int32)
  where
    packedPrimitivesF f = packedPrimitivesF (zigZagEncode . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (zigZagEncode . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (zigZagEncode . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

instance PackedPrimitives (Signed Int64)
  where
    packedPrimitivesF f = packedPrimitivesF (zigZagEncode . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (zigZagEncode . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (zigZagEncode . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

instance PackedPrimitives (Fixed Word32)
  where
    packedPrimitivesF f = Encode.packedFixed32F (fixed . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = Encode.packedFixed32R (fixed . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = Encode.unsafePackedFixed32RN (fixed . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

instance PackedPrimitives (Fixed Word64)
  where
    packedPrimitivesF f = Encode.packedFixed64F (fixed . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = Encode.packedFixed64R (fixed . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = Encode.unsafePackedFixed64RN (fixed . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

instance PackedPrimitives (Signed (Fixed Int32))
  where
    packedPrimitivesF f = packedPrimitivesF (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fixed32ToUnsigned . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (fixed32ToUnsigned . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

fixed32ToUnsigned :: Signed (Fixed Int32) -> Fixed Word32
fixed32ToUnsigned = Fixed . fromIntegral @Int32 @Word32 . fixed . signed
{-# INLINE fixed32ToUnsigned #-}

instance PackedPrimitives (Signed (Fixed Int64))
  where
    packedPrimitivesF f = packedPrimitivesF (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (fixed64ToUnsigned . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (fixed64ToUnsigned . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

fixed64ToUnsigned :: Signed (Fixed Int64) -> Fixed Word64
fixed64ToUnsigned = Fixed . fromIntegral @Int64 @Word64 . fixed . signed
{-# INLINE fixed64ToUnsigned #-}

instance PackedPrimitives Float
  where
    packedPrimitivesF = Encode.packedFloatsF
    packedPrimitivesR = Encode.packedFloatsR
    unsafePackedPrimitivesRN = Encode.unsafePackedFloatsRN

instance PackedPrimitives Double
  where
    packedPrimitivesF = Encode.packedDoublesF
    packedPrimitivesR = Encode.packedDoublesR
    unsafePackedPrimitivesRN = Encode.unsafePackedDoublesRN

instance ProtoEnum e => PackedPrimitives (Enumerated e)
  where
    packedPrimitivesF f = packedPrimitivesF (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesF #-}

    packedPrimitivesR f = packedPrimitivesR (codeFromEnumerated . f)
    {-# INLINE packedPrimitivesR #-}

    unsafePackedPrimitivesRN f = unsafePackedPrimitivesRN (codeFromEnumerated . f)
    {-# INLINE unsafePackedPrimitivesRN #-}

-- | Pass through those values that are outside the enum range;
-- this is for forward compatibility as enumerations are extended.
codeFromEnumerated :: ProtoEnum e => Enumerated e -> Int32
codeFromEnumerated = either id fromProtoEnum . enumerated
{-# INLINE codeFromEnumerated #-}

instantiatePackableField :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q TH.Exp -> Bool -> TH.Q [TH.Dec]
instantiatePackableField protoType elementType conversion hasWrapper = do
  direct <-
    [d|

      instance FieldForm ('Singular 'Alternative) $protoType $elementType
        where
          fieldForm rep ty !fn x = encodeScalarField rep ty fn ($conversion x)
          {-# INLINE fieldForm #-}

      instance FieldForm ('Singular 'Implicit) $protoType $elementType
        where
          fieldForm rep ty !fn x = encodeScalarField rep ty fn ($conversion x)
          {-# INLINE fieldForm #-}

      instance FieldForm ('Repeated 'Packed) $protoType (Identity $elementType)
        where
          fieldForm rep ty !fn xs = encodeScalarField rep ty fn (fmap $conversion xs)
          {-# INLINE fieldForm #-}

      instance FieldForm ('Repeated 'Packed) $protoType (Forward $elementType)
        where
          fieldForm rep ty !fn xs = encodeScalarField rep ty fn (fmap $conversion xs)
          {-# INLINE fieldForm #-}

      instance FieldForm ('Repeated 'Packed) $protoType (Reverse $elementType)
        where
          fieldForm rep ty !fn xs = encodeScalarField rep ty fn (fmap $conversion xs)
          {-# INLINE fieldForm #-}

      instance FieldForm ('Repeated 'Packed) $protoType (ReverseN $elementType)
        where
          fieldForm rep ty !fn xs = encodeScalarField rep ty fn (fmap $conversion xs)
          {-# INLINE fieldForm #-}

      |]

  wrapped <- if not hasWrapper then pure [] else
    [d|

      instance (omission ~ 'Alternative) =>
               FieldForm ('Singular omission) ('Message (Wrapper $protoType)) $elementType
        where
          fieldForm = coerce
            (fieldForm @('Singular omission) @('Message (Wrapper $protoType)) @(Wrap $elementType))
          {-# INLINE fieldForm #-}

      |]

  pure $ direct ++ wrapped

instantiateStringOrBytesField :: TH.Q TH.Type -> TH.Q TH.Type -> [TH.Q TH.Type] -> TH.Q [TH.Dec]
instantiateStringOrBytesField protoType elementTC specializations = do
  general <-
    [d|

      instance forall a .
               Primitive ($elementTC a) =>
               FieldForm ('Singular 'Alternative) $protoType ($elementTC a)
        where
          fieldForm = encodeScalarField
          {-# INLINE fieldForm #-}

      instance forall a .
               ( HasDefault ($elementTC a)
               , Primitive ($elementTC a)
               ) =>
               FieldForm ('Singular 'Implicit) $protoType ($elementTC a)
        where
          fieldForm = encodeScalarField
          {-# INLINE fieldForm #-}

      instance forall a omission .
               ( omission ~ 'Alternative
               , HasDefault ($elementTC a)
               , Primitive ($elementTC a)
               ) =>
               FieldForm ('Singular omission) ('Message (Wrapper $protoType)) ($elementTC a)
        where
          fieldForm = coerce
            (fieldForm @('Singular omission) @('Message (Wrapper $protoType)) @(Wrap ($elementTC a)))
          {-# INLINE fieldForm #-}

      |]

  special <- for specializations $ \spec ->
    [d|

      instance FieldForm ('Singular 'Alternative) $protoType $spec
        where
          fieldForm = coerce
            (encodeScalarField @('Singular 'Alternative) @($protoType) @($elementTC $spec))
          {-# INLINE fieldForm #-}

      instance FieldForm ('Singular 'Implicit) $protoType $spec
        where
          fieldForm = coerce
            (encodeScalarField @('Singular 'Implicit) @($protoType) @($elementTC $spec))
          {-# INLINE fieldForm #-}

      instance (omission ~ 'Alternative) =>
               FieldForm ('Singular omission) ('Message (Wrapper $protoType)) $spec
        where
          fieldForm = coerce
            (fieldForm @('Singular omission) @('Message (Wrapper $protoType)) @(Wrap $spec))
          {-# INLINE fieldForm #-}

      |]

  pure $ general ++ concat special
