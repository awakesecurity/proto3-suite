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
{-# LANGUAGE PackageImports #-}
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
{-# LANGUAGE ViewPatterns #-}

-- | Implementation details of "Proto3.Suite.Form.Encode" that
-- must be kept separate for the sake of @TemplateHaskell@.
module Proto3.Suite.Form.Encode.Core
  ( MessageEncoder(..)
  , messageEncoderToLazyByteString
  , messageEncoderToByteString
  , etaMessageEncoder
  , FieldsEncoder(..)
  , etaFieldsEncoder
  , Distinct
  , DistinctCheck
  , RepeatedNames
  , RepeatedNames1
  , Omits
  , Strip
  , OccupiedName
  , OccupiedOnly
  , OccupiedOnly1
  , fieldsToMessage
  , Occupy
  , Occupy1
  , NameSublist
  , omitted
  , SFieldNumberI
  , KnownFieldNumber
  , fieldNumberVal
  , Field(..)
  , FieldForm(..)
  , PackedFieldForm(..)
  , Wrap(..)
  , Auto(..)
  , instantiatePackableField
  , instantiateStringOrBytesField
  ) where

import Control.Category (Category(..))
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Traversable (for)
import GHC.Exts (Constraint, Proxy#, TYPE, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage(..), KnownNat, Nat, Symbol, TypeError, natVal')
import "template-haskell" Language.Haskell.TH qualified as TH
import Prelude hiding ((.), id)
import Proto3.Suite.Class (isDefault)
import Proto3.Suite.Form
         (Association, NumberOf, OneOfOf, Packing(..), Cardinality(..),
          CardinalityOf, ProtoType(..), ProtoTypeOf, Wrapper)
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Encode.Repeated (Repeated(..), ToRepeated(..), mapRepeated)
import Proto3.Wire.Types (FieldNumber, fieldNumber)

-- | Annotates 'Encode.MessageBuilder' with the type of protobuf message it encodes.
-- Prefix a tag and length to turn the message into a submessage of a larger message.
newtype MessageEncoder (message :: Type) = UnsafeMessageEncoder
  { untypedMessageEncoder :: Encode.MessageBuilder }

type role MessageEncoder nominal

-- | Serialize a message (or portion thereof) as a lazy 'BL.ByteString'.
messageEncoderToLazyByteString :: forall message . MessageEncoder message -> BL.ByteString
messageEncoderToLazyByteString = Encode.toLazyByteString . untypedMessageEncoder

-- | Serialize a message (or portion thereof) as a strict 'B.ByteString'.
--
-- Functionally equivalent to @'BL.toStrict' . 'messageEncoderToLazyByteString'@,
-- and currently even the performance is the same.
messageEncoderToByteString :: forall message . MessageEncoder message -> B.ByteString
messageEncoderToByteString = BL.toStrict . messageEncoderToLazyByteString

-- | Like 'Encode.etaMessageBuilder' but for 'MessageEncoder'.
etaMessageEncoder :: forall a message . (a -> MessageEncoder message) -> a -> MessageEncoder message
etaMessageEncoder = coerce (Encode.etaMessageBuilder @a)

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
-- See also: 'cacheFieldsEncoder'
newtype FieldsEncoder (message :: Type) (possible :: [Symbol]) (following :: [Symbol]) =
  UnsafeFieldsEncoder { untypedFieldsEncoder :: Encode.MessageBuilder }

type role FieldsEncoder nominal nominal nominal

-- | The 'Category' on encoders of zero or more fields of a particular
-- type of message whose '.' is '<>' on the contained builders.
--
-- Note that '.' preserves the requirements
-- on the type parameters of 'FieldsEncoder'.
instance Category (FieldsEncoder message)
  where
    id = UnsafeFieldsEncoder mempty
    f . g = UnsafeFieldsEncoder (untypedFieldsEncoder f <> untypedFieldsEncoder g)

-- | Like 'Encode.etaMessageBuilder' but for 'FieldsEncoder'.
etaFieldsEncoder ::
  forall a message possible following .
  (a -> FieldsEncoder message possible following) ->
  a -> FieldsEncoder message possible following
etaFieldsEncoder = coerce (Encode.etaMessageBuilder @a)

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

-- | Yields the name of the containing @oneof@ if there is one,
-- and otherwise returns the field name as is.  This type family can
-- help prevent two fields of the same @oneof@ from being emitted.
--
-- This type family is an implementation detail of other type families
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type OccupiedName (message :: Type) (name :: Symbol) = OccupiedName1 name (OneOfOf message name)

-- | This type family is an implementation detail of 'OccupiedName'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family OccupiedName1 (name :: Symbol) (oneof :: Symbol) :: Symbol
  where
    OccupiedName1 name "" = name
    OccupiedName1 _ oneof = oneof

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
      OccupiedOnly1 message name names (CardinalityOf message name)
    OccupiedOnly _ '[] =
      '[]

-- | Helps to implement 'OccupiedOnly'.
--
-- This type family is an implementation detail of 'Distinct'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family OccupiedOnly1 (message :: Type) (name :: Symbol) (names :: [Symbol])
                          (cardinality :: Cardinality) :: [Symbol]
  where
    OccupiedOnly1 message name names 'Implicit =
      name ': OccupiedOnly message names
    OccupiedOnly1 message name names 'Optional =
      OccupiedName message name ': OccupiedOnly message names
    OccupiedOnly1 message name names ('Repeated _) =
      OccupiedOnly message names

-- | Relabels a prefix of fields of a message as an encoding for
-- the message as a whole (though without any tag or length that
-- would make it a submessage).
fieldsToMessage ::
  forall (message :: Type) (names :: [Symbol]) .
  Distinct message names =>
  FieldsEncoder message '[] names ->
  MessageEncoder message
fieldsToMessage = UnsafeMessageEncoder . untypedFieldsEncoder

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
  Occupy1 message name names (CardinalityOf message name)

-- | Helps to implement 'Occupy'.
--
-- This type family is an implementation detail of 'Occupy'
-- that is subject to change, and is exported only to assist
-- in understanding of compilation errors.
type family Occupy1 (message :: Type) (name :: Symbol) (names :: [Symbol])
                    (cardinality :: Cardinality) :: [Symbol]
  where
    Occupy1 message name names 'Implicit = name ': names
    Occupy1 message name names 'Optional = OccupiedName message name ': names
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
-- that appear in the final type parameter of 'FieldsEncoder' but not the
-- previous type parameter, thereby implicitly emitting their default values.
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
  FieldsEncoder message names moreNames
omitted = UnsafeFieldsEncoder mempty

-- | Singleton field number type.
newtype SFieldNumber (fieldNumber :: Nat)
  = UnsafeSFieldNumber { untypedSFieldNumber :: FieldNumber }

-- | Provides the term-level value of the given field number type.
class SFieldNumberI (fieldNumber :: Nat)
  where
    -- | Provides the term-level value in the form of a value of the associated singleton type.
    sFieldNumber :: SFieldNumber fieldNumber

instance KnownNat fieldNumber =>
         SFieldNumberI fieldNumber
  where
    sFieldNumber =
      UnsafeSFieldNumber (fieldNumber (fromInteger (natVal' (proxy# :: Proxy# fieldNumber))))
    {-# INLINABLE sFieldNumber #-}  -- So that it can specialize to particular field number types.

-- | Provides the (term-level) field number of the field
-- having the specified message type and field name.
type KnownFieldNumber message name = SFieldNumberI (NumberOf message name)

-- | Provides the (term-level) field number of the field having
-- the message type and field name specified by the type arguments.
fieldNumberVal :: forall message name . KnownFieldNumber message name => FieldNumber
fieldNumberVal = untypedSFieldNumber (sFieldNumber @(NumberOf message name))
{-# INLINE fieldNumberVal #-}

-- | Provides a way to encode a field with the given name from a given type.
-- That name is interpreted in the context of a given type of message.
--
-- More than one argument type may be supported for any given field;
-- see further discussion in the comments for 'FieldForm', to which
-- this type class delegates after determining the cardinality and
-- protobuf type of the field in question.
type Field :: Symbol -> forall {r} . TYPE r -> Type -> Constraint
class Field name a message
  where
    -- | Encodes the named field from the given value.
    --
    -- If the field is neither @optional@, nor @repeated@, nor part of a @oneof@, then
    -- its default value is represented implicitly--that is, it encodes to zero octets.
    --
    -- In other cases the argument is often a container.  In particular, when using
    -- this method with @optional@ fields and fields within a @oneof@, the argument
    -- type typically involves 'Maybe' or (if the field is always "set") 'Identity'.
    --
    -- Use @TypeApplications@ to specify the field name as the first type parameter.
    --
    -- The second type parameter may also be ambiguous.  For example, the argument
    -- expression may be an integer literal or (when using @OverloadedStrings@)
    -- a string literal.  @TypeApplications@ would resolve the ambiguity, but you
    -- may prefer the 'Auto' wrapper or special-case helper functions such as:
    -- `Proto3.Suite.Form.Encode.message`,
    -- `Proto3.Suite.Form.Encode.associations`.
    --
    -- See also 'fieldForm'.
    field :: forall names . a -> FieldsEncoder message names (Occupy message name names)

instance forall (name :: Symbol)
#if defined(__GLASGOW_HASKELL__) && 904 <= __GLASGOW_HASKELL__
                r (a :: TYPE r)
#else
                (a :: Type)
                  -- Regarding the call to @coerce@, GHC 9.2.8 would say:
                  -- "Cannot use function with levity-polymorphic arguments".
                  -- So we just drop support for unlifted arguments until GHC 9.4.
#endif
                (message :: Type) .
         ( KnownFieldNumber message name
         , FieldForm (CardinalityOf message name) (ProtoTypeOf message name) a
         ) =>
         Field name a message
  where
    field :: forall names . a -> FieldsEncoder message names (Occupy message name names)
    field = coerce
      @(a -> Encode.MessageBuilder)
      @(a -> FieldsEncoder message names (Occupy message name names))
      (fieldForm @(CardinalityOf message name) @(ProtoTypeOf message name) @a
                 proxy# proxy# (fieldNumberVal @message @name))
      -- Implementation Note: Using the newtype constructor would require us
      -- to bind a variable of kind @TYPE r@, which is runtime-polymorphic.
      -- By using a coercion we avoid runtime polymorphism restrictions.
    {-# INLINE field #-}

-- | Implements 'Field' for all fields having the specified cardinality,
-- protobuf type, and type of argument to be encoded within that field.
--
-- Argument Type:
--
-- For any given cardinality and protobuf type there may be multiple
-- instances of this class for different types of argument.  For example,
-- a field of protobuf type @sint64@ may be encoded from any of the Haskell
-- types `Data.Int.Int8`, `Data.Int.Word8`, `Data.Int.Int16`, `Data.Int.Word16`,
-- `Data.Int.Int32`, `Data.Int.Word32`, or `Data.Int.Int64`.  Note that this
-- library does /not/ provide an instance for `Data.Int.Word64` because its
-- values greater than or equal to @2 ^ 63@ cannot be represented by @sint64@.
--
-- As another example, for fields of submessage type @m@ there
-- are type class instances for both @'MessageEncoder' m@
-- and @`Proto3.Suite.Form.Encode.MessageEncoding` m@
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
-- However, this library does provide some general instances:
--
--   * An instance for 'Optional' with 'Maybe' that delegates to
--     the corresponding instance for 'Optional' with 'Identity'.
--
--   * An instance for @'Repeated' 'Unpacked'@ that delegates to
--     the corresponding instance for 'Optional' with 'Identity'.
--
--   * An instance for @'Repeated' 'Packed'@ that delegates to 'PackedFieldForm'.
--
-- Design Note:
--
-- Importantly, the type parameters of this type class do not mention
-- the containing message type, field name, or field number, thus
-- allowing it to be broadly applicable to all containing message types.
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
type FieldForm :: Cardinality -> ProtoType -> forall {r} . TYPE r -> Constraint
class FieldForm cardinality protoType a
  where
    -- | Encodes a message field with the
    -- given number from the given value.
    --
    -- If the field is neither @repeated@, nor @optional@, nor
    -- part of a @oneof@, then its default value is represented
    -- implicitly--that is, it encodes to zero octets.
    --
    -- If you apply this method to a polymorphic expression,
    -- such as a literal value, then you may need to choose
    -- a particular type with @TypeApplications@ or @::@.
    fieldForm :: Proxy# cardinality -> Proxy# protoType -> FieldNumber -> a -> Encode.MessageBuilder

instance FieldForm 'Optional protoType (Identity a) =>
         FieldForm 'Optional protoType (Maybe a)
  where
    fieldForm rep ty !fn = Encode.etaMessageBuilder $
      foldMap @Maybe (fieldForm rep ty fn . Identity)
    {-# INLINE fieldForm #-}

instance ( ToRepeated c e
         , FieldForm 'Optional protoType (Identity e)
         ) =>
         FieldForm ('Repeated 'Unpacked) protoType c
  where
    fieldForm _ ty !fn = Encode.etaMessageBuilder $
      Encode.repeatedMessageBuilder .
      mapRepeated (fieldForm (proxy# :: Proxy# 'Optional) ty fn . Identity)
    {-# INLINE fieldForm #-}

-- | Ignores the preference for packed format when there is exactly one element,
-- and therefore packed format would be more verbose.  (Conforming parsers must
-- accept both packed and unpacked primitives regardless of packing preference.)
instance ( ToRepeated c e
         , PackedFieldForm protoType e
         , FieldForm 'Optional protoType (Identity e)
         ) =>
         FieldForm ('Repeated 'Packed) protoType c
  where
    fieldForm _ ty !fn (toRepeated -> !xs@(ReverseRepeated prediction reversed)) =
        case prediction of
          Just count
            | 2 <= count -> packedFieldForm ty fn xs  -- multiple packed elements
            | otherwise -> fieldForm (proxy# :: Proxy# ('Repeated 'Unpacked)) ty fn xs  -- 0 or 1
          Nothing -> case foldr singletonOp Empty reversed of
            Empty -> mempty  -- 0 elements can be expressed implicitly
            Singleton x -> fieldForm (proxy# :: Proxy# 'Optional) ty fn (Identity x) -- unpacked
            Multiple -> packedFieldForm ty fn xs  -- multiple packed elements
      where
        singletonOp :: a -> Singleton a -> Singleton a
        singletonOp x Empty = Singleton x
        singletonOp _ _ = Multiple
    {-# INLINE fieldForm #-}

data Singleton a = Empty | Singleton a | Multiple

instance FieldForm 'Optional ('Message inner) (Identity (MessageEncoder inner))
  where
    fieldForm _ _ !fn (Identity e) = Encode.embedded fn (untypedMessageEncoder e)
    {-# INLINE fieldForm #-}

-- | This instance is rather artificial because maps are automatically
-- repeated and unpacked, with no option to specify a single key-value pair
-- as an @optional@ field or a field of a @oneof@.  Hence the code generator
-- should never directly make use of this instance, but it will do so
-- indirectly via the general instance for repeated unpacked fields,
-- which will then delegate to this instance.
instance FieldForm 'Optional ('Map key value) (Identity (MessageEncoder (Association key value)))
  where
    fieldForm _ _ !fn (Identity a) = Encode.embedded fn (untypedMessageEncoder a)
    {-# INLINE fieldForm #-}

-- | 'FieldForm' delegates to this type class when encoding
-- packed repeated fields containing two or more elements.
type PackedFieldForm :: ProtoType -> forall {r} . TYPE r -> Constraint
class PackedFieldForm protoType a
  where
    -- | 'fieldForm' delegates to this method when encoding
    -- packed repeated fields containing two or more elements.
    packedFieldForm :: Proxy# protoType -> FieldNumber -> Repeated a -> Encode.MessageBuilder

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

instance FieldForm 'Implicit protoType a =>
         FieldForm 'Optional ('Message (Wrapper protoType)) (Identity (Wrap a))
  where
    fieldForm _ ty !fn (Identity (Wrap x)) =
      fieldForm (proxy# :: Proxy# 'Optional) ty fn
                (Identity (fieldsToMessage @(Wrapper protoType) (field @"value" x)))
    {-# INLINE fieldForm #-}

-- | Asks that its type parameter be chosen to be the most efficient Haskell type
-- that expresses the full range of possible values of the relevant protobuf field.
--
-- This choice can be used to resolve ambiguity around polymorphic
-- literal values, or when performing a polymorphic conversion
-- from a type that is not directly encodable, such as 'Int'.
newtype Auto (a :: Type) = Auto { unauto :: a }
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving newtype (Bounded, Enum, Eq, Fractional, Integral, Ord, Num, Read, Real, Show)

instantiatePackableField ::
  -- | Protobuf type of kind 'ProtoType'.
  TH.Q TH.Type ->
  -- | 'fieldForm' argument type.
  TH.Q TH.Type ->
  -- | The encoder for non-repeated or unpacked fields
  -- of the specified protobuf type and argument type.
  TH.Q TH.Exp ->
  -- | The encoder for packed fields of the specified protobuf type and argument type.
  TH.Q TH.Exp ->
  -- | For every promotion to this type of field:
  --  1. The argument type to before promotion.
  --  2. The conversion from that type to an argument type.
  --  3. The compatible protobuf type whose encoder we should use;
  --     it must support the post-conversion type of the argument.
  --
  -- NOTE: Do not convert when some source values have no semantically-equivalent
  -- value to which to convert.  Specifically, do not convert from signed integers
  -- to unsigned integers (because negative values become positive ones), and only
  -- convert from unsigned to signed when widening (because otherwise some positive
  -- values become will negative ones).
  --
  -- These are /not/ selected by 'Auto'.
  [(TH.Q TH.Type, TH.Q TH.Exp, TH.Q TH.Type)] ->
  -- | Is there a standard wrapper type?
  Bool ->
  TH.Q [TH.Dec]
instantiatePackableField protoType elementType encoder packedEncoder promotions hasWrapper = do
  direct <-
    [d|

      instance FieldForm 'Optional $protoType (Identity $elementType)
        where
          fieldForm _ _ !fn (Identity x) = $encoder fn x
          {-# INLINE fieldForm #-}

      instance FieldForm 'Implicit $protoType $elementType
        where
          fieldForm _ ty !fn x
            | isDefault x = mempty
            | otherwise = fieldForm (proxy# :: Proxy# 'Optional) ty fn (Identity x)
          {-# INLINE fieldForm #-}

      instance (a ~ $elementType) =>
               FieldForm 'Optional $protoType (Identity (Auto a))
        where
          fieldForm = coerce (fieldForm @'Optional @($protoType) @(Identity a))
          {-# INLINE fieldForm #-}

      instance (a ~ $elementType) =>
               FieldForm 'Implicit $protoType (Auto a)
        where
          fieldForm = coerce (fieldForm @'Implicit @($protoType) @a)
          {-# INLINE fieldForm #-}

      instance PackedFieldForm $protoType $elementType
        where
          packedFieldForm _ = $packedEncoder
          {-# INLINE packedFieldForm #-}

      instance (a ~ $elementType) =>
               PackedFieldForm $protoType (Auto a)
        where
          packedFieldForm = coerce (packedFieldForm @($protoType) @a)
          {-# INLINE packedFieldForm #-}

      |]

  promoted <- for promotions $ \(supportedType, conversion, compatibleProtoType) ->
    [d|

      instance FieldForm 'Optional $protoType (Identity $supportedType)
        where
          fieldForm rep _ !fn (Identity x) =
            fieldForm rep (proxy# :: Proxy# $compatibleProtoType) fn (Identity ($conversion x))
          {-# INLINE fieldForm #-}

      instance FieldForm 'Implicit $protoType $supportedType
        where
          fieldForm rep _ !fn x =
            fieldForm rep (proxy# :: Proxy# $compatibleProtoType) fn ($conversion x)
          {-# INLINE fieldForm #-}

      instance PackedFieldForm $protoType $supportedType
        where
          packedFieldForm _ !fn xs =
            packedFieldForm (proxy# :: Proxy# $compatibleProtoType) fn (fmap $conversion xs)
          {-# INLINE packedFieldForm #-}

      |]

  wrapped <- if not hasWrapper then pure [] else
    [d|

      instance FieldForm 'Optional ('Message (Wrapper $protoType)) (Identity $elementType)
        where
          fieldForm = coerce
            (fieldForm @'Optional @('Message (Wrapper $protoType)) @(Identity (Wrap $elementType)))
          {-# INLINE fieldForm #-}

      instance (a ~ $elementType) =>
               FieldForm 'Optional ('Message (Wrapper $protoType)) (Identity (Auto a))
        where
          fieldForm = coerce
            (fieldForm @'Optional @('Message (Wrapper $protoType)) @(Identity (Wrap a)))
          {-# INLINE fieldForm #-}

      |]

  wrappedPromoted <- if not hasWrapper then pure [] else
    for promotions $ \(supportedType, _, _) -> do
      [d|

        instance FieldForm 'Optional ('Message (Wrapper $protoType)) (Identity $supportedType)
          where
            fieldForm = coerce
              (fieldForm @'Optional @('Message (Wrapper $protoType)) @(Identity (Wrap $supportedType)))
            {-# INLINE fieldForm #-}

        |]

  pure $ direct ++ concat promoted ++ wrapped ++ concat wrappedPromoted

instantiateStringOrBytesField ::
  -- | Protobuf type of kind 'ProtoType'.
  TH.Q TH.Type ->
  -- | 'fieldForm' argument type.
  TH.Q TH.Type ->
  -- | The encoder for fields of the specified protobuf type and argument type.
  TH.Q TH.Exp ->
  -- | Additional argument types and their associated encoders.
  -- These are /not/ selected by 'Auto'.
  [(TH.Q TH.Type, TH.Q TH.Exp)] ->
  TH.Q [TH.Dec]
instantiateStringOrBytesField protoType argumentType encoder additional = do
  preferred <- [d|

    instance FieldForm 'Optional $protoType (Identity $argumentType)
      where
        fieldForm _ _ = coerce
          @(FieldNumber -> $argumentType -> Encode.MessageBuilder)
          @(FieldNumber -> Identity $argumentType -> Encode.MessageBuilder)
          $encoder
        {-# INLINE fieldForm #-}

    instance FieldForm 'Implicit $protoType $argumentType
      where
        fieldForm _ ty !fn x
          | isDefault x = mempty
          | otherwise = fieldForm (proxy# :: Proxy# 'Optional) ty fn (Identity x)
        {-# INLINE fieldForm #-}

    instance (a ~ $argumentType) =>
             FieldForm 'Optional $protoType (Identity (Auto a))
      where
        fieldForm = coerce (fieldForm @'Optional @($protoType) @(Identity a))
        {-# INLINE fieldForm #-}

    instance (a ~ $argumentType) =>
             FieldForm 'Implicit $protoType (Auto a)
      where
        fieldForm = coerce (fieldForm @'Implicit @($protoType) @a)
        {-# INLINE fieldForm #-}

    instance FieldForm 'Optional ('Message (Wrapper $protoType)) (Identity $argumentType)
      where
        fieldForm = coerce
          (fieldForm @'Optional @('Message (Wrapper $protoType)) @(Identity (Wrap $argumentType)))
        {-# INLINE fieldForm #-}

    instance (a ~ $argumentType) =>
             FieldForm 'Optional ('Message (Wrapper $protoType)) (Identity (Auto a))
      where
        fieldForm = coerce
          (fieldForm @'Optional @('Message (Wrapper $protoType)) @(Identity (Wrap a)))
        {-# INLINE fieldForm #-}

    |]

  supported <- for additional $ \(supportedType, additionalEncoder) -> [d|

    instance FieldForm 'Optional $protoType (Identity $supportedType)
      where
        fieldForm _ _ = coerce
          @(FieldNumber -> $supportedType -> Encode.MessageBuilder)
          @(FieldNumber -> Identity $supportedType -> Encode.MessageBuilder)
          $additionalEncoder
        {-# INLINE fieldForm #-}

    instance FieldForm 'Implicit $protoType $supportedType
      where
        fieldForm _ ty !fn x
          | isDefault x = mempty
          | otherwise = fieldForm (proxy# :: Proxy# 'Optional) ty fn (Identity x)
        {-# INLINE fieldForm #-}

    instance FieldForm 'Optional ('Message (Wrapper $protoType)) (Identity $supportedType)
      where
        fieldForm = coerce
          (fieldForm @'Optional @('Message (Wrapper $protoType)) @(Identity (Wrap $supportedType)))
        {-# INLINE fieldForm #-}

    |]

  pure $ preferred ++ concat supported
