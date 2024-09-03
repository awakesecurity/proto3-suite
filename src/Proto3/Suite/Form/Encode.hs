{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Encodes to protobuf directly from application-specific source data without
-- an intermediate value of a type generated from protobuf message definitions.
--
-- /WARNING/: This module is experimental and breaking changes may occur much more
-- frequently than in the other modules of this package, perhaps even in patch releases.
module Proto3.Suite.Form.Encode
  ( Encoding(..)
  , toLazyByteString
  , Prefix(..)
  , Distinct
  , messageFromFields
  , Occupy
  , omitted
  , KnownFieldNumber
  , Field
  , field
  , FieldImpl
  , Forward(..)
  , Reverse(..)
  , Vector(..)
  , foldPrefixF
  , foldPrefixR
  , foldPrefixV
  , message
  , associations
  , encodeField
  ) where

import Control.Category (Category(..))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Word (Word32, Word64)
import GHC.TypeLits (Symbol)
import Prelude hiding (String, (.), id)
import Proto3.Suite.Class (MessageField(..), Named)
import Proto3.Suite.Form.Encode.Core
import Proto3.Suite.Form
         (Association, MessageFieldType, Repetition(..), RepetitionOf, ProtoType(..), ProtoTypeOf)
import Proto3.Suite.Types (Enumerated(..), Fixed(..), Signed(..))
import Proto3.Suite.Types qualified
import Proto3.Wire.Class (ProtoEnum(..))

$(instantiatePackableField [t| 'Int32 |] [t| Int32 |] [| id |])
$(instantiatePackableField [t| 'Int64 |] [t| Int64 |] [| id |])
$(instantiatePackableField [t| 'SInt32 |] [t| Int32 |] [| Signed |])
$(instantiatePackableField [t| 'SInt64 |] [t| Int64 |] [| Signed |])
$(instantiatePackableField [t| 'UInt32 |] [t| Word32 |] [| id |])
$(instantiatePackableField [t| 'UInt64 |] [t| Word64 |] [| id |])
$(instantiatePackableField [t| 'Fixed32 |] [t| Word32 |] [| Fixed |])
$(instantiatePackableField [t| 'Fixed64 |] [t| Word64 |] [| Fixed |])
$(instantiatePackableField [t| 'SFixed32 |] [t| Int32 |] [| Signed . Fixed |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Int64 |] [| Signed . Fixed |])
$(instantiatePackableField [t| 'Bool |] [t| Bool |] [| id |])
$(instantiatePackableField [t| 'Float |] [t| Float |] [| id |])
$(instantiatePackableField [t| 'Double |] [t| Double |] [| id |])

$(instantiateStringOrBytesField [t| 'String |] [t| Proto3.Suite.Types.String |])
$(instantiateStringOrBytesField [t| 'Bytes |] [t| Proto3.Suite.Types.Bytes |])

instance ( RepetitionOf message name ~ 'Singular
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Enumerated e) message 'Singular ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'OneOf
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Enumerated e) message 'OneOf ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Identity (Enumerated e)) message 'Unpacked ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Forward (Enumerated e)) message 'Unpacked ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Reverse (Enumerated e)) message 'Unpacked ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Vector (Enumerated e)) message 'Unpacked ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

-- | NOTE: This instance produces the same encoding as for @[packed=false]@,
-- because packing does not improve efficiency for length-one sequences, and
-- because the protobuf specification requires parsers to handle that format.
instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Identity (Enumerated e)) message 'Packed ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Forward (Enumerated e)) message 'Packed ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Reverse (Enumerated e)) message 'Packed ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Vector (Enumerated e)) message 'Packed ('Enumeration e)
  where
    field = primitiveField @name
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Singular
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name e message 'Singular ('Enumeration e)
  where
    field = field @name . Enumerated . Right
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'OneOf
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name e message 'OneOf ('Enumeration e)
  where
    field = field @name . Enumerated . Right
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Identity e) message 'Unpacked ('Enumeration e)
  where
    field = field @name . fmap @Identity (Enumerated . Right)
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Forward e) message 'Unpacked ('Enumeration e)
  where
    field = field @name . fmap @Forward (Enumerated . Right)
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Reverse e) message 'Unpacked ('Enumeration e)
  where
    field = field @name . fmap @Reverse (Enumerated . Right)
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Unpacked
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Vector e) message 'Unpacked ('Enumeration e)
  where
    field = field @name . fmap @Vector (Enumerated . Right)
    {-# INLINE field #-}

-- | NOTE: This instance produces the same encoding as for @[packed=false]@,
-- because packing does not improve efficiency for length-one sequences, and
-- because the protobuf specification requires parsers to handle that format.
instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Identity e) message 'Packed ('Enumeration e)
  where
    field = field @name . fmap @Identity (Enumerated . Right)
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Forward e) message 'Packed ('Enumeration e)
  where
    field = field @name . fmap @Forward (Enumerated . Right)
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Reverse e) message 'Packed ('Enumeration e)
  where
    field = field @name . fmap @Reverse (Enumerated . Right)
    {-# INLINE field #-}

instance ( RepetitionOf message name ~ 'Packed
         , ProtoTypeOf message name ~ 'Enumeration e
         , KnownFieldNumber message name
         , Named e
         , ProtoEnum e
         ) =>
         FieldImpl name (Vector e) message 'Packed ('Enumeration e)
  where
    field = field @name . fmap @Vector (Enumerated . Right)
    {-# INLINE field #-}

-- | Specializes the argument type of 'field' to the encoding of a submessage type,
-- which can help to avoid ambiguity when the argument expression is polymorphic.
message ::
  forall (name :: Symbol) (inner :: Type) (outer :: Type) (names :: [Symbol]) .
  ( ProtoTypeOf outer name ~ 'Message inner
  , Field name (Encoding inner) outer
  , KnownFieldNumber outer name
  ) =>
  Encoding inner ->
  Prefix outer names (Occupy outer name names)
message = field @name @(Encoding inner)

-- | Specializes the argument type of 'field' to be a sequence of key-value pair encodings,
-- which can help to avoid ambiguity when the argument expression is polymorphic.
associations ::
  forall (name :: Symbol) (t :: Type -> Type) (key :: ProtoType) (value :: ProtoType)
         (message :: Type) (names :: [Symbol]) .
  ( ProtoTypeOf message name ~ 'Map key value
  , RepetitionOf message name ~ 'Unpacked
  , Field name (t (Encoding (Association key value))) message
  , KnownFieldNumber message name
  ) =>
  t (Encoding (Association key value)) ->
  Prefix message names names
associations = field @name @(t (Encoding (Association key value)))

-- | Delegates field encoding to 'MessageField' at the cost
-- of requiring the relevant intermediate representation.
--
-- Repeated fields must be supplied as
-- the appropriately-typed collection.
--
-- Use @TypeApplications@ to specify the field name.
-- You may also need to specify the argument type, for
-- example when passing an integer or string literal.
encodeField ::
  forall (name :: Symbol) (a :: Type) (message :: Type) (names :: [Symbol]) .
  ( KnownFieldNumber message name
  , MessageFieldType (RepetitionOf message name) (ProtoTypeOf message name) a
  , MessageField a
  ) =>
  a ->
  Prefix message names (Occupy message name names)
encodeField = UnsafePrefix . encodeMessageField (fieldNumber @message @name)
{-# INLINE encodeField #-}
