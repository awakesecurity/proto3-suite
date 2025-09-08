{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Encodes to protobuf directly from application-specific source data without
-- an intermediate value of a type generated from protobuf message definitions.
--
-- Importantly, code generation does not make use of this module,
-- instead using only "Proto3.Suite.Form".  Therefore one can replace
-- this module with another that makes use of the same generated code.
--
-- /WARNING/: This module is experimental and breaking changes may occur much more
-- frequently than in the other modules of this package, perhaps even in patch releases.
module Proto3.Suite.Form.Encode
  ( MessageEncoder(..)
  , messageEncoderToLazyByteString
  , messageEncoderToByteString
  , etaMessageEncoder
  , unsafeByteStringToMessageEncoder
  , MessageEncoding
  , messageCache
  , cacheMessageEncoding
  , cachedMessageEncoding
  , messageEncodingToByteString
  , unsafeByteStringToMessageEncoding
  , FieldsEncoder(..)
  , etaFieldsEncoder
  , FieldsEncoding
  , cacheFieldsEncoding
  , cachedFieldsEncoding
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
  , SFieldNumberI
  , KnownFieldNumber
  , Field(..)
  , FieldForm(..)
  , Wrap(..)
  , Auto(..)
  , foldFieldsEncoders
  , message
  , associations
  , Reflection(..)
  , messageReflection
  ) where

import Control.Applicative ((<|>))
import Control.Category (Category(..))
import Control.DeepSeq (NFData)
import Data.Aeson qualified as Aeson (FromJSON(..), ToJSON(..))
import Data.Coerce (coerce)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Type)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Short qualified as TS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Prelude hiding (String, (.), id)
import Proto3.Suite.Form.Encode.Core
import Proto3.Suite.Class
         (Message, MessageField, encodeMessage, encodeMessageField, fromByteString)
import Proto3.Suite.Form
         (Association, MessageFieldType, Omission(..), Packing(..),
          Cardinality(..), CardinalityOf, ProtoType(..), ProtoTypeOf)
import Proto3.Suite.JSONPB.Class qualified as JSONPB
         (FromJSONPB(..), ToJSONPB(..), toAesonEncoding, toAesonValue)
import Proto3.Suite.Types (Enumerated, codeFromEnumerated)
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Encode.Repeated (ToRepeated, mapRepeated)
import Proto3.Wire.Reverse qualified as RB
import Proto3.Wire.Types (fieldNumber)

-- | The unsafe but fast inverse of 'messageEncoderToByteString'.
unsafeByteStringToMessageEncoder :: B.ByteString -> MessageEncoder message
unsafeByteStringToMessageEncoder = UnsafeMessageEncoder . Encode.unsafeFromByteString

-- | The octet sequence that would be emitted by some
-- 'MessageEncoder' having the same type parameter.
--
-- (This type is not a 'Semigroup' because combining encodings that both
-- have the same non-repeated field is arguably incorrect, even though the
-- standard asks parsers to to try to work around such improper repetition.)
--
-- See also: 'cacheMessageEncoding'
newtype MessageEncoding (message :: Type) = UnsafeMessageEncoding B.ByteString
  deriving stock (Generic)
  deriving newtype (NFData)

type role MessageEncoding nominal

instance (omission ~ 'Alternative) =>
         FieldForm ('Unitary omission) ('Message inner) (MessageEncoding inner)
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
         FieldForm ('Unitary omission) ('Map key value) (MessageEncoding (Association key value))
  where
    fieldForm rep ty !fn e = fieldForm rep ty fn (cachedMessageEncoding e)
    {-# INLINE fieldForm #-}

-- | Precomputes the octet sequence that would be written by the given 'MessageEncoder'.
-- Do this only if you expect to reuse that specific octet sequence repeatedly.
--
-- @'cachedMessageEncoding' . 'cacheMessageEncoding'@ is functionally equivalent
-- to 'id' but has different performance characteristics.
--
-- See also: 'cacheFieldsEncoding'.
cacheMessageEncoding :: MessageEncoder message -> MessageEncoding message
cacheMessageEncoding = UnsafeMessageEncoding . messageEncoderToByteString

-- | Encodes a precomputed 'MessageEncoder' by copying octets from a memory buffer.
-- See 'cacheMessageEncoding'.
--
-- See also: 'cachedFieldsEncoding'
cachedMessageEncoding :: MessageEncoding message -> MessageEncoder message
cachedMessageEncoding = unsafeByteStringToMessageEncoder . messageEncodingToByteString

-- | Strips type information from the message encoding, leaving only its octets.
messageEncodingToByteString :: MessageEncoding message -> B.ByteString
messageEncodingToByteString (UnsafeMessageEncoding octets) = octets

-- | Unsafe because the caller must ensure that the given octets
-- are in the correct format for a message of the specified type.
unsafeByteStringToMessageEncoding :: B.ByteString -> MessageEncoding message
unsafeByteStringToMessageEncoding = UnsafeMessageEncoding

-- | The octet sequence that would be prefixed by some
-- 'FieldsEncoder' having the same type parameters.
newtype FieldsEncoding (message :: Type) (possible :: [Symbol]) (following :: [Symbol]) =
  UnsafeFieldsEncoding { untypedFieldsEncoding :: B.ByteString }

type role FieldsEncoding nominal nominal nominal

-- | Precomputes the octet sequence that would be written by the given 'FieldsEncoder'.
-- Do this only if you expect to reuse that specific octet sequence repeatedly.
--
-- @'cachedFieldsEncoding' . 'cacheFieldsEncoding'@ is functionally equivalent
-- to 'id' but has different performance characteristics.
--
-- See also: 'cacheMessageEncoding'
cacheFieldsEncoding ::
  FieldsEncoder message possible following -> FieldsEncoding message possible following
cacheFieldsEncoding =
  UnsafeFieldsEncoding . BL.toStrict . Encode.toLazyByteString . untypedFieldsEncoder

-- | Encodes a precomputed 'FieldsEncoder' by copying octets from a memory buffer.
-- See 'cacheFieldsEncoding'.
--
-- See also: 'cachedMessageEncoding'.
cachedFieldsEncoding ::
  FieldsEncoding message possible following -> FieldsEncoder message possible following
cachedFieldsEncoding = UnsafeFieldsEncoder . Encode.unsafeFromByteString . untypedFieldsEncoding
{-# INLINE cachedFieldsEncoding #-}

$(instantiatePackableField
  [t| 'UInt32 |] [t| Word32 |] [| Encode.uint32 |] [| Encode.packedUInt32R |]
  [ ([t| Word16 |], [| fromIntegral @Word16 @Word32 |], [t| 'UInt32 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Word32 |], [t| 'UInt32 |])
  ] True)

$(instantiatePackableField
  [t| 'UInt64 |] [t| Word64 |] [| Encode.uint64 |] [| Encode.packedUInt64R |]
  [ ([t| Word32 |], [|                           id |], [t| 'UInt32 |])
  , ([t| Word16 |], [| fromIntegral @Word16 @Word32 |], [t| 'UInt32 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Word32 |], [t| 'UInt32 |])
  ] True)

$(instantiatePackableField
  [t| 'Int32 |] [t| Int32 |] [| Encode.int32 |] [| Encode.packedInt32R |]
  [ ([t|  Int16 |], [| fromIntegral   @Int16 @Int32 |], [t|  'Int32 |])
  , ([t|   Int8 |], [| fromIntegral    @Int8 @Int32 |], [t|  'Int32 |])
  -- Because the encoding for @int32@ is just a conversion to the 64-bit unsigned
  -- integer that is equal to the original value modulo @2^64@ followed by @uint64@
  -- encoding, the encoding of unsigned values can be accomplished with the @uint32@
  -- encoder, which generates less code because it need not support values @>= 2^32@.
  , ([t| Word16 |], [|                           id |], [t| 'UInt32 |])
  , ([t|  Word8 |], [|                           id |], [t| 'UInt32 |])
  ] True)

$(instantiatePackableField
  [t| 'Int64 |] [t| Int64 |] [| Encode.int64 |] [| Encode.packedInt64R |]
  [ ([t|  Int32 |], [| fromIntegral  @Int32 @Int64 |], [t| 'Int64 |])
  , ([t|  Int16 |], [| fromIntegral  @Int16 @Int64 |], [t| 'Int64 |])
  , ([t|   Int8 |], [| fromIntegral   @Int8 @Int64 |], [t| 'Int64 |])
  -- Because the encoding for @int32@ is just a conversion to the 64-bit unsigned
  -- integer that is equal to the original value modulo @2^64@ followed by @uint64@
  -- encoding, the encoding of unsigned values can be accomplished with the @uint32@
  -- encoder, which generates less code because it need not support values @>= 2^32@.
  , ([t| Word32 |], [|                           id |], [t| 'UInt32 |])
  , ([t| Word16 |], [|                           id |], [t| 'UInt32 |])
  , ([t|  Word8 |], [|                           id |], [t| 'UInt32 |])
  ] True)

$(instantiatePackableField
  [t| 'SInt32 |] [t| Int32 |] [| Encode.sint32 |] [| Encode.packedSInt32R |]
  [ ([t|  Int16 |], [| fromIntegral  @Int16 @Int32 |], [t| 'SInt32 |])
  , ([t|   Int8 |], [| fromIntegral   @Int8 @Int32 |], [t| 'SInt32 |])
  , ([t| Word16 |], [| fromIntegral @Word16 @Int32 |], [t| 'SInt32 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Int32 |], [t| 'SInt32 |])
  ] False)

$(instantiatePackableField
  [t| 'SInt64 |] [t| Int64 |] [| Encode.sint64 |] [| Encode.packedSInt64R |]
  [ ([t|  Int32 |], [|                          id |], [t| 'SInt32 |])
  , ([t|  Int16 |], [|                          id |], [t| 'SInt32 |])
  , ([t|   Int8 |], [|                          id |], [t| 'SInt32 |])
  , ([t| Word32 |], [| fromIntegral @Word32 @Int64 |], [t| 'SInt64 |])
  , ([t| Word16 |], [|                          id |], [t| 'SInt32 |])
  , ([t|  Word8 |], [|                          id |], [t| 'SInt32 |])
  ] False)

$(instantiatePackableField
  [t| 'Fixed32 |] [t| Word32 |] [| Encode.fixed32 |] [| Encode.packedFixed32R |]
  [ ([t| Word16 |], [| fromIntegral @Word16 @Word32 |], [t| 'Fixed32 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Word32 |], [t| 'Fixed32 |])
  ] False)

$(instantiatePackableField
  [t| 'Fixed64 |] [t| Word64 |] [| Encode.fixed64 |] [| Encode.packedFixed64R |]
  [ ([t| Word32 |], [| fromIntegral @Word32 @Word64 |], [t| 'Fixed64 |])
  , ([t| Word16 |], [| fromIntegral @Word16 @Word64 |], [t| 'Fixed64 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Word64 |], [t| 'Fixed64 |])
  ] False)

$(instantiatePackableField
  [t| 'SFixed32 |] [t| Int32 |] [| Encode.sfixed32 |] [| Encode.packedSFixed32R |]
  [ ([t|  Int16 |], [| fromIntegral  @Int16 @Int32 |], [t| 'SFixed32 |])
  , ([t|   Int8 |], [| fromIntegral   @Int8 @Int32 |], [t| 'SFixed32 |])
  , ([t| Word16 |], [| fromIntegral @Word16 @Int32 |], [t| 'SFixed32 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Int32 |], [t| 'SFixed32 |])
  ] False)

$(instantiatePackableField
  [t| 'SFixed64 |] [t| Int64 |] [| Encode.sfixed64 |] [| Encode.packedSFixed64R |]
  [ ([t|  Int32 |], [| fromIntegral  @Int32 @Int64 |], [t| 'SFixed64 |])
  , ([t|  Int16 |], [| fromIntegral  @Int16 @Int64 |], [t| 'SFixed64 |])
  , ([t|   Int8 |], [| fromIntegral   @Int8 @Int64 |], [t| 'SFixed64 |])
  , ([t| Word32 |], [| fromIntegral @Word32 @Int64 |], [t| 'SFixed64 |])
  , ([t| Word16 |], [| fromIntegral @Word16 @Int64 |], [t| 'SFixed64 |])
  , ([t|  Word8 |], [| fromIntegral  @Word8 @Int64 |], [t| 'SFixed64 |])
  ] False)

$(instantiatePackableField
  [t| 'Bool |] [t| Bool |] [| Encode.bool |] [| Encode.packedBoolsR |]
  [] True)

$(instantiatePackableField
  [t| 'Float |] [t| Float |] [| Encode.float |] [| Encode.packedFloatsR |]
  [] True)

$(instantiatePackableField
  [t| 'Double |] [t| Double |] [| Encode.double |] [| Encode.packedDoublesR |]
  [ ([t| Float |], [| realToFrac @Float @Double |], [t| 'Double |])
  ] True)

$(instantiateStringOrBytesField
   [t| 'String |] [t| TS.ShortText |] [| Encode.shortText |]
   [ ([t| T.Text |], [| \(!fn) x -> Encode.text fn (TL.fromStrict x) |])
   , ([t| TL.Text |], [| Encode.text |])
   ])

$(instantiateStringOrBytesField
   [t| 'Bytes |] [t| BS.ShortByteString |] [| Encode.shortByteString |]
   [ ([t| B.ByteString |], [| Encode.byteString |])
   , ([t| BL.ByteString |], [| Encode.lazyByteString |])
   ])

instance ( ProtoEnum e
         , FieldForm ('Unitary omission) 'Int32 Int32
         ) =>
         FieldForm ('Unitary omission) ('Enumeration e) e
  where
    fieldForm rep _ !fn x = fieldForm rep (proxy# :: Proxy# 'Int32) fn (fromProtoEnum x)
    {-# INLINE fieldForm #-}

instance ( ProtoEnum e
         , FieldForm ('Unitary omission) 'Int32 Int32
         ) =>
         FieldForm ('Unitary omission) ('Enumeration e) (Enumerated e)
  where
    fieldForm rep _ !fn x = fieldForm rep (proxy# :: Proxy# 'Int32) fn (codeFromEnumerated x)
    {-# INLINE fieldForm #-}

instance ProtoEnum e =>
         PackedFieldForm ('Enumeration e) e
  where
    packedFieldForm _ !fn xs =
      packedFieldForm (proxy# :: Proxy# 'Int32) fn (fmap fromProtoEnum xs)
    {-# INLINE packedFieldForm #-}

instance ProtoEnum e =>
         PackedFieldForm ('Enumeration e) (Enumerated e)
  where
    packedFieldForm _ !fn xs =
      packedFieldForm (proxy# :: Proxy# 'Int32) fn (fmap codeFromEnumerated xs)
    {-# INLINE packedFieldForm #-}

instance FieldForm ('Unitary 'Alternative) 'Bytes RB.BuildR
  where
    fieldForm _ _ = Encode.bytes
    {-# INLINE fieldForm #-}

instance FieldForm ('Unitary 'Implicit) 'Bytes RB.BuildR
  where
    fieldForm _ _ = Encode.bytesIfNonempty
    {-# INLINE fieldForm #-}

-- | Combines 'FieldsEncoder' builders for zero or more repeated fields.
foldFieldsEncoders ::
  forall c message names .
  ToRepeated c (FieldsEncoder message names names) =>
  c ->
  FieldsEncoder message names names
foldFieldsEncoders prefixes =
  UnsafeFieldsEncoder (Encode.repeatedMessageBuilder (mapRepeated untypedFieldsEncoder prefixes))
{-# INLINE foldFieldsEncoders #-}

-- | Specializes the argument type of 'field' to the encoding of a submessage type,
-- which can help to avoid ambiguity when the argument expression is polymorphic.
message ::
  forall (name :: Symbol) (inner :: Type) (outer :: Type) (names :: [Symbol]) .
  ( ProtoTypeOf outer name ~ 'Message inner
  , Field name (MessageEncoder inner) outer
  , KnownFieldNumber outer name
  ) =>
  MessageEncoder inner ->
  FieldsEncoder outer names (Occupy outer name names)
message = field @name @(MessageEncoder inner)

-- | Specializes the argument type of 'field' to be a sequence of key-value pair encodings,
-- which can help to avoid ambiguity when the argument expression is polymorphic.
associations ::
  forall (name :: Symbol) (t :: Type -> Type) (key :: ProtoType) (value :: ProtoType)
         (message :: Type) (names :: [Symbol]) .
  ( ProtoTypeOf message name ~ 'Map key value
  , CardinalityOf message name ~ 'Repeated 'Unpacked
  , Field name (t (MessageEncoder (Association key value))) message
  , KnownFieldNumber message name
  ) =>
  t (MessageEncoder (Association key value)) ->
  FieldsEncoder message names names
associations = field @name @(t (MessageEncoder (Association key value)))

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
-- For this @newtype@, 'Field' delegates to `Proto3.Suite.Class.MessageField`
-- and has its performance characteristics.  The creation of temporary
-- reflections of protobuf messages may decrease efficiency
-- in some cases.  However, you may find this @newtype@ useful
-- where a mix of techniques is needed, either for compatibility
-- or during a gradual transition to use of 'Field'.
--
-- Note that for optional submessages you must use `Proto3.Suite.Types.Nested`,
-- and for repeated submessages you must use `Proto3.Suite.Types.NestedVec`.
-- (For submessages within a @oneof@ you can use the reflection type directly.)
--
-- To encode a top-level message instead of a field, use 'messageReflection'.
newtype Reflection a = Reflection a

instance ( MessageFieldType cardinality protoType a
         , MessageField a
         ) =>
         FieldForm cardinality protoType (Reflection a)
  where
    fieldForm _ _ = coerce (encodeMessageField @a)
    {-# INLINE fieldForm #-}

-- | Creates a message encoder by means of type class `Proto3.Suite.Class.Message`.
--
-- To encode a field instead of a top-level message, use 'Reflection'.
messageReflection :: forall message . Message message => message -> MessageEncoder message
messageReflection m = coerce (encodeMessage @message (fieldNumber 1) m)
{-# INLINABLE messageReflection #-}

-- | Creates a message encoding by means of type class `Proto3.Suite.Class.Message`.
--
-- Equivalent to @'cacheMessageEncoding' . 'messageReflection'@.
messageCache :: forall message . Message message => message -> MessageEncoding message
messageCache m = cacheMessageEncoding (messageReflection m)
{-# INLINABLE messageCache #-}

instance (Message message, Show message) =>
         Show (MessageEncoding message)
  where
    showsPrec d (messageEncodingToByteString -> bs) = showParen (d >= 11) $
      case fromByteString bs of
        Left _ -> shows 'unsafeByteStringToMessageEncoding . showChar ' ' . showsPrec 11 bs
        Right (msg :: message) -> shows 'messageCache . showChar ' ' . showsPrec 11 msg

instance (Message message, Show message) =>
         Show (MessageEncoder message)
  where
    showsPrec d (messageEncoderToByteString -> bs) = showParen (d >= 11) $
      case fromByteString bs of
        Left _ -> shows 'unsafeByteStringToMessageEncoder . showChar ' ' . showsPrec 11 bs
        Right (msg :: message) -> shows 'messageReflection . showChar ' ' . showsPrec 11 msg

instance (Message message, JSONPB.ToJSONPB message) =>
         JSONPB.ToJSONPB (MessageEncoding message)
  where
    toJSONPB (messageEncodingToByteString -> bs) opts =
      case fromByteString bs of
        Left _ -> JSONPB.toJSONPB @B.ByteString bs opts
        Right (msg :: message) -> JSONPB.toJSONPB @message msg opts

    toEncodingPB (messageEncodingToByteString -> bs) opts =
      case fromByteString bs of
        Left _ -> JSONPB.toEncodingPB @B.ByteString bs opts
        Right (msg :: message) -> JSONPB.toEncodingPB @message msg opts

instance (Message message, JSONPB.ToJSONPB message) =>
         JSONPB.ToJSONPB (MessageEncoder message)
  where
    toJSONPB = JSONPB.toJSONPB @(MessageEncoding message) . cacheMessageEncoding
    toEncodingPB = JSONPB.toEncodingPB @(MessageEncoding message) . cacheMessageEncoding

instance (Message message, JSONPB.FromJSONPB message) =>
         JSONPB.FromJSONPB (MessageEncoding message)
  where
    parseJSONPB v =
      messageCache <$> JSONPB.parseJSONPB @message v <|>
      unsafeByteStringToMessageEncoding <$> JSONPB.parseJSONPB @B.ByteString v

instance (Message message, JSONPB.FromJSONPB message) =>
         JSONPB.FromJSONPB (MessageEncoder message)
  where
    parseJSONPB v =
      messageReflection <$> JSONPB.parseJSONPB @message v <|>
      unsafeByteStringToMessageEncoder <$> JSONPB.parseJSONPB @B.ByteString v

instance (Message message, JSONPB.ToJSONPB message) =>
         Aeson.ToJSON (MessageEncoding message)
  where
    toJSON = JSONPB.toAesonValue
    toEncoding = JSONPB.toAesonEncoding

instance (Message message, JSONPB.ToJSONPB message) =>
         Aeson.ToJSON (MessageEncoder message)
  where
    toJSON = JSONPB.toAesonValue
    toEncoding = JSONPB.toAesonEncoding

instance (Message message, JSONPB.FromJSONPB message) =>
         Aeson.FromJSON (MessageEncoding message)
  where
    parseJSON = JSONPB.parseJSONPB

instance (Message message, JSONPB.FromJSONPB message) =>
         Aeson.FromJSON (MessageEncoder message)
  where
    parseJSON = JSONPB.parseJSONPB
