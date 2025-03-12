{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  , Field(..)
  , FieldForm(..)
  , Wrap(..)
  , Auto(..)
  , FoldBuilders(..)
  , Forward(..)
  , Reverse(..)
  , ReverseN(..)
  , MapToRepeated(..)
  , ToRepeated(..)
  , foldPrefixes
  , message
  , associations
  , Reflection(..)
  , messageReflection
  ) where

import Control.Category (Category(..))
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
import GHC.TypeLits (Symbol)
import Prelude hiding (String, (.), id)
import Proto3.Suite.Class (Message, MessageField, encodeMessage, encodeMessageField)
import Proto3.Suite.Form.Encode.Core
import Proto3.Suite.Form.Encode.Repeated
         (FoldBuilders(..), Forward(..), Reverse(..), ReverseN(..),
          MapToRepeated(..), ToRepeated(..))
import Proto3.Suite.Form
         (Association, MessageFieldType, Omission(..), Packing(..),
          Repetition(..), RepetitionOf, ProtoType(..), ProtoTypeOf)
import Proto3.Suite.Types (Enumerated(..), Fixed(..), Signed(..))
import Proto3.Suite.Types qualified
import Proto3.Wire qualified as Wire
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Reverse qualified as RB

$(instantiatePackableField [t| 'Int32 |] [t| Int8 |] [| fromIntegral @Int8 @Int32 |] True False)
$(instantiatePackableField [t| 'Int32 |] [t| Word8 |] [| fromIntegral @Word8 @Int32 |] True False)
$(instantiatePackableField [t| 'Int32 |] [t| Int16 |] [| fromIntegral @Int16 @Int32 |] True False)
$(instantiatePackableField [t| 'Int32 |] [t| Word16 |] [| fromIntegral @Word16 @Int32 |] True False)
$(instantiatePackableField [t| 'Int32 |] [t| Int32 |] [| id |] True True)
$(instantiatePackableField [t| 'Int64 |] [t| Int8 |] [| fromIntegral @Int8 @Int32 |] True False)
$(instantiatePackableField [t| 'Int64 |] [t| Word8 |] [| fromIntegral @Word8 @Int32 |] True False)
$(instantiatePackableField [t| 'Int64 |] [t| Int16 |] [| fromIntegral @Int16 @Int32 |] True False)
$(instantiatePackableField [t| 'Int64 |] [t| Word16 |] [| fromIntegral @Word16 @Int32 |] True False)
$(instantiatePackableField [t| 'Int64 |] [t| Int32 |] [| id |] True False)
$(instantiatePackableField [t| 'Int64 |] [t| Word32 |] [| fromIntegral @Word32 @Int64 |] True False)
$(instantiatePackableField [t| 'Int64 |] [t| Int64 |] [| id |] True True)
$(instantiatePackableField [t| 'SInt32 |] [t| Int8 |] [| Signed . fromIntegral @Int8 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt32 |] [t| Word8 |] [| Signed . fromIntegral @Word8 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt32 |] [t| Int16 |] [| Signed . fromIntegral @Int16 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt32 |] [t| Word16 |] [| Signed . fromIntegral @Word16 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt32 |] [t| Int32 |] [| Signed |] False True)
$(instantiatePackableField [t| 'SInt64 |] [t| Int8 |] [| Signed . fromIntegral @Int8 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt64 |] [t| Word8 |] [| Signed . fromIntegral @Word8 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt64 |] [t| Int16 |] [| Signed . fromIntegral @Int16 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt64 |] [t| Word16 |] [| Signed . fromIntegral @Word16 @Int32 |] False False)
$(instantiatePackableField [t| 'SInt64 |] [t| Int32 |] [| Signed |] False False)
$(instantiatePackableField [t| 'SInt64 |] [t| Word32 |] [| Signed . fromIntegral @Word32 @Int64 |] False False)
$(instantiatePackableField [t| 'SInt64 |] [t| Int64 |] [| Signed |] False True)
$(instantiatePackableField [t| 'UInt32 |] [t| Word8 |] [| fromIntegral @Word8 @Word32 |] True False)
$(instantiatePackableField [t| 'UInt32 |] [t| Word16 |] [| fromIntegral @Word16 @Word32 |] True False)
$(instantiatePackableField [t| 'UInt32 |] [t| Word32 |] [| id |] True True)
$(instantiatePackableField [t| 'UInt64 |] [t| Word8 |] [| fromIntegral @Word8 @Word32 |] True False)
$(instantiatePackableField [t| 'UInt64 |] [t| Word16 |] [| fromIntegral @Word16 @Word32 |] True False)
$(instantiatePackableField [t| 'UInt64 |] [t| Word32 |] [| id |] True False)
$(instantiatePackableField [t| 'UInt64 |] [t| Word64 |] [| id |] True True)
$(instantiatePackableField [t| 'Fixed32 |] [t| Word8 |] [| Fixed . fromIntegral @Word8 @Word32 |] False False)
$(instantiatePackableField [t| 'Fixed32 |] [t| Word16 |] [| Fixed . fromIntegral @Word16 @Word32 |] False False)
$(instantiatePackableField [t| 'Fixed32 |] [t| Word32 |] [| Fixed |] False True)
$(instantiatePackableField [t| 'Fixed64 |] [t| Word8 |] [| Fixed . fromIntegral @Word8 @Word64 |] False False)
$(instantiatePackableField [t| 'Fixed64 |] [t| Word16 |] [| Fixed . fromIntegral @Word16 @Word64 |] False False)
$(instantiatePackableField [t| 'Fixed64 |] [t| Word32 |] [| Fixed . fromIntegral @Word32 @Word64 |] False False)
$(instantiatePackableField [t| 'Fixed64 |] [t| Word64 |] [| Fixed |] False True)
$(instantiatePackableField [t| 'SFixed32 |] [t| Int8 |] [| Signed . Fixed . fromIntegral @Int8 @Int32 |] False False)
$(instantiatePackableField [t| 'SFixed32 |] [t| Word8 |] [| Signed . Fixed . fromIntegral @Word8 @Int32 |] False False)
$(instantiatePackableField [t| 'SFixed32 |] [t| Int16 |] [| Signed . Fixed . fromIntegral @Int16 @Int32 |] False False)
$(instantiatePackableField [t| 'SFixed32 |] [t| Word16 |] [| Signed . Fixed . fromIntegral @Word16 @Int32 |] False False)
$(instantiatePackableField [t| 'SFixed32 |] [t| Int32 |] [| Signed . Fixed |] False True)
$(instantiatePackableField [t| 'SFixed64 |] [t| Int8 |] [| Signed . Fixed . fromIntegral @Int8 @Int64 |] False False)
$(instantiatePackableField [t| 'SFixed64 |] [t| Word8 |] [| Signed . Fixed . fromIntegral @Word8 @Int64 |] False False)
$(instantiatePackableField [t| 'SFixed64 |] [t| Int16 |] [| Signed . Fixed . fromIntegral @Int16 @Int64 |] False False)
$(instantiatePackableField [t| 'SFixed64 |] [t| Word16 |] [| Signed . Fixed . fromIntegral @Word16 @Int64 |] False False)
$(instantiatePackableField [t| 'SFixed64 |] [t| Int32 |] [| Signed . Fixed . fromIntegral @Int32 @Int64 |] False False)
$(instantiatePackableField [t| 'SFixed64 |] [t| Word32 |] [| Signed . Fixed . fromIntegral @Word32 @Int64 |] False False)
$(instantiatePackableField [t| 'SFixed64 |] [t| Int64 |] [| Signed . Fixed |] False True)
$(instantiatePackableField [t| 'Bool |] [t| Bool |] [| id |] True True)
$(instantiatePackableField [t| 'Float |] [t| Float |] [| id |] True True)
$(instantiatePackableField [t| 'Double |] [t| Float |] [| realToFrac @Float @Double |] True False)
$(instantiatePackableField [t| 'Double |] [t| Double |] [| id |] True True)

$(instantiateStringOrBytesField
   [t| 'String |]
   [t| Proto3.Suite.Types.String |]
   [ [t| TS.ShortText |], [t| T.Text |], [t| TL.Text |] ]
 )
$(instantiateStringOrBytesField
   [t| 'Bytes |]
   [t| Proto3.Suite.Types.Bytes |]
   [ [t| BS.ShortByteString |], [t| B.ByteString |], [t| BL.ByteString |] ]
 )

instance ( ProtoEnum e
         , FieldForm ('Singular omission) 'Int32 Int32
         ) =>
         FieldForm ('Singular omission) ('Enumeration e) e
  where
    fieldForm rep _ !fn x = fieldForm rep (proxy# :: Proxy# 'Int32) fn (fromProtoEnum x)
    {-# INLINE fieldForm #-}

instance ( ProtoEnum e
         , Functor t
         , FieldForm ('Repeated 'Packed) 'Int32 (t Int32)
         ) =>
         FieldForm ('Repeated 'Packed) ('Enumeration e) (t e)
  where
    fieldForm rep _ !fn xs = fieldForm rep (proxy# :: Proxy# 'Int32) fn (fmap fromProtoEnum xs)
    {-# INLINE fieldForm #-}

instance ( ProtoEnum e
         , FieldForm ('Singular omission) 'Int32 Int32
         ) =>
         FieldForm ('Singular omission) ('Enumeration e) (Enumerated e)
  where
    fieldForm rep _ !fn x = fieldForm rep (proxy# :: Proxy# 'Int32) fn (codeFromEnumerated x)
    {-# INLINE fieldForm #-}

instance ( ProtoEnum e
         , Functor t
         , FieldForm ('Repeated 'Packed) 'Int32 (t Int32)
         ) =>
         FieldForm ('Repeated 'Packed) ('Enumeration e) (t (Enumerated e))
  where
    fieldForm rep _ !fn xs = fieldForm rep (proxy# :: Proxy# 'Int32) fn (fmap codeFromEnumerated xs)
    {-# INLINE fieldForm #-}

instance FieldForm ('Singular 'Alternative) 'Bytes RB.BuildR
  where
    fieldForm _ _ = Encode.bytes
    {-# INLINE fieldForm #-}

instance FieldForm ('Singular 'Implicit) 'Bytes RB.BuildR
  where
    fieldForm _ _ = Encode.bytesIfNonempty
    {-# INLINE fieldForm #-}

-- | Combines 'Prefix' builders for zero or more repeated fields.
foldPrefixes ::
  forall t message names .
  FoldBuilders t =>
  t (Prefix message names names) ->
  Prefix message names names
foldPrefixes prefixes = UnsafePrefix (foldBuilders @t (fmap @t untypedPrefix prefixes))
{-# INLINE foldPrefixes #-}

-- | Specializes the argument type of 'field' to the encoding of a submessage type,
-- which can help to avoid ambiguity when the argument expression is polymorphic.
message ::
  forall (name :: Symbol) (inner :: Type) (outer :: Type) (names :: [Symbol]) .
  ( ProtoTypeOf outer name ~ 'Message inner
  , Field name (MessageEncoder inner) outer
  , KnownFieldNumber outer name
  ) =>
  MessageEncoder inner ->
  Prefix outer names (Occupy outer name names)
message = field @name @(MessageEncoder inner)

-- | Specializes the argument type of 'field' to be a sequence of key-value pair encodings,
-- which can help to avoid ambiguity when the argument expression is polymorphic.
associations ::
  forall (name :: Symbol) (t :: Type -> Type) (key :: ProtoType) (value :: ProtoType)
         (message :: Type) (names :: [Symbol]) .
  ( ProtoTypeOf message name ~ 'Map key value
  , RepetitionOf message name ~ 'Repeated 'Unpacked
  , Field name (t (MessageEncoder (Association key value))) message
  , KnownFieldNumber message name
  ) =>
  t (MessageEncoder (Association key value)) ->
  Prefix message names names
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

instance ( MessageFieldType repetition protoType a
         , MessageField a
         ) =>
         FieldForm repetition protoType (Reflection a)
  where
    fieldForm _ _ = coerce (encodeMessageField @a)
    {-# INLINE fieldForm #-}

-- | Creates a message encoder by means of type class `Proto3.Suite.Class.Message`.
--
-- To encode a field instead of a top-level message, use 'Reflection'.
messageReflection :: forall a . Message a => a -> MessageEncoder a
messageReflection m = coerce (encodeMessage @a (Wire.fieldNumber 1) m)
{-# INLINABLE messageReflection #-}
