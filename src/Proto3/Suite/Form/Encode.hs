{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Encodes to protobuf directly from application-specific source data without
-- an intermediate value of a type generated from protobuf message definitions.
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
  , Distinct
  , fieldsToMessage
  , Occupy
  , omitted
  , KnownFieldNumber
  , Field(..)
  , RawField(..)
  , Forward(..)
  , Reverse(..)
  , Vector(..)
  , FoldBuilders(..)
  , message
  , associations
  ) where

import Control.Category (Category(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Type)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Short qualified as TS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeLits (Symbol)
import Prelude hiding (String, (.), id)
import Proto3.Suite.Form.Encode.Core
import Proto3.Suite.Form
         (Association, Omission(..), Packing(..), Repetition(..),
          RepetitionOf, ProtoType(..), ProtoTypeOf)
import Proto3.Suite.Types (Enumerated(..), Fixed(..), Signed(..))
import Proto3.Suite.Types qualified
import Proto3.Wire.Class (ProtoEnum(..))
import Proto3.Wire.Encode qualified as Encode
import Proto3.Wire.Reverse qualified as RB

$(instantiatePackableField [t| 'Int32 |] [t| Int8 |] [| fromIntegral @Int8 @Int32 |])
$(instantiatePackableField [t| 'Int32 |] [t| Word8 |] [| fromIntegral @Word8 @Int32 |])
$(instantiatePackableField [t| 'Int32 |] [t| Int16 |] [| fromIntegral @Int16 @Int32 |])
$(instantiatePackableField [t| 'Int32 |] [t| Word16 |] [| fromIntegral @Word16 @Int32 |])
$(instantiatePackableField [t| 'Int32 |] [t| Int32 |] [| id |])
$(instantiatePackableField [t| 'Int64 |] [t| Int8 |] [| fromIntegral @Int8 @Int32 |])
$(instantiatePackableField [t| 'Int64 |] [t| Word8 |] [| fromIntegral @Word8 @Int32 |])
$(instantiatePackableField [t| 'Int64 |] [t| Int16 |] [| fromIntegral @Int16 @Int32 |])
$(instantiatePackableField [t| 'Int64 |] [t| Word16 |] [| fromIntegral @Word16 @Int32 |])
$(instantiatePackableField [t| 'Int64 |] [t| Int32 |] [| id |])
$(instantiatePackableField [t| 'Int64 |] [t| Word32 |] [| fromIntegral @Word32 @Int64 |])
$(instantiatePackableField [t| 'Int64 |] [t| Int64 |] [| id |])
$(instantiatePackableField [t| 'SInt32 |] [t| Int8 |] [| Signed . fromIntegral @Int8 @Int32 |])
$(instantiatePackableField [t| 'SInt32 |] [t| Word8 |] [| Signed . fromIntegral @Word8 @Int32 |])
$(instantiatePackableField [t| 'SInt32 |] [t| Int16 |] [| Signed . fromIntegral @Int16 @Int32 |])
$(instantiatePackableField [t| 'SInt32 |] [t| Word16 |] [| Signed . fromIntegral @Word16 @Int32 |])
$(instantiatePackableField [t| 'SInt32 |] [t| Int32 |] [| Signed |])
$(instantiatePackableField [t| 'SInt64 |] [t| Int8 |] [| Signed . fromIntegral @Int8 @Int32 |])
$(instantiatePackableField [t| 'SInt64 |] [t| Word8 |] [| Signed . fromIntegral @Word8 @Int32 |])
$(instantiatePackableField [t| 'SInt64 |] [t| Int16 |] [| Signed . fromIntegral @Int16 @Int32 |])
$(instantiatePackableField [t| 'SInt64 |] [t| Word16 |] [| Signed . fromIntegral @Word16 @Int32 |])
$(instantiatePackableField [t| 'SInt64 |] [t| Int32 |] [| Signed |])
$(instantiatePackableField [t| 'SInt64 |] [t| Word32 |] [| Signed . fromIntegral @Word32 @Int64 |])
$(instantiatePackableField [t| 'SInt64 |] [t| Int64 |] [| Signed |])
$(instantiatePackableField [t| 'UInt32 |] [t| Word8 |] [| fromIntegral @Word8 @Word32 |])
$(instantiatePackableField [t| 'UInt32 |] [t| Word16 |] [| fromIntegral @Word16 @Word32 |])
$(instantiatePackableField [t| 'UInt32 |] [t| Word32 |] [| id |])
$(instantiatePackableField [t| 'UInt64 |] [t| Word8 |] [| fromIntegral @Word8 @Word32 |])
$(instantiatePackableField [t| 'UInt64 |] [t| Word16 |] [| fromIntegral @Word16 @Word32 |])
$(instantiatePackableField [t| 'UInt64 |] [t| Word32 |] [| id |])
$(instantiatePackableField [t| 'UInt64 |] [t| Word64 |] [| id |])
$(instantiatePackableField [t| 'Fixed32 |] [t| Word8 |] [| Fixed . fromIntegral @Word8 @Word32 |])
$(instantiatePackableField [t| 'Fixed32 |] [t| Word16 |] [| Fixed . fromIntegral @Word16 @Word32 |])
$(instantiatePackableField [t| 'Fixed32 |] [t| Word32 |] [| Fixed |])
$(instantiatePackableField [t| 'Fixed64 |] [t| Word8 |] [| Fixed . fromIntegral @Word8 @Word64 |])
$(instantiatePackableField [t| 'Fixed64 |] [t| Word16 |] [| Fixed . fromIntegral @Word16 @Word64 |])
$(instantiatePackableField [t| 'Fixed64 |] [t| Word32 |] [| Fixed . fromIntegral @Word32 @Word64 |])
$(instantiatePackableField [t| 'Fixed64 |] [t| Word64 |] [| Fixed |])
$(instantiatePackableField [t| 'SFixed32 |] [t| Int8 |] [| Signed . Fixed . fromIntegral @Int8 @Int32 |])
$(instantiatePackableField [t| 'SFixed32 |] [t| Word8 |] [| Signed . Fixed . fromIntegral @Word8 @Int32 |])
$(instantiatePackableField [t| 'SFixed32 |] [t| Int16 |] [| Signed . Fixed . fromIntegral @Int16 @Int32 |])
$(instantiatePackableField [t| 'SFixed32 |] [t| Word16 |] [| Signed . Fixed . fromIntegral @Word16 @Int32 |])
$(instantiatePackableField [t| 'SFixed32 |] [t| Int32 |] [| Signed . Fixed |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Int8 |] [| Signed . Fixed . fromIntegral @Int8 @Int64 |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Word8 |] [| Signed . Fixed . fromIntegral @Word8 @Int64 |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Int16 |] [| Signed . Fixed . fromIntegral @Int16 @Int64 |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Word16 |] [| Signed . Fixed . fromIntegral @Word16 @Int64 |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Int32 |] [| Signed . Fixed . fromIntegral @Int32 @Int64 |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Word32 |] [| Signed . Fixed . fromIntegral @Word32 @Int64 |])
$(instantiatePackableField [t| 'SFixed64 |] [t| Int64 |] [| Signed . Fixed |])
$(instantiatePackableField [t| 'Bool |] [t| Bool |] [| id |])
$(instantiatePackableField [t| 'Float |] [t| Float |] [| id |])
$(instantiatePackableField [t| 'Double |] [t| Float |] [| realToFrac @Float @Double |])
$(instantiatePackableField [t| 'Double |] [t| Double |] [| id |])

$(instantiateStringOrBytesField
   [t| 'String |]
   [t| Proto3.Suite.Types.String |]
   [ [t| T.Text |], [t| TL.Text |], [t| TS.ShortText |] ]
 )
$(instantiateStringOrBytesField
   [t| 'Bytes |]
   [t| Proto3.Suite.Types.Bytes |]
   [ [t| B.ByteString |], [t| BL.ByteString |] ]
 )

instance ( ProtoEnum e
         , RawField ('Singular omission) 'Int32 Int32
         ) =>
         RawField ('Singular omission) ('Enumeration e) e
  where
    rawField !fn x = rawField @('Singular omission) @'Int32 fn (fromProtoEnum x)
    {-# INLINE rawField #-}

instance ProtoEnum e =>
         RawField 'Optional ('Enumeration e) (Maybe e)
  where
    rawField !fn x = rawField @'Optional @'Int32 fn (fmap fromProtoEnum x)
    {-# INLINE rawField #-}

instance ( ProtoEnum e
         , Functor t
         , RawField ('Repeated packing) 'Int32 (t Int32)
         ) =>
         RawField ('Repeated packing) ('Enumeration e) (t e)
  where
    rawField !fn xs = rawField @('Repeated packing) @'Int32 fn (fmap fromProtoEnum xs)
    {-# INLINE rawField #-}

instance ( ProtoEnum e
         , RawField ('Singular omission) 'Int32 Int32
         ) =>
         RawField ('Singular omission) ('Enumeration e) (Enumerated e)
  where
    rawField !fn x = rawField @('Singular omission) @'Int32 fn (codeFromEnumerated x)
    {-# INLINE rawField #-}

instance ProtoEnum e =>
         RawField 'Optional ('Enumeration e) (Maybe (Enumerated e))
  where
    rawField !fn x = rawField @'Optional @'Int32 fn (fmap codeFromEnumerated x)
    {-# INLINE rawField #-}

instance ( ProtoEnum e
         , Functor t
         , RawField ('Repeated packing) 'Int32 (t Int32)
         ) =>
         RawField ('Repeated packing) ('Enumeration e) (t (Enumerated e))
  where
    rawField !fn xs = rawField @('Repeated packing) @'Int32 fn (fmap codeFromEnumerated xs)
    {-# INLINE rawField #-}

instance RawField ('Singular 'Alternative) 'Bytes RB.BuildR
  where
    rawField !fn x = Encode.bytes fn x
    {-# INLINE rawField #-}

instance RawField ('Singular 'Implicit) 'Bytes RB.BuildR
  where
    rawField !fn x = Encode.bytesIfNonempty fn x
    {-# INLINE rawField #-}

instance RawField 'Optional 'Bytes (Maybe RB.BuildR)
  where
    rawField !fn = maybe mempty (Encode.bytes fn)
    {-# INLINE rawField #-}

instance forall t . FoldBuilders t =>
         RawField ('Repeated 'Unpacked) 'Bytes (t RB.BuildR)
  where
    rawField !fn xs = foldBuilders (Encode.bytes fn <$> xs)
    {-# INLINE rawField #-}

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
