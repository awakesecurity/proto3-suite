{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if !(MIN_VERSION_dhall(1,27,0))
#define FromDhall Interpret
#define ToDhall Inject
#endif

module Proto3.Suite.DhallPB
  ( -- * Modules
    module Dhall
  )
where
import           Data.Functor.Contravariant  (contramap)
import           Data.Int                    (Int32, Int64)
import           Data.Word                   (Word32, Word64)
import           Dhall                       (FromDhall (..), ToDhall (..))
import           GHC.Float                   (double2Float, float2Double)
import           Proto3.Suite.Types          (Enumerated (..), Fixed (..))

import qualified Data.ByteString
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Base64.Lazy
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding
import qualified Dhall

#if !(MIN_VERSION_dhall(1,27,0))
import qualified Data.Map
#endif

--------------------------------------------------------------------------------
-- Interpret the special 'Enumerated' type

instance Dhall.FromDhall a => Dhall.FromDhall (Enumerated a)

instance Dhall.FromDhall a => Dhall.FromDhall (Either Int32 a)

--------------------------------------------------------------------------------
-- Interpret the strict and lazy ByteString types
--
-- We must base-64 decode a 'ByteString' after encoding it from a Text
-- because it may contain invalid UTF-8 data and Dhall does not have a
-- native type for bytes.

instance Dhall.FromDhall Data.ByteString.Lazy.ByteString where
  autoWith _ = fmap b64Decode Dhall.lazyText
    where
      b64Decode = Data.ByteString.Base64.Lazy.decodeLenient . Data.Text.Lazy.Encoding.encodeUtf8

instance Dhall.FromDhall Data.ByteString.ByteString where
  autoWith _ = fmap b64Decode Dhall.strictText
    where
      b64Decode =  Data.ByteString.Base64.decodeLenient . Data.Text.Encoding.encodeUtf8

--------------------------------------------------------------------------------
-- Interpret integer scalar types

-- Lossy interpretation and integer overflow can happen with the
-- following instances because the 'Dhall.Integer' (Dhall's only
-- integer type) equals Haskell's Integer type. We don't expect these
-- instances to introduce integer overflow because they should only
-- interpret Dhall rendered from protobuf messages created with
-- generated code.
--
-- TODO: we should perform run-time bounds-checking to at least hint
-- to the user that we interpreted something bad.

-- instance Dhall.FromDhall Int where
--   autoWith _ = fmap fromInteger Dhall.integer

-- instance Dhall.FromDhall Int32 where
--   autoWith _ = fmap fromInteger Dhall.integer

-- instance Dhall.FromDhall Int64 where
--   autoWith _ = fmap fromInteger Dhall.integer

-- instance Dhall.FromDhall Word32 where
--   autoWith _ = fmap fromIntegral Dhall.integer

-- instance Dhall.FromDhall Word64 where
--   autoWith _ = fmap fromIntegral Dhall.integer

instance Dhall.FromDhall (Fixed Int32) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.FromDhall (Fixed Int64) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.FromDhall (Fixed Word32) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.FromDhall (Fixed Word64) where
  autoWith = fmap Fixed . Dhall.autoWith

--------------------------------------------------------------------------------
-- Interpret floating point scalar types
--
-- Loss of precision can happen when converting a 'Double' to a
-- 'Float'. We don't expect this instance to introduce loss of
-- precision because it should only interpret Dhall rendered from
-- protobuf messages created with generated code. The Dhall rendering
-- converts from a 'Float' to the 'Dhall.Double' type.

instance Dhall.FromDhall Float where
  autoWith _ = fmap double2Float Dhall.double

#if !(MIN_VERSION_dhall(1,27,0))
--------------------------------------------------------------------------------
-- Interpret maps
--
-- Dhall has no map type.  We resort to an association list,
-- though that is not safe because keys may be repeated.

instance (Dhall.Interpret k, Dhall.Interpret v, Ord k) =>
         Dhall.Interpret (Data.Map.Map k v) where
  autoWith = fmap (fmap Data.Map.fromList) Dhall.autoWith
#endif

--------------------------------------------------------------------------------
-- Inject the special 'Enumerated' type

instance Dhall.ToDhall a => Dhall.ToDhall (Enumerated a)

instance Dhall.ToDhall a => Dhall.ToDhall (Either Int32 a)

--------------------------------------------------------------------------------
-- Inject integer scalar types

instance Dhall.ToDhall Int32 where
  injectWith = fmap (contramap toInteger) Dhall.injectWith

instance Dhall.ToDhall Int64 where
  injectWith = fmap (contramap toInteger) Dhall.injectWith

instance Dhall.ToDhall (Fixed Int32) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.ToDhall (Fixed Int64) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.ToDhall (Fixed Word32) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.ToDhall (Fixed Word64) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

--------------------------------------------------------------------------------
-- Inject floating point scalar types

instance Dhall.ToDhall Float where
  injectWith = fmap (contramap float2Double) Dhall.injectWith

--------------------------------------------------------------------------------
-- Inject strict and lazy ByteStrings
--
-- We must base-64 encode a 'ByteString' before decoding it to a Text
-- because it may contain invalid UTF-8 data and Dhall does not have a
-- native type for bytes.

instance Dhall.ToDhall Data.ByteString.Lazy.ByteString where
  injectWith = fmap (contramap b64Encode) Dhall.injectWith
    where
      -- 'decodeUtf8' will throw an error on any invalid UTF-8 data
      -- but we should never encounter that case with this usage
      -- because we Base64 encode the ByteString first
      b64Encode = Data.Text.Lazy.Encoding.decodeUtf8 . Data.ByteString.Base64.Lazy.encode

instance Dhall.ToDhall Data.ByteString.ByteString where
  injectWith = fmap (contramap b64Encode) Dhall.injectWith
    where
      -- 'decodeUtf8' will throw an error on any invalid UTF-8 data
      -- but we should never encounter that case with this usage
      -- because we Base64 encode the ByteString first
      b64Encode = Data.Text.Encoding.decodeUtf8 . Data.ByteString.Base64.encode

#if !(MIN_VERSION_dhall(1,27,0))
--------------------------------------------------------------------------------
-- Inject maps
--
-- Dhall has no map type.  We resort to an association list,
-- though that is not safe because keys may be repeated.

instance (Dhall.Inject k, Dhall.Inject v) =>
         Dhall.Inject (Data.Map.Map k v) where
  injectWith = fmap (contramap Data.Map.toAscList) Dhall.injectWith
#endif
