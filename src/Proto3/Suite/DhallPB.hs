{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Proto3.Suite.DhallPB where

import           Data.Functor.Contravariant (contramap)
import           Data.Int                   (Int32, Int64)
import           Data.Word                  (Word32, Word64)
import           GHC.Float                  (double2Float, float2Double)
import           Proto3.Suite.Types         (Enumerated (..), Fixed (..))

import qualified Data.ByteString.Base64
import qualified Data.ByteString.Base64.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding
import qualified Dhall

--------------------------------------------------------------------------------
-- Interpret the special 'Enumerated' type

instance Dhall.Interpret a => Dhall.Interpret (Enumerated a)

instance Dhall.Interpret a => Dhall.Interpret (Either Int a)

--------------------------------------------------------------------------------
-- Interpret the strict and lazy ByteString types
--
-- We must base-64 decode a 'ByteString' after encoding it from a Text
-- because it may contain invalid UTF-8 data and Dhall does not have a
-- native type for bytes.

instance Dhall.Interpret Data.ByteString.Lazy.ByteString where
  autoWith _ = fmap b64Decode Dhall.lazyText
    where
      b64Decode = Data.ByteString.Base64.Lazy.decodeLenient . Data.Text.Lazy.Encoding.encodeUtf8

instance Dhall.Interpret Data.ByteString.ByteString where
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

instance Dhall.Interpret Int where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Int32 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Int64 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.Interpret Word32 where
  autoWith _ = fmap fromIntegral Dhall.integer

instance Dhall.Interpret Word64 where
  autoWith _ = fmap fromIntegral Dhall.integer

instance Dhall.Interpret (Fixed Int32) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.Interpret (Fixed Int64) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.Interpret (Fixed Word32) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.Interpret (Fixed Word64) where
  autoWith = fmap Fixed . Dhall.autoWith

--------------------------------------------------------------------------------
-- Interpret floating point scalar types
--
-- Loss of precision can happen when converting a 'Double' to a
-- 'Float'. We don't expect this instance to introduce loss of
-- precision because it should only interpret Dhall rendered from
-- protobuf messages created with generated code. The Dhall rendering
-- converts from a 'Float' to the 'Dhall.Double' type.

instance Dhall.Interpret Float where
  autoWith _ = fmap double2Float Dhall.double

--------------------------------------------------------------------------------
-- Inject the special 'Enumerated' type

instance Dhall.Inject a => Dhall.Inject (Enumerated a)

instance Dhall.Inject a => Dhall.Inject (Either Int a)

--------------------------------------------------------------------------------
-- Inject integer scalar types

instance Dhall.Inject Int32 where
  injectWith = fmap (contramap toInteger) Dhall.injectWith

instance Dhall.Inject Int64 where
  injectWith = fmap (contramap toInteger) Dhall.injectWith

instance Dhall.Inject (Fixed Int32) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.Inject (Fixed Int64) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.Inject (Fixed Word32) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.Inject (Fixed Word64) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

--------------------------------------------------------------------------------
-- Inject floating point scalar types

instance Dhall.Inject Float where
  injectWith = fmap (contramap float2Double) Dhall.injectWith

--------------------------------------------------------------------------------
-- Inject strict and lazy ByteStrings
--
-- We must base-64 encode a 'ByteString' before decoding it to a Text
-- because it may contain invalid UTF-8 data and Dhall does not have a
-- native type for bytes.

instance Dhall.Inject Data.ByteString.Lazy.ByteString where
  injectWith = fmap (contramap b64Encode) Dhall.injectWith
    where
      -- 'decodeUtf8' will throw an error on any invalid UTF-8 data
      -- but we should never encounter that case with this usage
      -- because we Base64 encode the ByteString first
      b64Encode = Data.Text.Lazy.Encoding.decodeUtf8 . Data.ByteString.Base64.Lazy.encode

instance Dhall.Inject Data.ByteString.ByteString where
  injectWith = fmap (contramap b64Encode) Dhall.injectWith
    where
      -- 'decodeUtf8' will throw an error on any invalid UTF-8 data
      -- but we should never encounter that case with this usage
      -- because we Base64 encode the ByteString first
      b64Encode = Data.Text.Encoding.decodeUtf8 . Data.ByteString.Base64.encode
