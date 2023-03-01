{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

#ifdef LARGE_RECORDS
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

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

#ifdef LARGE_RECORDS
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import qualified Data.Text as Text
import qualified Dhall.Core as Dhall
import qualified Dhall.Map as Dhall
import qualified Dhall.Parser as Dhall
import Data.Record.Generic ((:.:)(Comp), I (..), K (..))
import qualified Data.Record.Generic as LG
import qualified Data.Record.Generic.GHC as LG
import qualified Data.Record.Generic.Rep as LG
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

#if !MIN_VERSION_dhall(1,35,0)
instance Dhall.FromDhall Int where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.FromDhall Int32 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.FromDhall Int64 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.FromDhall Word32 where
  autoWith _ = fmap fromIntegral Dhall.integer

instance Dhall.FromDhall Word64 where
  autoWith _ = fmap fromIntegral Dhall.integer
#endif

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

#ifdef LARGE_RECORDS

instance (LG.Generic a, LG.Constraints a Dhall.FromDhall) => Dhall.GenericFromDhall a (LG.ThroughLRGenerics a) where
  genericAutoWithNormalizer _ normalizer _ = pure $ coerce $ Dhall.Decoder extract expected
    where
      makeCompRep :: (forall x. Dhall.FromDhall x => K String x -> f (g x)) -> LG.Rep (f :.: g) a
      makeCompRep f = LG.cmap
                        (Proxy @Dhall.FromDhall)
                        (Comp . f)
                        (LG.recordFieldNames $ LG.metadata (Proxy @a))

      extract :: Dhall.Expr Dhall.Src Void -> Dhall.Extractor Dhall.Src Void a
      extract expr =
        case expr of
          Dhall.RecordLit flds -> do
            let getField :: forall x. Dhall.FromDhall x => K String x -> Dhall.Extractor Dhall.Src Void x
                getField (K fld) =
                  let decoder = Dhall.autoWith @x normalizer
                  in maybe
                       (Dhall.typeError expected expr)
                       (Dhall.extract decoder)
                       (Dhall.recordFieldValue <$> Dhall.lookup (Text.pack fld) flds)
            LG.to <$> LG.sequenceA (makeCompRep (fmap I . getField))
          _ -> Dhall.typeError expected expr

      expected :: Dhall.Expector (Dhall.Expr Dhall.Src Void)
      expected = do
        let getField :: forall x. Dhall.FromDhall x => K String x -> Dhall.Expector (Dhall.Text, Dhall.RecordField Dhall.Src Void)
            getField (K fld) = do
              let decoder = Dhall.autoWith @x normalizer
              f <- Dhall.makeRecordField <$> Dhall.expected decoder
              pure (Text.pack fld, f)
        Dhall.Record . Dhall.fromList . LG.collapse <$> LG.sequenceA (makeCompRep (fmap K . getField))

instance (LG.Generic a, LG.Constraints a Dhall.ToDhall) => Dhall.GenericToDhall (LG.ThroughLRGenerics a) where
  genericToDhallWithNormalizer normalizer _ = pure $ coerce $ Dhall.Encoder embed declared
    where
      md = LG.metadata (Proxy @a)
      fieldNames = LG.recordFieldNames md

      embed :: a -> Dhall.Expr Dhall.Src Void
      embed = Dhall.RecordLit
              . Dhall.fromList
              . LG.collapse
              . LG.zipWith (LG.mapKKK $ \n x -> (Text.pack n, Dhall.makeRecordField x)) fieldNames
              . LG.cmap (Proxy @Dhall.ToDhall) (K . Dhall.embed (injectWith normalizer) . LG.unI)
              . LG.from

      declared :: Dhall.Expr Dhall.Src Void
      declared = Dhall.Record
                 $ Dhall.fromList
                 $ LG.collapse
                 $ LG.cmap
                     (Proxy @Dhall.ToDhall)
                     (\(K n :: K String x) ->
                         let typ = Dhall.declared (injectWith @x normalizer)
                         in K (Text.pack n, Dhall.makeRecordField typ))
                     fieldNames

#endif
