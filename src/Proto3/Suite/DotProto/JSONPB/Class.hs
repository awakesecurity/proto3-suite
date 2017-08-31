{-# LANGUAGE DefaultSignatures #-}

-- | Aeson-like helper functions and typeclasses for converting to and from
-- values of types with a protobuf representation and an IR (we reuse the
-- 'A.Value' type) which represents the "jsonpb" canonical JSON encoding
-- described at https://developers.google.com/protocol-buffers/docs/proto3#json.
--
-- This module also presents a (very minimal) surface syntax for Aeson-like
-- operations; the idea is that we can write 'ToJSONPB' and 'FromJSONPB'
-- instances in a very similar manner to 'A.ToJSON' and 'A.FromJSON' instances,
-- except that jsonpb codec implementations are produced instead of vanilla JSON
-- codecs.

module Proto3.Suite.DotProto.JSONPB.Class where

import qualified Data.Aeson                       as A (Encoding, FromJSON (..),
                                                        ToJSON (..), Value (..),
                                                        json, (.!=))
import qualified Data.Aeson.Encoding              as E (encodingToLazyByteString,
                                                        pair, pairs)
import qualified Data.Aeson.Internal              as A (formatError, iparse)
import qualified Data.Aeson.Parser                as A (eitherDecodeWith)
import qualified Data.Aeson.Types                 as A (Object, Options (..),
                                                        Parser, Series,
                                                        defaultOptions,
                                                        explicitParseFieldMaybe)
import qualified Data.Attoparsec.ByteString       as Atto (skipWhile)
import qualified Data.Attoparsec.ByteString.Char8 as Atto (Parser, endOfInput)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Char                        (toLower)
import qualified Data.Proxy                       as DP
import           Data.Text                        (Text)

import           Proto3.Suite.Class               (HasDefault (def, isDefault),
                                                   Named (nameOf))

-- * Typeclass definitions

-- | 'A.ToJSON' variant for jsonpb direct encoding via 'A.Encoding'
class ToJSONPB a where
  -- | 'A.toEncoding' variant for jsonpb encoder implementations. Equivalent to
  -- 'A.toEncoding' if an implementation is not provided.
  toEncodingPB :: a -> A.Encoding
  default toEncodingPB :: (A.ToJSON a) => a -> A.Encoding
  toEncodingPB = A.toEncoding

-- | 'A.FromJSON' variant for jsonpb decoding from the aeson 'A.Value' IR
class FromJSONPB a where
  -- | 'A.parseJSON' variant for jsonpb decoder implementations. Equivalent to
  -- 'A.parseJSON' if an implementation is not provided.
  parseJSONPB :: A.Value -> A.Parser a

  default parseJSONPB :: (A.FromJSON a) => A.Value -> A.Parser a
  parseJSONPB = A.parseJSON

-- * JSONPB codec entry points

-- | 'Data.Aeson.encode' variant for serializing a JSONPB value as a lazy
-- 'LBS.ByteString'.
encode :: ToJSONPB a => a -> LBS.ByteString
encode = E.encodingToLazyByteString . toEncodingPB
{-# INLINE encode #-}

-- | 'Data.Aeson..eitherDecode' variant for deserializing a JSONPB value from a
-- lazy 'LBS.ByteString'.
eitherDecode :: FromJSONPB a => LBS.ByteString -> Either String a
eitherDecode = eitherFormatError . A.eitherDecodeWith jsonEOF (A.iparse parseJSONPB)
  where
    eitherFormatError = either (Left . uncurry A.formatError) Right
    {-# INLINE eitherFormatError #-}

    -- NB: cribbed from aeson-1.1.1.0:Data.Aeson.Parser.Internal.jsonEOF, which
    -- is not exported. It's simple, so we just inline it here. Might be worth
    -- submitting a PR to export this.
    jsonEOF :: Atto.Parser A.Value
    jsonEOF = A.json <* skipSpace <* Atto.endOfInput
      where
        skipSpace :: Atto.Parser ()
        skipSpace = Atto.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
        {-# INLINE skipSpace #-}
{-# INLINE eitherDecode #-}

-- * Operator definitions

-- | Construct key-value pair, e.g.:
--
-- @
-- instance ToJSONPB MyType where
--   toEncodingPB (MyType fld0 fld1) = fieldsPB False
--     [ "fld0" .= fld0
--     , "fld1" .= fld1
--     ]
-- @
(.=) :: (HasDefault v, ToJSONPB v) => Text -> v -> FieldPB
k .= v = FieldPB (isDefault v) (E.pair k (toEncodingPB v))

-- | 'Data.Aeson..:' variant for jsonpb decoding; if the given key is missing
-- from the object, or if it is present but its value is null, we produce the
-- default protobuf value for the field type.
(.:) :: (FromJSONPB a, HasDefault a) => A.Object -> Text -> A.Parser a
obj .: key = obj .:? key A..!= def
  where
    (.:?) = A.explicitParseFieldMaybe parseJSONPB

-- * Helper types and functions

-- | A key-value pair which also designates whether or not the value component
-- is a protobuf default value or not. Produced by '(.=)' and consumed by
-- 'fieldsPB'.
data FieldPB = FieldPB Bool A.Series

-- | @fieldsPB emitDefaults flds@ encodes the given key-value pair fields.  If
-- @emitDefaults@ is @True@, all values will be emitted, otherwise fields with a
-- default protobuf value are omitted from the encoding.
fieldsPB :: Bool -> [FieldPB] -> A.Encoding
fieldsPB emitDefaults flds = E.pairs (mconcat (fmap emit flds))
  where
    emit (FieldPB isDflt kvp)
      | not emitDefaults && isDflt = mempty
      | otherwise                  = kvp

-- TODO: sanity check field prefix modification; these functions are no longer
-- being called so we likely dropped this piece somewhere along the way.

dropFldPfx :: Named a => DP.Proxy a -> String -> String
dropFldPfx p s = case dropNamed s of [] -> []; (c:cs) -> toLower c : cs
  where
    dropNamed = drop (length (nameOf p :: String))

pbOpts :: (Named a) => DP.Proxy a -> A.Options
pbOpts p = A.defaultOptions{ A.fieldLabelModifier = dropFldPfx p }
