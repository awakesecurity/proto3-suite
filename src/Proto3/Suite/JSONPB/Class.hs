{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Support for the "JSONPB" canonical JSON encoding described at
-- https://developers.google.com/protocol-buffers/docs/proto3#json.
--
-- This modules provides 'Data.Aeson'-like helper functions and typeclasses for
-- converting to and from values of types which have a JSONPB representation and
-- equivalent underlying 'Data.Aeson' representations.
--
-- This module also presents a (very minimal) surface syntax for Aeson-like
-- operations; the idea is that we can write 'ToJSONPB' and 'FromJSONPB'
-- instances in a very similar manner to 'A.ToJSON' and 'A.FromJSON' instances,
-- except that doing so specifies JSONPB codecs instead of vanilla JSON codecs.
--
-- Example use:
--
-- @
-- message Scalar32 {
--   int32     i32 = 1;
--   uint32    u32 = 2;
--   sint32    s32 = 3;
--   fixed32   f32 = 4;
--   sfixed32 sf32 = 5;
-- }
--
-- instance ToJSONPB Scalar32 where
--   toEncodingPB opts (Scalar32 i32 u32 s32 f32 sf32) = fieldsPB opts
--       [ "i32"  .= i32
--       , "u32"  .= u32
--       , "s32"  .= s32
--       , "f32"  .= f32
--       , "sf32" .= sf32
--       ]
--
-- instance FromJSONPB Scalar32 where
--   parseJSONPB = withObject "Scalar32" $ \obj ->
--     pure Scalar32
--     <*> obj .: "i32"
--     <*> obj .: "u32"
--     <*> obj .: "s32"
--     <*> obj .: "f32"
--     <*> obj .: "sf32"
-- @

module Proto3.Suite.JSONPB.Class where

import qualified Data.Aeson                       as A (Encoding, FromJSON (..),
                                                        FromJSONKey (..),
                                                        FromJSONKeyFunction (..),
                                                        ToJSON (..), Value (..),
                                                        eitherDecode, json,
                                                        (.!=))
import qualified Data.Aeson.Encoding              as E
import qualified Data.Aeson.Internal              as A (formatError, iparse)
import qualified Data.Aeson.Parser                as A (eitherDecodeWith)
import qualified Data.Aeson.Types                 as A (Object, Parser, Series,
                                                        explicitParseFieldMaybe,
                                                        typeMismatch)
import qualified Data.Attoparsec.ByteString       as Atto (skipWhile)
import qualified Data.Attoparsec.ByteString.Char8 as Atto (Parser, endOfInput)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Proxy
import           Data.Text                        (Text)
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TL
import           Proto3.Suite.Class               (HasDefault (def, isDefault),
                                                   Named (nameOf))

-- * Typeclass definitions

-- | 'A.ToJSON' variant for JSONPB direct encoding via 'A.Encoding'
class ToJSONPB a where
  -- | 'A.toEncoding' variant for JSONPB encoders. Equivalent to 'A.toEncoding'
  -- if an implementation is not provided.
  toEncodingPB :: Options -> a -> A.Encoding
  default toEncodingPB :: (A.ToJSON a) => Options -> a -> A.Encoding
  toEncodingPB _ = A.toEncoding

-- | 'A.FromJSON' variant for JSONPB decoding from the 'A.Value' IR
class FromJSONPB a where
  -- | 'A.parseJSON' variant for JSONPB decoders. Equivalent to 'A.parseJSON' if
  -- an implementation is not provided.
  parseJSONPB :: A.Value -> A.Parser a

  default parseJSONPB :: (A.FromJSON a) => A.Value -> A.Parser a
  parseJSONPB = A.parseJSON

-- * JSONPB codec entry points

-- | 'Data.Aeson.encode' variant for serializing a JSONPB value as a lazy
-- 'LBS.ByteString'.
encode :: ToJSONPB a => Options -> a -> LBS.ByteString
encode opts = E.encodingToLazyByteString . toEncodingPB opts
{-# INLINE encode #-}

-- | 'Data.Aeson.eitherDecode' variant for deserializing a JSONPB value from a
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

-- | Construct a (field name, JSONPB-encoded value) tuple
(.=) :: (HasDefault v, ToJSONPB v) => Text -> v -> FieldPB
k .= v = FieldPB (isDefault v) (\opts -> E.pair k (toEncodingPB opts v))

-- | 'Data.Aeson..:' variant for JSONPB; if the given key is missing from the
-- object, or if it is present but its value is null, we produce the default
-- protobuf value for the field type
(.:) :: (FromJSONPB a, HasDefault a) => A.Object -> Text -> A.Parser a
obj .: key = obj .:? key A..!= def
  where
    (.:?) = A.explicitParseFieldMaybe parseJSONPB

-- * JSONPB rendering and parsing options

data Options = Options
  { optEmitDefaultValuedFields :: Bool
  }
  deriving Show

-- | Default options for JSONPB encoding. By default, all options are @False@.
defaultOptions :: Options
defaultOptions = Options
  { optEmitDefaultValuedFields = False
  }

-- * Helper types and functions

dropNamedPfx :: Named a => Proxy a -> String -> String
dropNamedPfx p = drop (length (nameOf p :: String))

-- | An internal ~(field name, value) tuple (using 'E.pair' as the underlying
-- tuple) which carries additional useful state.
--
-- Values of this type are produced by '.='
--
-- Values of this type are consumed by 'fieldsPB'
--
data FieldPB = FieldPB
  { fieldIsDefaultValued :: Bool
    -- ^ Whether or not this field is "default-valued"; i.e., whether the value
    -- component of the key-value pair is the protobuf default value for its
    -- type
  , fieldTuple           :: Options -> A.Series
    -- ^ Produce the JSONPB-encoded key-value tuple
  }

-- | @fieldsPB opts flds@ encodes the given JSONPB-encoded fields
fieldsPB :: Options -> [FieldPB] -> A.Encoding
fieldsPB opts@Options{..} fields = E.pairs (mconcat (fmap emit fields))
  where
    emit FieldPB{..}
      | not optEmitDefaultValuedFields && fieldIsDefaultValued
        = mempty
      | otherwise
        = fieldTuple opts

namedEncoding :: forall e. (Named e, Show e) => e -> A.Encoding
namedEncoding = E.string . dropNamedPfx (Proxy :: Proxy e) . show

-- | Parse a JSONPB floating point value; first parameter provides context for
-- type mismatches
parseFP :: (A.FromJSON a, A.FromJSONKey a) => String -> A.Value -> A.Parser a
parseFP tyDesc v = case v of
  A.Number{} -> A.parseJSON v
  A.String t -> case A.fromJSONKey of
                  A.FromJSONKeyTextParser p
                    -> p t
                  _ -> fail "internal: parseKeyPB: unexpected FromJSONKey summand"
  _          -> A.typeMismatch tyDesc v

-- | Liberally parse an integer value (e.g. 42 or "42" as 42); first parameter
-- provides context for type mismatches
parseNumOrDecimalString :: (A.FromJSON a) => String -> A.Value -> A.Parser a
parseNumOrDecimalString tyDesc v = case v of
  A.Number{} -> A.parseJSON v
  A.String t -> either fail pure . A.eitherDecode . TL.encodeUtf8 . TL.fromStrict $ t
  _          -> A.typeMismatch tyDesc v
