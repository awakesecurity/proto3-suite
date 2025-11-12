{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Support for the "JSONPB" canonical JSON encoding described at
-- <https://developers.google.com/protocol-buffers/docs/proto3#json>.
--
-- This modules provides 'Data.Aeson'-like helper functions, typeclasses, and
-- instances for converting to and from values of types which have a JSONPB
-- representation and equivalent underlying 'Data.Aeson' representations.
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
--   toJSONPB (Scalar32 i32 u32 s32 f32 sf32) = object
--       [ "i32"  .= i32
--       , "u32"  .= u32
--       , "s32"  .= s32
--       , "f32"  .= f32
--       , "sf32" .= sf32
--       ]
--   toEncodingPB (Scalar32 i32 u32 s32 f32 sf32) = pairs
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
                                                        ToJSON1(..), FromJSON1(..),
                                                        ToJSONKey(..),
                                                        decode, eitherDecode,
                                                        (.!=))
import qualified Data.Aeson.Encoding              as E
import qualified Data.Aeson.Encoding.Internal     as E
#if !MIN_VERSION_aeson(2,1,0)
import qualified Data.Aeson.Internal              as A (formatError, iparse)
#endif
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key                   as A
#endif
import qualified Data.Aeson.Parser                as A (eitherDecodeWith, json)
import qualified Data.Aeson.Types                 as A (Object, Pair, Parser,
                                                        Series,
                                                        explicitParseField,
                                                        explicitParseFieldMaybe,
#if MIN_VERSION_aeson(2,1,0)
                                                        formatError,
                                                        iparse,
#endif
                                                        object,
#if !(MIN_VERSION_aeson(2,0,2))
                                                        toJSONKeyText,
#endif
                                                        typeMismatch,)
import qualified Data.Attoparsec.ByteString       as Atto (skipWhile)
import qualified Data.Attoparsec.ByteString.Char8 as Atto (Parser, endOfInput)
import qualified Data.Binary.Builder              as Builder
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Lazy             as LBS
import           Data.Maybe
import qualified Data.Map                         as M
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TL
import qualified Data.Text.Short                  as TS
import qualified Data.Vector                      as V
import           GHC.Exts                         (Proxy#, proxy#)
import           GHC.Generics                     (Generic)
import           GHC.Int                          (Int32, Int64)
import           GHC.Word                         (Word32, Word64)
import           Google.Protobuf.Wrappers.Polymorphic (Wrapped(..))
import           Proto3.Suite.Class               (HasDefault (def, isDefault),
                                                   Named (nameOf))
import           Proto3.Suite.Types               (Enumerated(..), Fixed(..),
                                                   ForceEmit(..), Nested(..),
                                                   NestedVec(..), PackedVec(..),
                                                   Signed(..), UnpackedVec(..))
import qualified Proto3.Suite.Types
import           Proto3.Wire.Class                (ProtoEnum(..))
import           Test.QuickCheck.Arbitrary        (Arbitrary(..))

#if MIN_VERSION_aeson(2,0,0)
type Key = A.Key
keyFromText :: Text -> Key
keyFromText = A.fromText
#else
type Key = Text
keyFromText :: Text -> Text
keyFromText = id
#endif

-- * Typeclass definitions

-- | 'A.ToJSON' variant for JSONPB direct encoding via 'A.Encoding'
class ToJSONPB a where
  -- | 'A.toJSON' variant for JSONPB encoders.
  toJSONPB :: a -> Options -> A.Value

  -- | 'A.toEncoding' variant for JSONPB encoders. If an implementation is not
  -- provided, uses 'toJSONPB' (which is less efficient since it indirects
  -- through the 'A.Value' IR).
  toEncodingPB :: a -> Options -> A.Encoding
  toEncodingPB x = A.toEncoding . toJSONPB x

instance ToJSONPB A.Value where
  toJSONPB v _ = v
  toEncodingPB v _ = E.value v

instance ToJSONPB A.Encoding where
  toJSONPB e _ = fromMaybe A.Null . A.decode . Builder.toLazyByteString . E.fromEncoding $ e
  toEncodingPB e _ = e

-- | 'A.FromJSON' variant for JSONPB decoding from the 'A.Value' IR
class FromJSONPB a where
  -- | 'A.parseJSON' variant for JSONPB decoders.
  parseJSONPB :: A.Value -> A.Parser a

instance FromJSONPB A.Value where
  parseJSONPB = pure

-- | JSONPB format shortcuts Google wrappers types.
deriving newtype instance FromJSONPB a => FromJSONPB (Wrapped a)

-- | JSONPB format shortcuts Google wrappers types.
deriving newtype instance ToJSONPB a => ToJSONPB (Wrapped a)

-- * JSONPB codec entry points

-- | 'Data.Aeson.encode' variant for serializing a JSONPB value as a lazy
-- 'LBS.ByteString'.
encode :: ToJSONPB a => Options -> a -> LBS.ByteString
encode opts x = E.encodingToLazyByteString (toEncodingPB x opts)
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

-- | JSONPB-encoded monoidal key-value pairs
class Monoid m => KeyValuePB m where
  pair :: ToJSONPB v => Text -> v -> Options -> m

instance KeyValuePB A.Series where pair k v opts = E.pair (keyFromText k) (toEncodingPB v opts)
instance KeyValuePB [A.Pair] where pair k v opts = pure (keyFromText k, toJSONPB v opts)

-- | Construct a monoidal key-value pair, using 'mempty' to represent omission
-- of default values (unless the given 'Options' force their emission).
(.=) :: (HasDefault v, ToJSONPB v, KeyValuePB kvp) => Text -> v -> Options -> kvp
k .= v = mk
  where
    mk opts@Options{..}
      | not optEmitDefaultValuedFields && isDefault v
        = mempty
      | otherwise
        = pair k v opts


-- | 'Data.Aeson..:' variant for JSONPB; if the given key is missing from the
-- object, or if it is present but its value is null, we produce the default
-- protobuf value for the field type
(.:) :: (FromJSONPB a, HasDefault a) => A.Object -> Text -> A.Parser a
obj .: key = obj .:? keyFromText key A..!= def
  where
    (.:?) = A.explicitParseFieldMaybe parseJSONPB

parseField :: FromJSONPB a => A.Object -> Key -> A.Parser a
parseField = A.explicitParseField parseJSONPB

-- | >>> isDefault (def :: E.Encoding)
-- True
instance HasDefault E.Encoding where
  def       = E.empty
  isDefault = E.nullEncoding

-- | >>> isDefault (def :: A.Value)
-- True
instance HasDefault A.Value where
  def       = A.Null
  isDefault = (== A.Null)

-- * JSONPB rendering and parsing options

data Options = Options
  { optEmitDefaultValuedFields :: Bool
  , optEmitNamedOneof :: Bool
  -- ^ For compatibility with the Go, C++, and Python JSONPB implementations.
  --
  -- If 'False', the following message
  --
  -- > message MyMessage {
  -- >   oneof animal {
  -- >     Cat cat = 1;
  -- >     Dog dog = 2;
  -- >   }
  -- > }
  --
  -- will be serialized as
  --
  -- > MyMessage (Animal (Cat "Simba")) => { "cat": "Simba" }
  --
  -- instead of
  --
  -- > MyMessage (Animal (Cat "Simba")) => { "animal": { "cat": "Simba" } }
  --
  } deriving (Eq, Generic, Show)

instance Arbitrary Options where
  arbitrary = Options <$> arbitrary <*> arbitrary

-- | Default options for JSON encoding. By default, all options are @True@.
defaultOptions :: Options
defaultOptions = Options
  { optEmitDefaultValuedFields = True
  , optEmitNamedOneof = True
  }

-- | Options for JSONPB encoding.
jsonPBOptions :: Options
jsonPBOptions = Options
  { optEmitDefaultValuedFields = False
  , optEmitNamedOneof = False
  }

-- * Helper types and functions

dropNamedPrefix :: Named a => Proxy# a -> String -> String
dropNamedPrefix p = drop (length (nameOf p :: String))

object :: [Options -> [A.Pair]] -> Options -> A.Value
object fs = A.object . mconcat fs

-- | As 'object', but produces 'A.Null' when there are no pairs to wrap (cf. the
-- empty object result of 'object)
--
-- >>> object [const []] defaultOptions
-- Object (fromList [])
--
-- >>> objectOrNull [const []] defaultOptions
-- Null
objectOrNull :: [Options -> [A.Pair]] -> Options -> A.Value
objectOrNull fs options = case mconcat fs options of
  []       -> A.Null
  nonEmpty -> A.object nonEmpty

pairs :: [Options -> A.Series] -> Options -> E.Encoding
pairs fs = E.pairs . mconcat fs

-- | As 'pairs', but produces the "null" when there is no series to encode
-- (cf. the empty object encoding of 'pairs')
--
-- >>> pairs [const mempty] defaultOptions
-- "{}"
-- >>> pairsOrNull [const mempty] defaultOptions
-- "null"
pairsOrNull :: [Options -> A.Series] -> Options -> E.Encoding
pairsOrNull fs options = case mconcat fs options of
  E.Empty  -> E.null_
  nonEmpty -> E.pairs nonEmpty

enumFieldString :: forall a. (Named a, Show a) => a -> A.Value
enumFieldString = A.String . T.pack . dropNamedPrefix (proxy# :: Proxy# a) . show

enumFieldEncoding :: forall a. (Named a, Show a) => a -> A.Encoding
enumFieldEncoding = E.string . dropNamedPrefix (proxy# :: Proxy# a) . show

-- | A 'Data.Aeson' 'A.Value' encoder for values which can be
-- JSONPB-encoded.
toAesonValue :: ToJSONPB a => a -> A.Value
toAesonValue = flip toJSONPB defaultOptions

-- | A direct 'A.Encoding' for values which can be JSONPB-encoded.
toAesonEncoding :: ToJSONPB a => a -> A.Encoding
toAesonEncoding = flip toEncodingPB defaultOptions

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

-- * Common instances for jsonpb codec implementations

-- ** Instances for scalar types

--------------------------------------------------------------------------------
-- Boolean scalar type

instance ToJSONPB Bool where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding

instance FromJSONPB Bool where
  parseJSONPB = A.parseJSON

--------------------------------------------------------------------------------
-- Integer scalar types
--
--   * 32 bit integer values render to JSON decimal numbers; either numbers or
--     strings are accepted.
--
--   * 64 bit integer values render to JSON decimal strings; either numbers
--     or strings are accepted.
--

-- int32 / sint32 / sfixed32
instance ToJSONPB Int32 where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding

instance FromJSONPB Int32 where
  parseJSONPB = parseNumOrDecimalString "int32 / sint32"

-- uint32 / fixed32
instance ToJSONPB Word32 where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding

instance FromJSONPB Word32 where
  parseJSONPB = parseNumOrDecimalString "uint32"

-- int64 / sint64 / sfixed64
instance ToJSONPB Int64 where
  toJSONPB x _     = A.String . T.pack . show $ x
  toEncodingPB x _ = E.string (show x)
instance FromJSONPB Int64 where
  parseJSONPB = parseNumOrDecimalString "int64 / sint64"

-- unit64 / fixed64
instance ToJSONPB Word64 where
  toJSONPB x _     = A.String . T.pack . show $ x
  toEncodingPB x _ = E.string (show x)
instance FromJSONPB Word64 where
  parseJSONPB = parseNumOrDecimalString "int64 / sint64"

-- Distinctions between varint and fixed-width formats do not matter to JSONPB.
deriving newtype instance FromJSONPB a => FromJSONPB (Fixed a)
deriving newtype instance ToJSONPB a => ToJSONPB (Fixed a)

-- Zig-zag encoding issues do not matter to JSONPB.
deriving newtype instance FromJSONPB a => FromJSONPB (Signed a)
deriving newtype instance ToJSONPB a => ToJSONPB (Signed a)

--------------------------------------------------------------------------------
-- Floating point scalar types
--
-- JSON value will be a number or one of the special string values "NaN",
-- "Infinity", and "-Infinity". Either numbers or strings are accepted. Exponent
-- notation is also accepted.

-- float
instance ToJSONPB Float where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding

instance FromJSONPB Float where
  parseJSONPB = parseFP "float"

-- double
instance ToJSONPB Double where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding
instance FromJSONPB Double where
  parseJSONPB = parseFP "double"

--------------------------------------------------------------------------------
-- Stringly types (string and bytes)

-- string
instance ToJSONPB T.Text where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding
instance FromJSONPB T.Text where
  parseJSONPB = A.parseJSON

instance ToJSONPB TL.Text where
  toJSONPB     = const . A.toJSON
  toEncodingPB = const . A.toEncoding
instance FromJSONPB TL.Text where
  parseJSONPB = A.parseJSON

instance ToJSONPB TS.ShortText where
  toJSONPB     = toJSONPB     . TS.toText
  toEncodingPB = toEncodingPB . TS.toText
instance FromJSONPB TS.ShortText where
  parseJSONPB = fmap TS.fromText . A.parseJSON

deriving newtype instance ToJSONPB a => ToJSONPB (Proto3.Suite.Types.String a)
deriving newtype instance FromJSONPB a => FromJSONPB (Proto3.Suite.Types.String a)

-- bytes

bsToJSONPB :: BS.ByteString -> A.Value
bsToJSONPB (T.decodeUtf8' . B64.encode -> ebs) = case ebs of
  Right bs -> A.toJSON bs
  Left e   -> error ("internal: failed to encode B64-encoded bytestring: " ++ show e)
              -- NB: T.decodeUtf8' should never fail because we B64-encode the
              -- incoming bytestring.

instance ToJSONPB BS.ByteString where
  toJSONPB bs _        = bsToJSONPB bs
  toEncodingPB bs opts = E.value (toJSONPB bs opts)

instance FromJSONPB BS.ByteString where
  parseJSONPB (A.String b64enc) = pure . B64.decodeLenient . T.encodeUtf8 $ b64enc
  parseJSONPB v                 = A.typeMismatch "bytes" v

deriving newtype instance ToJSONPB a => ToJSONPB (Proto3.Suite.Types.Bytes a)
deriving newtype instance FromJSONPB a => FromJSONPB (Proto3.Suite.Types.Bytes a)

--------------------------------------------------------------------------------
-- Enumerated types

enumToJSONPB :: (e -> Options -> a) -- ^ JSONPB encoder function to use
             -> (Int32 -> a)        -- ^ handles out-of-range enums
             -> Enumerated e        -- ^ the enumerated value to encode
             -> Options             -- ^ JSONPB encoding options
             -> a                   -- ^ the JSONPB-encoded value
enumToJSONPB enc outOfRange (Enumerated e) opts =
  either outOfRange (\input -> enc input opts) e

-- | If you ask for an unrecognized enumerator code to be emitted, then this
-- instance will emit it numerically, relying upon parser behavior required by:
-- <https://developers.google.com/protocol-buffers/docs/proto3#json>
instance ToJSONPB e => ToJSONPB (Enumerated e) where
  toJSONPB     = enumToJSONPB toJSONPB (A.Number . fromIntegral)
  toEncodingPB = enumToJSONPB toEncodingPB E.int32

-- | Supports both names and integer codes, as required by:
-- <https://developers.google.com/protocol-buffers/docs/proto3#json>
instance (ProtoEnum e, FromJSONPB e) => FromJSONPB (Enumerated e) where
  -- In order to reduce the amount of generated Haskell code we delegate to
  -- such code only in the String case, and when converting 'Int32' to @e@.
  -- The rest of the parser we implement here, generically.
  parseJSONPB A.Null = pure def
  parseJSONPB (A.String s) = Enumerated . Right <$> parseJSONPB (A.String s)
  parseJSONPB (A.Number n) = fromCode <$> parseJSONPB (A.Number n)
    where
      fromCode c = Enumerated $ maybe (Left c) Right (toProtoEnumMay c)
  parseJSONPB v = fail $ "Expected enumerator name or code, not: " ++ show v

-- ** Instances for composite types

--------------------------------------------------------------------------------
-- Instances for repeated messages
--
-- JSON value will be the vector elements encoded as a JSON array. The null
-- value is accepted as the empty list, @[]@.

instance ToJSONPB a => ToJSONPB (V.Vector a) where
  toJSONPB v opts     = A.Array (V.map (\x -> toJSONPB x opts) v)
  toEncodingPB v opts = E.list (\x -> toEncodingPB x opts) (V.toList v)
instance FromJSONPB a => FromJSONPB (V.Vector a) where
  parseJSONPB (A.Array vs) = mapM parseJSONPB vs
  parseJSONPB A.Null       = pure []
  parseJSONPB v            = A.typeMismatch "repeated" v

-- Packed/unpacked distinctions do not matter to JSONPB.
deriving via (V.Vector a) instance FromJSONPB a => FromJSONPB (NestedVec a)
deriving via (V.Vector a) instance ToJSONPB a => ToJSONPB (NestedVec a)
deriving via (V.Vector a) instance FromJSONPB a => FromJSONPB (PackedVec a)
deriving via (V.Vector a) instance ToJSONPB a => ToJSONPB (PackedVec a)
deriving via (V.Vector a) instance FromJSONPB a => FromJSONPB (UnpackedVec a)
deriving via (V.Vector a) instance ToJSONPB a => ToJSONPB (UnpackedVec a)

--------------------------------------------------------------------------------
-- Instances for nested messages

instance ToJSONPB a => ToJSONPB (Maybe a) where
  toJSONPB mx opts     = maybe A.Null (\x -> toJSONPB x opts) mx
  toEncodingPB mx opts = maybe E.null_ (\x -> toEncodingPB x opts) mx
instance FromJSONPB a => FromJSONPB (Maybe a) where
  parseJSONPB A.Null = pure Nothing
  parseJSONPB v      = fmap Just (parseJSONPB v)

deriving via (Maybe a) instance FromJSONPB a => FromJSONPB (Nested a)
deriving via (Maybe a) instance ToJSONPB a => ToJSONPB (Nested a)

--------------------------------------------------------------------------------
-- Instances for optional fields

deriving newtype instance FromJSONPB a => FromJSONPB (ForceEmit a)
deriving newtype instance ToJSONPB a => ToJSONPB (ForceEmit a)

--------------------------------------------------------------------------------
-- Instances for map

deriving newtype instance A.FromJSONKey a => A.FromJSONKey (Fixed a)
deriving newtype instance A.ToJSONKey a => A.ToJSONKey (Fixed a)

deriving newtype instance A.FromJSONKey a => A.FromJSONKey (Signed a)
deriving newtype instance A.ToJSONKey a => A.ToJSONKey (Signed a)

deriving via T.Text instance A.FromJSONKey (Proto3.Suite.Types.String T.Text)
deriving via T.Text instance A.ToJSONKey (Proto3.Suite.Types.String T.Text)

deriving via TL.Text instance A.FromJSONKey (Proto3.Suite.Types.String TL.Text)
deriving via TL.Text instance A.ToJSONKey (Proto3.Suite.Types.String TL.Text)

#if MIN_VERSION_aeson(2,0,2)

deriving via TS.ShortText instance A.FromJSONKey (Proto3.Suite.Types.String TS.ShortText)
deriving via TS.ShortText instance A.ToJSONKey (Proto3.Suite.Types.String TS.ShortText)

#else

instance A.FromJSON (Proto3.Suite.Types.String TS.ShortText) where
  parseJSON = fmap (Proto3.Suite.Types.String . TS.fromText) . A.parseJSON

instance A.FromJSONKey (Proto3.Suite.Types.String TS.ShortText) where
  fromJSONKey = A.FromJSONKeyText (Proto3.Suite.Types.String . TS.fromText)

instance A.ToJSON (Proto3.Suite.Types.String TS.ShortText) where
  toJSON = A.toJSON . TS.toText . Proto3.Suite.Types.string
  toEncoding = A.toEncoding . TS.toText . Proto3.Suite.Types.string

instance A.ToJSONKey (Proto3.Suite.Types.String TS.ShortText) where
  toJSONKey = A.toJSONKeyText (TS.toText . Proto3.Suite.Types.string)

#endif

instance (A.ToJSONKey k, ToJSONPB k, ToJSONPB v) => ToJSONPB (M.Map k v) where
  toJSONPB m opts = A.liftToJSON @(M.Map k)
#if MIN_VERSION_aeson(2,2,0)
                      (const False)  -- Unless and until we test for the protobuf default value.
#endif
                         (`toJSONPB` opts) (A.Array . V.fromList . map (`toJSONPB` opts)) m
  toEncodingPB m opts = A.liftToEncoding @(M.Map k)
#if MIN_VERSION_aeson(2,2,0)
                          (const False)  -- Unless and until we test for the protobuf default value.
#endif
                            (`toEncodingPB` opts) (E.list (`toEncodingPB` opts)) m

instance (Ord k, A.FromJSONKey k, FromJSONPB k, FromJSONPB v) => FromJSONPB (M.Map k v) where
  parseJSONPB = A.liftParseJSON @(M.Map k)
#if MIN_VERSION_aeson(2,2,0)
                  Nothing  -- Unless and until we decide to use the protobuf default value.
#endif
                    parseJSONPB parseList
    where
      parseList (A.Array a) = traverse parseJSONPB (V.toList a)
      parseList v = A.typeMismatch "not a list" v
