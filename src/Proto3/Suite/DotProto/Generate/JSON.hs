-- | This module provides functions to generate Haskell code for doing JSON
-- serdes for protobuf messages, as per the proto3 canonical JSON encoding
-- described at https://developers.google.com/protocol-buffers/docs/proto3#json.

{-

.d8888.  .o88b. d8888b.  .d8b.  d888888b  .o88b. db   db       .o88b.  .d88b.  d8888b. d88888b
88'  YP d8P  Y8 88  `8D d8' `8b `~~88~~' d8P  Y8 88   88      d8P  Y8 .8P  Y8. 88  `8D 88'
`8bo.   8P      88oobY' 88ooo88    88    8P      88ooo88      8P      88    88 88   88 88ooooo
  `Y8b. 8b      88`8b   88~~~88    88    8b      88~~~88      8b      88    88 88   88 88~~~~~
db   8D Y8b  d8 88 `88. 88   88    88    Y8b  d8 88   88      Y8b  d8 `8b  d8' 88  .8D 88.
`8888Y'  `Y88P' 88   YD YP   YP    YP     `Y88P' YP   YP       `Y88P'  `Y88P'  Y8888D' Y88888P

Currently this module just contains a bunch of experimental code -- mostly
prototyping for the kind of code that we'll want to end up generating.

-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Proto3.Suite.DotProto.Generate.JSON where

-- Stuff we need for experimentation. Once we have the whole import set and
-- decide how to namespace it, we will need to carry these additional imports
-- over into the code generators.
import Proto3.Suite.DotProto
import Proto3.Suite.DotProto.Generate
import qualified Data.Map as Hs (fromList)
import qualified Proto3.Suite as P3S
import qualified Proto3.Suite.DotProto.Rendering as P3S
import qualified Data.Proxy as DP
import qualified Proto3.Wire.Encode as Enc
import qualified Proto3.Wire.Decode as Dec
import qualified Control.Monad as Hs (fail)
import qualified Data.Attoparsec.ByteString.Char8 as Atto (Parser, endOfInput)
import qualified Data.Attoparsec.ByteString       as Atto (skipWhile)
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A (eitherDecodeWith)
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Internal as A (IResult(..), iparse, formatError)
import qualified Data.Aeson.Encoding as E
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Safe
import           Data.Monoid ((<>))
import qualified Data.Char as Hs (toLower)
import Prelude
import Data.Coerce
import Debug.Trace (trace)

-- Imports from the generated code below, lifted here for convenience.
import qualified Prelude as Hs
import qualified Prelude as Prelude
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Data.Text as Hs (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Hs (encodeUtf8, decodeUtf8')
import qualified Data.ByteString as Hs
import qualified Data.ByteString.Lazy as LBS
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Vector as V
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import GHC.Generics as Hs
import GHC.Enum as Hs
import Debug.Trace

-- Import the generated code from JSON.proto. This can be regenerated via e.g.:
--   $ compile-proto-file --proto src/Proto3/Suite/DotProto/Generate/JSON.proto > src/Proto3/Suite/DotProto/Generate/JSONPBProto.hs
import Proto3.Suite.DotProto.Generate.JSONPBProto

-- $setup
--
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings

--------------------------------------------------------------------------------
-- Begin hand-generated instances for JSON PB renderings; these instances will
-- be generated once their design is finalized, and live in the same module as
-- the other typedefs and instances (e.g.,
-- Proto3.Suite.DotProto.Generate.JSONPBProto in this case).
--
-- We also put some placeholder doctests here for prelim regression checking
-- until we get some property-based tests in place.

-- | Scalar32
--
-- >>> roundTrip (Scalar32 32 33 (-34) 35 36)
-- Right True
--
-- >>> pbToJSON (Scalar32 32 33 (-34) 35 36)
-- "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
--
-- >>> Right (Scalar32 32 33 (-34) 35 36) == jsonToPB "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
-- True

-- TODO: Use RecordWildCards to name the fields instead of an explicit pattern match

instance ToJSONPB Scalar32 where
  toJSONPB (Scalar32 i32 u32 s32 f32 sf32) = A.object . mconcat $
    [ "i32"  .= i32
    , "u32"  .= u32
    , "s32"  .= s32
    , "f32"  .= f32
    , "sf32" .= sf32
    ]
  toEncodingPB (Scalar32 i32 u32 s32 f32 sf32) = A.pairs . mconcat $
    [ "i32"  .= i32
    , "u32"  .= u32
    , "s32"  .= s32
    , "f32"  .= f32
    , "sf32" .= sf32
    ]
instance FromJSONPB Scalar32 where
  parseJSONPB = A.withObject "Scalar32" $ \obj ->
    pure Scalar32
    <*> obj .: "i32"
    <*> obj .: "u32"
    <*> obj .: "s32"
    <*> obj .: "f32"
    <*> obj .: "sf32"

-- | Scalar64
--
-- >>> roundTrip (Scalar64 64 65 (-66) 67 68)
-- Right True
--
-- >>> pbToJSON (Scalar64 64 65 (-66) 67 68)
-- "{\"i64\":\"64\",\"u64\":\"65\",\"s64\":\"-66\",\"f64\":\"67\",\"sf64\":\"68\"}"
--
-- >>> Right (Scalar64 64 65 (-66) 67 68) == jsonToPB "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}"
-- True
--
-- >>> Right (Scalar64 0 65 66 67 68) == jsonToPB "{\"u64\":\"65\",\"s64\":\"66\",\"f64\":\"67\",\"sf64\":\"68\"}"
-- True

instance ToJSONPB Scalar64 where
  toJSONPB (Scalar64 i64 u64 s64 f64 sf64) = A.object . mconcat $
    [ "i64"  .= i64
    , "u64"  .= u64
    , "s64"  .= s64
    , "f64"  .= f64
    , "sf64" .= sf64
    ]
  toEncodingPB (Scalar64 i64 u64 s64 f64 sf64) = A.pairs . mconcat $
    [ "i64"  .= i64
    , "u64"  .= u64
    , "s64"  .= s64
    , "f64"  .= f64
    , "sf64" .= sf64
    ]
instance FromJSONPB Scalar64 where
  parseJSONPB = A.withObject "Scalar64" $ \obj ->
    pure Scalar64
    <*> obj .: "i64"
    <*> obj .: "u64"
    <*> obj .: "s64"
    <*> obj .: "f64"
    <*> obj .: "sf64"

-- | ScalarFP
--
-- >>> roundTrip (ScalarFP 98.6 255.16)
-- Right True
--
-- >>> pbToJSON (ScalarFP 98.6 255.16)
-- "{\"f\":98.6,\"d\":255.16}"
--
-- >>> Right (ScalarFP 98.6 255.16) == jsonToPB "{\"f\":98.6,\"d\":255.16}"
-- True
--
-- >>> Right (ScalarFP 23.6 (-99.001)) == jsonToPB "{\"f\":\"23.6\",\"d\":\"-99.001\"}"
-- True
--
-- >>> Right (ScalarFP 1000000.0 (-1000.0)) == jsonToPB "{\"f\":\"1e6\",\"d\":\"-0.1e4\"}"
-- True
--
-- >>> Right (ScalarFP (1/0) (1/0)) == jsonToPB "{\"f\":\"Infinity\",\"d\":\"Infinity\"}"
-- True
--
-- >>> Right (ScalarFP (negate 1/0) (negate 1/0)) == jsonToPB "{\"f\":\"-Infinity\",\"d\":\"-Infinity\"}"
-- True
--
-- >>> jsonToPB "{\"f\":\"NaN\",\"d\":\"NaN\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = NaN, scalarFPD = NaN})

instance ToJSONPB ScalarFP where
  toJSONPB (ScalarFP f d) = A.object . mconcat $
    [ "f" .= f
    , "d" .= d
    ]
  toEncodingPB (ScalarFP f d) = A.pairs . mconcat $
    [ "f" .= f
    , "d" .= d
    ]
instance FromJSONPB ScalarFP where
  parseJSONPB = A.withObject "ScalarFP" $ \obj ->
    pure ScalarFP
    <*> obj .: "f"
    <*> obj .: "d"

-- | Stringly
--
-- >>> roundTrip (Stringly "foo" "abc123!?$*&()'-=@~")
-- Right True
--
-- >>> pbToJSON (Stringly "foo" "abc123!?$*&()'-=@~")
-- "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
--
-- >>> Right (Stringly "foo" "abc123!?$*&()'-=@~") == jsonToPB "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
-- True
--

instance ToJSONPB Stringly where
  toJSONPB (Stringly str bs) = A.object . mconcat $
    [ "str" .= str
    , "bs"  .= bs
    ]
  toEncodingPB (Stringly str bs) = A.pairs . mconcat $
    [ "str" .= str
    , "bs"  .= bs
    ]
instance FromJSONPB Stringly where
  parseJSONPB = A.withObject "Stringly" $ \obj ->
    pure Stringly
    <*> obj .: "str"
    <*> obj .: "bs"

-- | Repeat
--
-- >>> roundTrip (Repeat [4,5] [6,7])
-- Right True
--
-- >>> roundTrip (Repeat [] [6,7])
-- Right True
--
-- >>> roundTrip (Repeat [4,5] [])
-- Right True
--
-- >>> pbToJSON (Repeat [4,5] [6,7])
-- "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
--
-- >>> Right (Repeat [4,5] [6,7]) == jsonToPB "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
-- True
--
-- >>> Right (Repeat [4,5] []) == jsonToPB "{\"i32s\":[4,5]}"
-- True
--
-- >>> Right (Repeat [4,5] []) == jsonToPB "{\"i32s\":[4,5],\"i64s\":null}"
-- True
--
-- >>> Right (Repeat [4,5] []) == jsonToPB "{\"i32s\":[4,5],\"i64s\":[]}"
-- True
--
-- >>> Right (Repeat [] []) == jsonToPB "{}"
-- True

instance ToJSONPB Repeat where
  toJSONPB (Repeat i32s i64s) = A.object . mconcat $
    [ "i32s" .= i32s
    , "i64s" .= i64s
    ]
  toEncodingPB (Repeat i32s i64s) = A.pairs . mconcat $
    [ "i32s" .= i32s
    , "i64s" .= i64s
    ]
instance FromJSONPB Repeat where
  parseJSONPB = A.withObject "Repeat" $ \obj ->
    pure Repeat
    <*> obj .: "i32s"
    <*> obj .: "i64s"

-- | Nested
--
-- >>> roundTrip (Nested Nothing)
-- Right True
--
-- >>> roundTrip (Nested (Just (Nested_Inner 42)))
-- Right True
--
-- >>> pbToJSON (Nested Nothing)
-- "{}"
--
-- >>> pbToJSON (Nested (Just (Nested_Inner 42)))
-- "{\"nestedInner\":{\"i64\":\"42\"}}"
--
-- >>> Right (Nested Nothing) == jsonToPB "{}"
-- True
--
-- >>> Right (Nested (Just (Nested_Inner 42))) == jsonToPB "{\"nestedInner\":{\"i64\":\"42\"}}"
-- True
--

instance ToJSONPB Nested where
  toJSONPB (Nested minner) = A.object . mconcat $
    [ "nestedInner" .= minner
    ]
  toEncodingPB (Nested minner) = A.pairs . mconcat $
    [ "nestedInner" .= minner
    ]
instance FromJSONPB Nested where
  parseJSONPB = A.withObject "Nested" $ \obj ->
    pure Nested
    <*> obj .: "nestedInner"

-- Nested_Inner

instance ToJSONPB Nested_Inner where
  toJSONPB (Nested_Inner i64) = A.object . mconcat $
    [ "i64" .= i64
    ]
  toEncodingPB (Nested_Inner i64) = A.pairs . mconcat $
    [ "i64" .= i64
    ]
instance FromJSONPB Nested_Inner where
  parseJSONPB = A.withObject "Nested_Inner" $ \obj ->
    pure Nested_Inner
    <*> obj .: "i64"

-- End hand-generated instances for JSON PB renderings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Typeclasses for converting to and from values of types with a protobuf
-- representation and the aeson value IR which represents the "jsonpb" canonical
-- JSON encoding.

-- | 'A.ToJSON' variant for jsonpb encoding to the aeson IR
class ToJSONPB a where
  -- | 'A.toJSON' variant for jsonpb encoder implementations. Equivalent to
  -- 'A.toJSON' if an implementation is not provided.
  toJSONPB :: a -> A.Value

  default toJSONPB :: (A.ToJSON a) => a -> A.Value
  toJSONPB = A.toJSON

  -- | 'A.toEncoding' variant for jsonpb encoder implementations.
  toEncodingPB :: a -> A.Encoding
  toEncodingPB = E.value . toJSONPB

-- | 'A.FromJSON' variant for jsonpb decoding from the aeson IR
class FromJSONPB a where
  -- | 'A.parseJSON' variant for jsonpb decoder implementations. Equivalent to
  -- 'A.parseJSON' if an implementation is not provided.
  parseJSONPB :: A.Value -> A.Parser a

  default parseJSONPB :: (A.FromJSON a) => A.Value -> A.Parser a
  parseJSONPB = A.parseJSON

-- | 'A.KeyValue' variant for types in the 'ToJSONPB' typeclass
class KeyValuePB kv where
  (.=) :: (HsProtobuf.HasDefault v, ToJSONPB v) => Hs.Text -> v -> kv
  infixr 8 .=

--------------------------------------------------------------------------------
-- Instances for 32 bit integer primtypes: int32, sint32, uint32
--
-- JSON value will be a decimal number. Either numbers or strings are accepted.

-- FIXME: I don't think sint32 should overlap with this (possible bug in CG); no Signed wrapper is generated?
-- int32 / "sint32"
instance ToJSONPB Hs.Int32
instance FromJSONPB Hs.Int32 where parseJSONPB = parseNumOrDecimalString "int32 / sint32"

-- uint32
instance ToJSONPB Hs.Word32
instance FromJSONPB Hs.Word32 where parseJSONPB = parseNumOrDecimalString "uint32"

-- FIXME: As with sint32, CG might be buggy here for sfixed32; no Signed wrapper
-- is generated? NB: there are no HasDefault instances for (Fixed Int32) which
-- is another indication that this is a bug, because there IS a HasDefault
-- instance for (Signed (Fixed Int32)) which I think is what should be generated
-- here. As a stopgap we'll go ahead and define the instances for (Fixed Int32)
-- here so we can make progress but we should nuke these as soon as we fix CG.

-- TODO: delete me once CG is fixed
instance HsProtobuf.HasDefault (HsProtobuf.Fixed Hs.Int32)

--------------------------------------------------------------------------------
-- Instances for 64 bit integer primtypes: int64, sint64, uint64.
--
-- JSON value will be a decimal string. Either numbers or strings are accepted.

-- FIXME: same issue for sint64 as remarked above for sint32 (no Signed etc.)
-- int64 / "sint64"
instance ToJSONPB Hs.Int64   where toJSONPB    = showDecimalString
instance FromJSONPB Hs.Int64 where parseJSONPB = parseNumOrDecimalString "int64 / sint64"

-- TODO: Spec seems to imply that "-10" is a valid value for a uint64; wha-huh?
-- Cf. go impl, I suppose.
-- uint64
instance ToJSONPB Hs.Word64   where toJSONPB    = showDecimalString
instance FromJSONPB Hs.Word64 where parseJSONPB = parseNumOrDecimalString "uint64"

-- TODO: delete me once CG is fixed
instance HsProtobuf.HasDefault (HsProtobuf.Fixed Hs.Int64)

--------------------------------------------------------------------------------
-- Instances for fixed integer variants

-- fixed32, fixed64, "sfixed32", "sfixed64"
instance ToJSONPB a => ToJSONPB (HsProtobuf.Fixed a) where
  toJSONPB = toJSONPB . HsProtobuf.fixed
instance FromJSONPB a => FromJSONPB (HsProtobuf.Fixed a) where
  parseJSONPB = fmap HsProtobuf.Fixed . parseJSONPB

--------------------------------------------------------------------------------
-- Instances for floating point types (float, double)
--
-- JSON value will be a number or one of the special string values "NaN",
-- "Infinity", and "-Infinity". Either numbers or strings are accepted. Exponent
-- notation is also accepted.

parseFP :: (A.FromJSON a, A.FromJSONKey a) => String -> A.Value -> A.Parser a
parseFP tyDesc v = case v of
  A.Number{} -> A.parseJSON v
  A.String t -> case A.fromJSONKey of
                  A.FromJSONKeyTextParser p
                    -> p t
                  _ -> fail "internal: parseKeyPB: unexpected FromJSONKey summand"
  _          -> A.typeMismatch tyDesc v

instance ToJSONPB Hs.Float
instance FromJSONPB Hs.Float where parseJSONPB = parseFP "float"

instance ToJSONPB Hs.Double
instance FromJSONPB Hs.Double where parseJSONPB = parseFP "double"

--------------------------------------------------------------------------------
-- Instances for string
--

instance ToJSONPB Hs.Text
instance FromJSONPB Hs.Text

--------------------------------------------------------------------------------
-- Instances for bytes
--
-- JSON value will be the data encoded as a string using standard base64
-- encoding with paddings. Either standard or URL-safe base64 encoding
-- with/without paddings are accepted.

instance ToJSONPB Hs.ByteString where
  toJSONPB bs = case Hs.decodeUtf8' (B64.encode bs) of
    Left e -> error ("Failed to encode B64-encoded bytestring: " ++ show e)
               -- decodeUtf8' should never fail because we B64-encode the
               -- incoming bytestring, but we provide an explicit error here
               -- rather than using the partial decodeUf8.
    Right t -> A.toJSON t
instance FromJSONPB Hs.ByteString where
  parseJSONPB (A.String b64enc) = pure . B64.decodeLenient . Hs.encodeUtf8 $ b64enc
  parseJSONPB v                 = A.typeMismatch "bytes" v

--------------------------------------------------------------------------------
-- Instances for repeated
--
-- JSON value will be the vector elements encoded as a JSON array. The null
-- value is accepted as the empty list [].

-- TODO: move into Proto3.Suite.Class or somesuch?
instance HsProtobuf.HasDefault (Hs.Vector a) where
  def       = V.empty
  isDefault = V.null

instance ToJSONPB a => ToJSONPB (Hs.Vector a) where
  toJSONPB = A.Array . fmap toJSONPB
instance FromJSONPB a => FromJSONPB (Hs.Vector a) where
  parseJSONPB (A.Array vs) = mapM parseJSONPB vs
  parseJSONPB A.Null       = pure []
  parseJSONPB v            = A.typeMismatch "repeated" v

--------------------------------------------------------------------------------
-- Instances for nested messages
--
-- Generates JSON objects. Message field names are mapped to lowerCamelCase and
-- become JSON object keys. null is accepted and treated as the default value of
-- the corresponding field type.

instance ToJSONPB a => ToJSONPB (Maybe a) where
  toJSONPB = maybe A.Null toJSONPB
instance FromJSONPB a => FromJSONPB (Maybe a) where
  -- parseJSONPB :: A.Value -> A.Parser (Maybe a)
  parseJSONPB A.Null = pure Nothing
  parseJSONPB v      = fmap Just (parseJSONPB v)

-- TODO: move into Proto3.Suite.Class or somesuch?
instance HsProtobuf.HasDefault (Maybe a) where
  def = Nothing

  isDefault Nothing = True
  isDefault _       = False

--------------------------------------------------------------------------------
-- Helpers

instance KeyValuePB [A.Pair] where
  k .= v
    | HsProtobuf.isDefault v = mempty
    | otherwise              = [(k, toJSONPB v)]

instance KeyValuePB A.Series where
  k .= v
    | HsProtobuf.isDefault v = mempty
    | otherwise              = E.pair k (toEncodingPB v)

parseNumOrDecimalString :: (A.FromJSON a) => String -> A.Value -> A.Parser a
parseNumOrDecimalString tyDesc v = case v of
  A.Number{} -> A.parseJSON v
  A.String t -> either fail pure . A.eitherDecode . LBS.fromStrict . Hs.encodeUtf8 $ t
  _          -> A.typeMismatch tyDesc v

showDecimalString :: Show a => a -> A.Value
showDecimalString = A.String . Text.pack . Hs.show

-- | 'A..:' variant for jsonpb decoding; if the given key is missing from the
-- object, we use the default value for the field type.
(.:) :: (FromJSONPB a, HsProtobuf.HasDefault a) => A.Object -> Hs.Text -> A.Parser a
obj .: key = obj .:? key A..!= HsProtobuf.def
  where
    (.:?) = A.explicitParseFieldMaybe parseJSONPB

-- | 'A.encode' variant for serializing a JSONPB value as a lazy
-- 'LBS.ByteString'.
--
-- This is implemented in terms of the 'ToJSONPB' class's 'toEncodingPB' method.
encode :: ToJSONPB a => a -> LBS.ByteString
encode = E.encodingToLazyByteString . toEncodingPB
{-# INLINE encode #-}

-- | 'A.eitherDecode' variant for deserializing a JSONPB value from a lazy
-- 'LBS.ByteString'.
eitherDecode :: FromJSONPB a => LBS.ByteString -> Either String a
eitherDecode = eitherFormatError . A.eitherDecodeWith jsonEOF (A.iparse parseJSONPB)
  where
    eitherFormatError = either (Left . uncurry A.formatError) Right
    {-# INLINE eitherFormatError #-}

    -- Cribbed from aeson-1.1.1.0:Data.Aeson.Parser.Internal.jsonEOF, which is
    -- not exported. It's simple, so we just inline it here. Might be worth
    -- submitting a PR to export this.
    jsonEOF :: Atto.Parser A.Value
    jsonEOF = A.json <* skipSpace <* Atto.endOfInput
      where
        skipSpace :: Atto.Parser ()
        skipSpace = Atto.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
        {-# INLINE skipSpace #-}
{-# INLINE eitherDecode #-}

pbToJSON :: ToJSONPB a => a -> LBS.ByteString
pbToJSON = encode

jsonToPB :: FromJSONPB a => LBS.ByteString -> Hs.Either Hs.String a
jsonToPB = eitherDecode

roundTrip :: (ToJSONPB a, FromJSONPB a, Eq a) => a -> Either String Bool
roundTrip x = either Left (Right . (x==)) . jsonToPB . pbToJSON $ x

dropFldPfx :: HsProtobuf.Named a => DP.Proxy a -> Hs.String -> Hs.String
dropFldPfx p s = case dropNamed s of [] -> []; (c:cs) -> Hs.toLower c : cs
  where
    dropNamed = drop (length (HsProtobuf.nameOf p :: String))

pbOpts :: (HsProtobuf.Named a) => DP.Proxy a -> A.Options
pbOpts p = A.defaultOptions{ A.fieldLabelModifier = dropFldPfx p }

--------------------------------------------------------------------------------
-- TODOs
--
-- [ ] Migrate JSONPB typeclass and instances off to DotProto/JSONPB and cleanup
-- imports and naming
--
-- [ ] Look into bugfix regarding Fixed being used for sfixed32/64 fields; I
--     think this should be using Signed instead, and we should write our
--     classes/implementations accordingly. E.g. it seems like either sfixed32
--     and/or sint32 should have a Signed wrapper somewhere, no? Check elsewhere
--     in the codebase to see where it is used and so forth. Even if the current
--     behavior is correct (which would surprise me because I think this means
--     we are not distinguishing the codecs by expected-sign (which is the point
--     of eg sint32 iirc)), I should understand why.
--
-- [ ] Also, it looks like Nested and some the *Vec variants aren't used in CG
--     either, so we should determine if those are bugs or not. We need to
--     ensure that the generated protobufs match the expectations for by-hand
--     construction. E.g, I'm wondering if nested repeateds should use
--     NestedVec.
--
-- Other type support:
--
--   - [ ] enum
--   - [ ] map<K,V>
--   - [ ] bool
--   - [ ] Any
--   - [ ] Timestamp
--   - [ ] Duration
--   - [ ] Struct
--   - [ ] Wrapper types (?)
--   - [ ] FieldMask
--   - [ ] ListValue
--   - [ ] Value
--   - [ ] NullValue
--
--   - [ ] Explore generation of Monoid instances and hiding the details about
--         nestedFieldTo{Enc,JSON} and parseNested; and/or use HasDefault[X] as
--         mentioned above. There is some partial work in a branch for a PBRep
--         (Maybe a) which might be of use to make the field emission/parsing
--         helpers even more abstract w.r.t. field types (ie nested message can
--         be treated just like others with a PBRep and such). I think it will
--         be obvious how this needs to work with the new typeclasses though.
--
-- [ ] Make sure we have all of the by-hand generation pieces working, get it
--     checked with Gabriel, and then whip up a brute force CG version before
--     moving onto the Generics-based implementation. jcarey wants to be able to
--     runthe emission atop protobuf value ASTs, so I think that increases the
--     priority for a Generic implementation.
