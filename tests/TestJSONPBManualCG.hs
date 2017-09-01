{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module tests hand-generated JSONPB instances for types from
-- @TestJSONPBTypes.proto@, as per the proto3 canonical JSON encoding described
-- at https://developers.google.com/protocol-buffers/docs/proto3#json. It is a
-- temporary module containing code that we'll eventually generate via the
-- proto3-suite code generator, once the design is complete.

module TestJSONPBManualCG where

import qualified Data.Aeson                   as A (Value (..))
import qualified Data.Aeson.Types             as A (typeMismatch)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Proxy
import           JSONPBTestTypes
import           Proto3.Suite.Class           (HasDefault (..))
import           Proto3.Suite.DotProto.JSONPB (FromJSONPB (..), Options (..),
                                               ToJSONPB (..), eitherDecode,
                                               encode, fieldsPB, namedEncoding,
                                               withObject, (.:), (.=))
import           Text.Show.Pretty


-- tmp/repl
import qualified Data.Aeson.Encoding          as E
import           Proto3.Suite.Types           (Enumerated (..))
import           Test.DocTest

--------------------------------------------------------------------------------
-- Begin hand-generated instances for JSON PB renderings; these instances will
-- be generated once their design is finalized, and live in the same module as
-- the other typedefs and instances (e.g., inside JSONPBProtoTypes, in this
-- case).
--
-- We also put some placeholder doctests here for prelim regression checking
-- until we get some property-based tests in place.

-- | Scalar32
-- prop> roundTrip omitDefaults (Scalar32 32 33 (-34) 35 36)
-- prop> roundTrip emitDefaults (Scalar32 32 33 (-34) 35 36)
--
-- prop> encodesAs omitDefaults (Scalar32 32 33 (-34) 35 36) "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
-- prop> encodesAs emitDefaults (Scalar32 0   1    0   3  4) "{\"i32\":0,\"u32\":1,\"s32\":0,\"f32\":3,\"sf32\":4}"
-- prop> encodesAs omitDefaults (Scalar32 0   1    0   3  4) "{\"u32\":1,\"f32\":3,\"sf32\":4}"
--
-- prop> decodesAs "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}" (Scalar32 32 33 (-34) 35 36)
--

instance ToJSONPB Scalar32 where
  toEncodingPB opts (Scalar32 i32 u32 s32 f32 sf32) = fieldsPB opts
      [ "i32"  .= i32
      , "u32"  .= u32
      , "s32"  .= s32
      , "f32"  .= f32
      , "sf32" .= sf32
      ]

instance FromJSONPB Scalar32 where
  parseJSONPB = withObject "Scalar32" $ \obj ->
    pure Scalar32
    <*> obj .: "i32"
    <*> obj .: "u32"
    <*> obj .: "s32"
    <*> obj .: "f32"
    <*> obj .: "sf32"

-- | Scalar64
-- prop> roundTrip omitDefaults (Scalar64 64 65 (-66) 67 68)
--
-- prop> encodesAs omitDefaults (Scalar64 64 65 (-66) 67 68) "{\"i64\":\"64\",\"u64\":\"65\",\"s64\":\"-66\",\"f64\":\"67\",\"sf64\":\"68\"}"
--
-- prop> decodesAs "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}" (Scalar64 64 65 (-66) 67 68)
-- prop> decodesAs "{\"u64\":\"65\",\"s64\":\"66\",\"f64\":\"67\",\"sf64\":\"68\"}" (Scalar64 0 65 66 67 68)
--

instance ToJSONPB Scalar64 where
  toEncodingPB opts (Scalar64 i64 u64 s64 f64 sf64) = fieldsPB opts
    [ "i64"  .= i64
    , "u64"  .= u64
    , "s64"  .= s64
    , "f64"  .= f64
    , "sf64" .= sf64
    ]
instance FromJSONPB Scalar64 where
  parseJSONPB = withObject "Scalar64" $ \obj ->
    pure Scalar64
    <*> obj .: "i64"
    <*> obj .: "u64"
    <*> obj .: "s64"
    <*> obj .: "f64"
    <*> obj .: "sf64"

-- | ScalarFP
-- prop> roundTrip omitDefaults (ScalarFP x y)
--
-- prop> encodesAs omitDefaults (ScalarFP 98.6 255.16) "{\"f\":98.6,\"d\":255.16}"
--
-- prop> decodesAs "{\"f\":98.6,\"d\":255.16}"                 (ScalarFP 98.6 255.16)
-- prop> decodesAs "{\"f\":\"23.6\",\"d\":\"-99.001\"}"        (ScalarFP 23.6 (-99.001))
-- prop> decodesAs "{\"f\":\"1e6\",\"d\":\"-0.1e4\"}"          (ScalarFP 1000000.0 (-1000.0))
-- prop> decodesAs "{\"f\":\"Infinity\",\"d\":\"Infinity\"}"   (ScalarFP (1/0) (1/0))
-- prop> decodesAs "{\"f\":\"-Infinity\",\"d\":\"-Infinity\"}" (ScalarFP (negate 1/0) (negate 1/0))
--
-- >>> eitherDecode "{\"f\":\"NaN\",\"d\":\"NaN\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = NaN, scalarFPD = NaN})
--

instance ToJSONPB ScalarFP where
  toEncodingPB opts (ScalarFP f d) = fieldsPB opts
    [ "f" .= f
    , "d" .= d
    ]
instance FromJSONPB ScalarFP where
  parseJSONPB = withObject "ScalarFP" $ \obj ->
    pure ScalarFP
    <*> obj .: "f"
    <*> obj .: "d"

-- | Stringly
-- prop> roundTrip omitDefaults (Stringly "foo" "abc123!?$*&()'-=@~")
--
-- prop> encodesAs omitDefaults (Stringly "foo" "abc123!?$*&()'-=@~") "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
--
-- prop> decodesAs "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}" (Stringly "foo" "abc123!?$*&()'-=@~")
--

instance ToJSONPB Stringly where
  toEncodingPB opts (Stringly str bs) = fieldsPB opts
    [ "str" .= str
    , "bs"  .= bs
    ]
instance FromJSONPB Stringly where
  parseJSONPB = withObject "Stringly" $ \obj ->
    pure Stringly
    <*> obj .: "str"
    <*> obj .: "bs"

-- | Repeat
-- prop> roundTrip omitDefaults (Repeat (V.fromList xs) (V.fromList ys))
--
-- prop> encodesAs omitDefaults (Repeat [4,5] [6,7]) "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
--
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}" (Repeat [4,5] [6,7])
-- prop> decodesAs "{\"i32s\":[4,5]}"                        (Repeat [4,5] [])
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":null}"          (Repeat [4,5] [])
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":[]}"            (Repeat [4,5] [])
-- prop> decodesAs "{}"                                      (Repeat [] [])
--

instance ToJSONPB Repeat where
  toEncodingPB opts (Repeat i32s i64s) = fieldsPB opts
    [ "i32s" .= i32s
    , "i64s" .= i64s
    ]
instance FromJSONPB Repeat where
  parseJSONPB = withObject "Repeat" $ \obj ->
    pure Repeat
    <*> obj .: "i32s"
    <*> obj .: "i64s"

-- | Nested
-- prop> roundTrip omitDefaults (Nested Nothing)
-- prop> roundTrip omitDefaults (Nested (Just (Nested_Inner x)))
--
-- prop> encodesAs omitDefaults (Nested Nothing)                  "{}"
-- prop> encodesAs omitDefaults (Nested (Just (Nested_Inner 42))) "{\"nestedInner\":{\"i64\":\"42\"}}"
--
-- prop> decodesAs "{}"                                 (Nested Nothing)
-- prop> decodesAs "{\"nestedInner\":{\"i64\":\"42\"}}" (Nested (Just (Nested_Inner 42)))
--

instance ToJSONPB Nested where
  toEncodingPB opts (Nested minner) = fieldsPB opts
    [ "nestedInner" .= minner
    ]
instance FromJSONPB Nested where
  parseJSONPB = withObject "Nested" $ \obj ->
    pure Nested
    <*> obj .: "nestedInner"

-- Nested_Inner

instance ToJSONPB Nested_Inner where
  toEncodingPB opts (Nested_Inner i64) = fieldsPB opts
    [ "i64" .= i64
    ]
instance FromJSONPB Nested_Inner where
  parseJSONPB = withObject "Nested_Inner" $ \obj ->
    pure Nested_Inner
    <*> obj .: "i64"

--------------------------------------------------------------------------------
-- Incremental support for all of the types from test_proto.proto:

-- NB: We differ slightly in style from the above hand-generated instances and
-- and (generally) provide fewer doctests, since this is slightly closer to the
-- CG target we intended to provide.

-- | Trivial
-- prop> roundTrip omitDefaults (Trivial x)

instance ToJSONPB Trivial where
  toEncodingPB opts (Trivial f0) = fieldsPB opts
    [ "trivialField" .= f0
    ]

instance FromJSONPB Trivial where
  parseJSONPB = withObject "Trivial" $ \obj ->
    pure Trivial
    <*> obj .: "trivialField"

-- | MultipleFields
-- prop> roundTrip omitDefaults (MultipleFields d f i32 i64 (TL.pack s) b)
-- prop> encodesAs omitDefaults (MultipleFields 0 0 0 0 "" False) "{}"
-- prop> encodesAs emitDefaults (MultipleFields 0 2.0 0 0 "" True) "{\"multiFieldDouble\":0.0,\"multiFieldFloat\":2.0,\"multiFieldInt32\":0,\"multiFieldInt64\":\"0\",\"multiFieldString\":\"\",\"multiFieldBool\":true}"

instance ToJSONPB MultipleFields where
  toEncodingPB opts(MultipleFields f0 f1 f2 f3 f4 f5) = fieldsPB opts
    [ "multiFieldDouble" .= f0
    , "multiFieldFloat"  .= f1
    , "multiFieldInt32"  .= f2
    , "multiFieldInt64"  .= f3
    , "multiFieldString" .= f4
    , "multiFieldBool"   .= f5
    ]

instance FromJSONPB MultipleFields where
  parseJSONPB = withObject "MultipleFields" $ \obj ->
    pure MultipleFields
    <*> obj .: "multiFieldDouble"
    <*> obj .: "multiFieldFloat"
    <*> obj .: "multiFieldInt32"
    <*> obj .: "multiFieldInt64"
    <*> obj .: "multiFieldString"
    <*> obj .: "multiFieldBool"

-- | SignedInts
-- prop> roundTrip omitDefaults (SignedInts x y)
-- prop> roundTrip omitDefaults (SignedInts minBound minBound)
-- prop> roundTrip omitDefaults (SignedInts maxBound maxBound)
--
-- prop> encodesAs omitDefaults (SignedInts minBound minBound) "{\"signed32\":-2147483648,\"signed64\":\"-9223372036854775808\"}"
-- prop> encodesAs omitDefaults (SignedInts maxBound maxBound) "{\"signed32\":2147483647,\"signed64\":\"9223372036854775807\"}"
--
-- prop> decodesAs "{\"signed32\":2147483647,\"signed64\":\"9223372036854775807\"}" (SignedInts 2147483647 9223372036854775807)
--

instance ToJSONPB SignedInts where
  toEncodingPB opts (SignedInts f0 f1) = fieldsPB opts
    [ "signed32" .= f0
    , "signed64" .= f1
    ]

instance FromJSONPB SignedInts where
  parseJSONPB = withObject "SignedInts" $ \obj ->
    pure SignedInts
    <*> obj .: "signed32"
    <*> obj .: "signed64"

-- | WithEnum
-- prop> roundTrip omitDefaults (WithEnum (Enumerated (Right WithEnum_TestEnumENUM1)))
-- prop> roundTrip omitDefaults (WithEnum (Enumerated (Right WithEnum_TestEnumENUM2)))
--
-- prop> encodesAs omitDefaults (WithEnum (Enumerated (Right WithEnum_TestEnumENUM1))) "{}"
-- prop> encodesAs emitDefaults (WithEnum (Enumerated (Right WithEnum_TestEnumENUM1))) "{\"enumField\":\"ENUM1\"}"
-- prop> encodesAs omitDefaults (WithEnum (Enumerated (Right WithEnum_TestEnumENUM3))) "{\"enumField\":\"ENUM3\"}"
--
-- prop> decodesAs "{\"enumField\":\"ENUM3\"}" (WithEnum (Enumerated (Right WithEnum_TestEnumENUM3)))
-- prop> decodesAs "{\"enumField\":null}"      (WithEnum (Enumerated (Right WithEnum_TestEnumENUM1)))
-- prop> decodesAs "{}"                        (WithEnum (Enumerated (Right WithEnum_TestEnumENUM1)))

instance ToJSONPB WithEnum where
  toEncodingPB opts (WithEnum f0) = fieldsPB opts
    [ "enumField" .= f0
    ]

instance FromJSONPB WithEnum where
  parseJSONPB = withObject "withEnum" $ \obj ->
    pure WithEnum
    <*> obj .: "enumField"

-- WithEnum_TestEnum
instance ToJSONPB WithEnum_TestEnum where
  toEncodingPB _ = namedEncoding

instance FromJSONPB WithEnum_TestEnum where
  parseJSONPB (A.String "ENUM1") = pure WithEnum_TestEnumENUM1
  parseJSONPB (A.String "ENUM2") = pure WithEnum_TestEnumENUM2
  parseJSONPB (A.String "ENUM3") = pure WithEnum_TestEnumENUM3
  parseJSONPB v                  = A.typeMismatch "WithEnum_TestEnum" v

-- End hand-generated instances for JSON PB renderings
--------------------------------------------------------------------------------

-- Helper quickcheck props

roundTrip :: (ToJSONPB a, FromJSONPB a, Eq a)
          => Options -> a -> Bool
roundTrip opts x = eitherDecode (encode opts x) == Right x

encodesAs :: (ToJSONPB a)
          => Options -> a -> LBS.ByteString -> Bool
encodesAs opts x bs = encode opts x == bs

decodesAs :: (Eq a, FromJSONPB a)
          => LBS.ByteString -> a -> Bool
decodesAs bs x = eitherDecode bs == Right x

__unused_nowarn :: a
__unused_nowarn = undefined (ppShow :: String -> String)

-- Doctest preamble
-- $setup
-- >>> import qualified Data.Text.Lazy as TL
-- >>> import qualified Data.Vector    as V
-- >>> import Proto3.Suite.DotProto.JSONPB (defaultOptions)
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> let omitDefaults = defaultOptions
-- >>> let emitDefaults = defaultOptions{ optEmitDefaultValuedFields = True }
