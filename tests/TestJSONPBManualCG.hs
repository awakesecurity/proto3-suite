{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module tests hand-generated JSONPB instances for types from
-- @TestJSONPBTypes.proto@, as per the proto3 canonical JSON encoding described
-- at https://developers.google.com/protocol-buffers/docs/proto3#json. It is a
-- temporary module containing code that we'll eventually generate via the
-- proto3-suite code generator, once the design is complete.

module TestJSONPBManualCG where

import           Proto3.Suite.DotProto.JSONPB
import           JSONPBTestTypes -- generated code
-- import           Proto3.Suite.DotProto.AST
-- import           Proto3.Suite.DotProto.Generate
import           Text.Show.Pretty

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists

--------------------------------------------------------------------------------
-- Begin hand-generated instances for JSON PB renderings; these instances will
-- be generated once their design is finalized, and live in the same module as
-- the other typedefs and instances (e.g.,
-- Proto3.Suite.DotProto.Generate.JSONPBProto in this case).
--
-- We also put some placeholder doctests here for prelim regression checking
-- until we get some property-based tests in place.

-- | SignedInts
--
-- >>> roundTrip (SignedInts minBound minBound)
-- Right True
-- >>> roundTrip (SignedInts maxBound maxBound)
-- Right True
--
-- >>> encode (SignedInts minBound minBound)
-- "{\"s32\":-2147483648,\"s64\":\"-9223372036854775808\"}"
--
-- >>> encode (SignedInts maxBound maxBound)
-- "{\"s32\":2147483647,\"s64\":\"9223372036854775807\"}"
--
-- >>> Right (SignedInts 2147483647 9223372036854775807) == eitherDecode "{\"s32\":2147483647,\"s64\":\"9223372036854775807\"}"
-- True

instance ToJSONPB SignedInts where
  toJSONPB SignedInts{..} = object . mconcat $
    [ "s32" .= signedIntsS32
    , "s64" .= signedIntsS64
    ]
  toEncodingPB SignedInts{..} = pairs . mconcat $
    [ "s32" .= signedIntsS32
    , "s64" .= signedIntsS64
    ]
instance FromJSONPB SignedInts where
  parseJSONPB = withObject "SignedInts" $ \obj ->
    pure SignedInts
    <*> obj .: "s32"
    <*> obj .: "s64"

-- | Scalar32
--
-- >>> roundTrip (Scalar32 32 33 (-34) 35 36)
-- Right True
--
-- >>> encode (Scalar32 32 33 (-34) 35 36)
-- "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
--
-- >>> Right (Scalar32 32 33 (-34) 35 36) == eitherDecode "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
-- True

-- TODO: Use RecordWildCards to name the fields instead of an explicit pattern match

instance ToJSONPB Scalar32 where
  toJSONPB (Scalar32 i32 u32 s32 f32 sf32) = object . mconcat $
    [ "i32"  .= i32
    , "u32"  .= u32
    , "s32"  .= s32
    , "f32"  .= f32
    , "sf32" .= sf32
    ]
  toEncodingPB (Scalar32 i32 u32 s32 f32 sf32) = pairs . mconcat $
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
--
-- >>> roundTrip (Scalar64 64 65 (-66) 67 68)
-- Right True
--
-- >>> encode (Scalar64 64 65 (-66) 67 68)
-- "{\"i64\":\"64\",\"u64\":\"65\",\"s64\":\"-66\",\"f64\":\"67\",\"sf64\":\"68\"}"
--
-- >>> Right (Scalar64 64 65 (-66) 67 68) == eitherDecode "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}"
-- True
--
-- >>> Right (Scalar64 0 65 66 67 68) == eitherDecode "{\"u64\":\"65\",\"s64\":\"66\",\"f64\":\"67\",\"sf64\":\"68\"}"
-- True

instance ToJSONPB Scalar64 where
  toJSONPB (Scalar64 i64 u64 s64 f64 sf64) = object . mconcat $
    [ "i64"  .= i64
    , "u64"  .= u64
    , "s64"  .= s64
    , "f64"  .= f64
    , "sf64" .= sf64
    ]
  toEncodingPB (Scalar64 i64 u64 s64 f64 sf64) = pairs . mconcat $
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
--
-- >>> roundTrip (ScalarFP 98.6 255.16)
-- Right True
--
-- >>> encode (ScalarFP 98.6 255.16)
-- "{\"f\":98.6,\"d\":255.16}"
--
-- >>> Right (ScalarFP 98.6 255.16) == eitherDecode "{\"f\":98.6,\"d\":255.16}"
-- True
--
-- >>> Right (ScalarFP 23.6 (-99.001)) == eitherDecode "{\"f\":\"23.6\",\"d\":\"-99.001\"}"
-- True
--
-- >>> Right (ScalarFP 1000000.0 (-1000.0)) == eitherDecode "{\"f\":\"1e6\",\"d\":\"-0.1e4\"}"
-- True
--
-- >>> Right (ScalarFP (1/0) (1/0)) == eitherDecode "{\"f\":\"Infinity\",\"d\":\"Infinity\"}"
-- True
--
-- >>> Right (ScalarFP (negate 1/0) (negate 1/0)) == eitherDecode "{\"f\":\"-Infinity\",\"d\":\"-Infinity\"}"
-- True
--
-- >>> eitherDecode "{\"f\":\"NaN\",\"d\":\"NaN\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = NaN, scalarFPD = NaN})

instance ToJSONPB ScalarFP where
  toJSONPB (ScalarFP f d) = object . mconcat $
    [ "f" .= f
    , "d" .= d
    ]
  toEncodingPB (ScalarFP f d) = pairs . mconcat $
    [ "f" .= f
    , "d" .= d
    ]
instance FromJSONPB ScalarFP where
  parseJSONPB = withObject "ScalarFP" $ \obj ->
    pure ScalarFP
    <*> obj .: "f"
    <*> obj .: "d"

-- | Stringly
--
-- >>> roundTrip (Stringly "foo" "abc123!?$*&()'-=@~")
-- Right True
--
-- >>> encode (Stringly "foo" "abc123!?$*&()'-=@~")
-- "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
--
-- >>> Right (Stringly "foo" "abc123!?$*&()'-=@~") == eitherDecode "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
-- True
--

instance ToJSONPB Stringly where
  toJSONPB (Stringly str bs) = object . mconcat $
    [ "str" .= str
    , "bs"  .= bs
    ]
  toEncodingPB (Stringly str bs) = pairs . mconcat $
    [ "str" .= str
    , "bs"  .= bs
    ]
instance FromJSONPB Stringly where
  parseJSONPB = withObject "Stringly" $ \obj ->
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
-- >>> encode (Repeat [4,5] [6,7])
-- "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
--
-- >>> Right (Repeat [4,5] [6,7]) == eitherDecode "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
-- True
--
-- >>> Right (Repeat [4,5] []) == eitherDecode "{\"i32s\":[4,5]}"
-- True
--
-- >>> Right (Repeat [4,5] []) == eitherDecode "{\"i32s\":[4,5],\"i64s\":null}"
-- True
--
-- >>> Right (Repeat [4,5] []) == eitherDecode "{\"i32s\":[4,5],\"i64s\":[]}"
-- True
--
-- >>> Right (Repeat [] []) == eitherDecode "{}"
-- True

instance ToJSONPB Repeat where
  toJSONPB (Repeat i32s i64s) = object . mconcat $
    [ "i32s" .= i32s
    , "i64s" .= i64s
    ]
  toEncodingPB (Repeat i32s i64s) = pairs . mconcat $
    [ "i32s" .= i32s
    , "i64s" .= i64s
    ]
instance FromJSONPB Repeat where
  parseJSONPB = withObject "Repeat" $ \obj ->
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
-- >>> encode (Nested Nothing)
-- "{}"
--
-- >>> encode (Nested (Just (Nested_Inner 42)))
-- "{\"nestedInner\":{\"i64\":\"42\"}}"
--
-- >>> Right (Nested Nothing) == eitherDecode "{}"
-- True
--
-- >>> Right (Nested (Just (Nested_Inner 42))) == eitherDecode "{\"nestedInner\":{\"i64\":\"42\"}}"
-- True
--

instance ToJSONPB Nested where
  toJSONPB (Nested minner) = object . mconcat $
    [ "nestedInner" .= minner
    ]
  toEncodingPB (Nested minner) = pairs . mconcat $
    [ "nestedInner" .= minner
    ]
instance FromJSONPB Nested where
  parseJSONPB = withObject "Nested" $ \obj ->
    pure Nested
    <*> obj .: "nestedInner"

-- Nested_Inner

instance ToJSONPB Nested_Inner where
  toJSONPB (Nested_Inner i64) = object . mconcat $
    [ "i64" .= i64
    ]
  toEncodingPB (Nested_Inner i64) = pairs . mconcat $
    [ "i64" .= i64
    ]
instance FromJSONPB Nested_Inner where
  parseJSONPB = withObject "Nested_Inner" $ \obj ->
    pure Nested_Inner
    <*> obj .: "i64"

-- End hand-generated instances for JSON PB renderings
--------------------------------------------------------------------------------

roundTrip :: (ToJSONPB a, FromJSONPB a, Eq a) => a -> Either String Bool
roundTrip x = either Left (Right . (x==)) . eitherDecode . encode $ x

jsonProtoPath, testProtoPath :: String
jsonProtoPath = "/w/proto3-suite/src/Proto3/Suite/DotProto/Generate/JSON.proto"
testProtoPath = "/w/proto3-suite/test-files/test.proto"

__unused_nowarn :: a
__unused_nowarn = undefined (ppShow :: String -> String)
