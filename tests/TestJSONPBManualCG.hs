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

import qualified Data.ByteString.Lazy         as LBS
import           JSONPBTestTypes
import           Proto3.Suite.DotProto.JSONPB
import qualified Proto3.Suite.Types           as HsProtobuf
import           Text.Show.Pretty

--------------------------------------------------------------------------------
-- Begin hand-generated instances for JSON PB renderings; these instances will
-- be generated once their design is finalized, and live in the same module as
-- the other typedefs and instances (e.g., inside JSONPBProtoTypes, in this
-- case).
--
-- We also put some placeholder doctests here for prelim regression checking
-- until we get some property-based tests in place.

-- | Scalar32
-- prop> roundTrip (Scalar32 32 33 (-34) 35 36)
--
-- prop> encodesAs (Scalar32 32 33 (-34) 35 36) "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
--
-- prop> decodesAs "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}" (Scalar32 32 33 (-34) 35 36)
--

instance ToJSONPB Scalar32 where
  toJSONPB (Scalar32 i32 u32 s32 f32 sf32) = object . mconcat $
    [ "i32"  .= i32
    , "u32"  .= u32
    , "s32"  .= s32
    , "f32"  .= f32
    -- NB: For sfixed32, we need to insert the Signed wrapper ourselves, since
    -- the field type in the record is Fixed Int32, not Signed (Fixed Int32),
    -- and the Signed ctor is un/wrapped via decodeMessage/encodeMessage
    -- instances. During CG we need to apply this whenever the field type is
    -- (Prim SFixed32).
    , "sf32" .= HsProtobuf.Signed sf32
    ]
  toEncodingPB (Scalar32 i32 u32 s32 f32 sf32) = pairs . mconcat $
    [ "i32"  .= i32
    , "u32"  .= u32
    , "s32"  .= s32
    , "f32"  .= f32
      -- See comment above pertaining to sfixed32
    , "sf32" .= HsProtobuf.Signed sf32
    ]
instance FromJSONPB Scalar32 where
  parseJSONPB = withObject "Scalar32" $ \obj ->
    pure Scalar32
    <*> obj .: "i32"
    <*> obj .: "u32"
    <*> obj .: "s32"
    <*> obj .: "f32"
    -- See comment above pertaining to sfixed32, except we are unwrapping
    <*> fmap HsProtobuf.signed (obj .: "sf32")

-- | Scalar64
-- prop> roundTrip (Scalar64 64 65 (-66) 67 68)
--
-- prop> encodesAs (Scalar64 64 65 (-66) 67 68) "{\"i64\":\"64\",\"u64\":\"65\",\"s64\":\"-66\",\"f64\":\"67\",\"sf64\":\"68\"}"
--
-- prop> decodesAs "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}" (Scalar64 64 65 (-66) 67 68)
-- prop> decodesAs "{\"u64\":\"65\",\"s64\":\"66\",\"f64\":\"67\",\"sf64\":\"68\"}" (Scalar64 0 65 66 67 68)
--

instance ToJSONPB Scalar64 where
  toJSONPB (Scalar64 i64 u64 s64 f64 sf64) = object . mconcat $
    [ "i64"  .= i64
    , "u64"  .= u64
    , "s64"  .= s64
    , "f64"  .= f64
    -- NB: For sfixed64, we need to insert the Signed wrapper ourselves, since
    -- the field type in the record is Fixed Int64, not Signed (Fixed Int64),
    -- and the Signed ctor is un/wrapped via decodeMessage/encodeMessage
    -- instances. During CG we need to apply this whenever the field type is
    -- (Prim SFixed64).
    , "sf64" .= HsProtobuf.Signed sf64
    ]
  toEncodingPB (Scalar64 i64 u64 s64 f64 sf64) = pairs . mconcat $
    [ "i64"  .= i64
    , "u64"  .= u64
    , "s64"  .= s64
    , "f64"  .= f64
      -- See comment above pertaining to sfixed64
    , "sf64" .= HsProtobuf.Signed sf64
    ]
instance FromJSONPB Scalar64 where
  parseJSONPB = withObject "Scalar64" $ \obj ->
    pure Scalar64
    <*> obj .: "i64"
    <*> obj .: "u64"
    <*> obj .: "s64"
    <*> obj .: "f64"
      -- See comment above pertaining to sfixed64, except we are unwrapping
    <*> fmap HsProtobuf.signed (obj .: "sf64")

-- | ScalarFP
-- prop> roundTrip (ScalarFP x y)
--
-- prop> encodesAs (ScalarFP 98.6 255.16) "{\"f\":98.6,\"d\":255.16}"
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
-- prop> roundTrip (Stringly "foo" "abc123!?$*&()'-=@~")
--
-- prop> encodesAs (Stringly "foo" "abc123!?$*&()'-=@~") "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
--
-- prop> decodesAs "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}" (Stringly "foo" "abc123!?$*&()'-=@~")
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
-- prop> roundTrip (Repeat (V.fromList xs) (V.fromList ys))
--
-- prop> encodesAs (Repeat [4,5] [6,7]) "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
--
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}" (Repeat [4,5] [6,7])
-- prop> decodesAs "{\"i32s\":[4,5]}"                        (Repeat [4,5] [])
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":null}"          (Repeat [4,5] [])
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":[]}"            (Repeat [4,5] [])
-- prop> decodesAs "{}"                                      (Repeat [] [])
--

instance ToJSONPB Repeat where
  toJSONPB (Repeat i32s i64s) = object . mconcat $
    -- As with the Signed un/wrapping for sfixed32/sfixed64, we need to un/wrap
    -- with PackedVec or UnpackedVec as needed (former by default or when
    -- [packed=false], latter when [packed=true]). In this case they are both
    -- packed. Emit when field type is Repeated elemTy.
    [ "i32s" .= HsProtobuf.PackedVec i32s
    , "i64s" .= HsProtobuf.PackedVec i64s
    ]
  toEncodingPB (Repeat i32s i64s) = pairs . mconcat $
    [ "i32s" .= HsProtobuf.PackedVec i32s
    , "i64s" .= HsProtobuf.PackedVec i64s
    ]
instance FromJSONPB Repeat where
  parseJSONPB = withObject "Repeat" $ \obj ->
    pure Repeat
    <*> fmap HsProtobuf.packedvec (obj .: "i32s")
    <*> fmap HsProtobuf.packedvec (obj .: "i64s")

-- | Nested
-- prop> roundTrip (Nested Nothing)
-- prop> roundTrip (Nested (Just (Nested_Inner x)))
--
-- prop> encodesAs (Nested Nothing)                  "{}"
-- prop> encodesAs (Nested (Just (Nested_Inner 42))) "{\"nestedInner\":{\"i64\":\"42\"}}"
--
-- prop> decodesAs "{}"                                 (Nested Nothing)
-- prop> decodesAs "{\"nestedInner\":{\"i64\":\"42\"}}" (Nested (Just (Nested_Inner 42)))
--

instance ToJSONPB Nested where
  toJSONPB (Nested minner) = object . mconcat $
    -- As with PackedVec and Signed above, we need to un/wrap via Nested for the
    -- Maybe types which correspond to nested messages, i.e. when the field type
    -- is (Prim (Named ...)).
    [ "nestedInner" .= HsProtobuf.Nested minner
    ]
  toEncodingPB (Nested minner) = pairs . mconcat $
    [ "nestedInner" .= HsProtobuf.Nested minner
    ]
instance FromJSONPB Nested where
  parseJSONPB = withObject "Nested" $ \obj ->
    pure Nested
    <*> fmap HsProtobuf.nested (obj .: "nestedInner")

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

--------------------------------------------------------------------------------
-- Incremental support for all of the types from test_proto.proto:

-- NB: We differ slightly in style from the above hand-generated instances and
-- and (generally) provide fewer doctests, since this is slightly closer to the
-- CG target we intended to provide.

-- | Trivial
-- prop> roundTrip (Trivial x)

fieldDefs_Trivial :: (KeyValuePB a, Monoid a) => Trivial -> a
fieldDefs_Trivial (Trivial fld0) = mconcat
  [ "trivialField" .= fld0
  ]

instance ToJSONPB Trivial where
  toJSONPB     = object . fieldDefs_Trivial
  toEncodingPB = pairs  . fieldDefs_Trivial

instance FromJSONPB Trivial where
  parseJSONPB = withObject "Trivial" $ \obj ->
    pure Trivial
    <*> obj .: "trivialField"

-- | MultipleFields
-- prop> roundTrip (MultipleFields d f i32 i64 (TL.pack s) b)

fieldDefs_MultipleFields :: (KeyValuePB a, Monoid a) => MultipleFields -> a
fieldDefs_MultipleFields (MultipleFields f0 f1 f2 f3 f4 f5) = mconcat
  [ "multiFieldDouble" .= f0
  , "multiFieldFloat"  .= f1
  , "multiFieldInt32"  .= f2
  , "multiFieldInt64"  .= f3
  , "multiFieldString" .= f4
  , "multiFieldBool"   .= f5
  ]

instance ToJSONPB MultipleFields where
  toJSONPB     = object . fieldDefs_MultipleFields
  toEncodingPB = pairs  . fieldDefs_MultipleFields

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
-- prop> roundTrip (SignedInts x y)
-- prop> roundTrip (SignedInts minBound minBound)
-- prop> roundTrip (SignedInts maxBound maxBound)
--
-- prop> encodesAs (SignedInts minBound minBound) "{\"signed32\":-2147483648,\"signed64\":\"-9223372036854775808\"}"
-- prop> encodesAs (SignedInts maxBound maxBound) "{\"signed32\":2147483647,\"signed64\":\"9223372036854775807\"}"
--
-- prop> decodesAs "{\"signed32\":2147483647,\"signed64\":\"9223372036854775807\"}" (SignedInts 2147483647 9223372036854775807)
--

fieldDefs_SignedInts :: (KeyValuePB a, Monoid a) => SignedInts -> a
fieldDefs_SignedInts (SignedInts f0 f1) = mconcat
  [ "signed32" .= f0
  , "signed64" .= f1
  ]

instance ToJSONPB SignedInts where
  toJSONPB     = object . fieldDefs_SignedInts
  toEncodingPB = pairs  . fieldDefs_SignedInts

instance FromJSONPB SignedInts where
  parseJSONPB = withObject "SignedInts" $ \obj ->
    pure SignedInts
    <*> obj .: "signed32"
    <*> obj .: "signed64"

-- End hand-generated instances for JSON PB renderings
--------------------------------------------------------------------------------

-- Helper quickcheck props

roundTrip :: (ToJSONPB a, FromJSONPB a, Eq a) => a -> Bool
roundTrip x = eitherDecode (encode x) == Right x

encodesAs :: (ToJSONPB a) => a -> LBS.ByteString -> Bool
encodesAs x bs = encode x == bs

decodesAs :: (Eq a, FromJSONPB a) => LBS.ByteString -> a -> Bool
decodesAs bs x = eitherDecode bs == Right x

__unused_nowarn :: a
__unused_nowarn = undefined (ppShow :: String -> String)

-- Doctest preamble
-- $setup
-- >>> import qualified Data.Text.Lazy as TL
-- >>> import qualified Data.Vector    as V
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
