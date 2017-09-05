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

import           ArbitraryGeneratedTestTypes ()
import qualified Data.ByteString.Lazy        as LBS
import           JSONPBTestTypes
import           Proto3.Suite.JSONPB         (FromJSONPB (..), Options (..),
                                              ToJSONPB (..), eitherDecode,
                                              encode)

--------------------------------------------------------------------------------
-- Formerly, this module held hand-generated instances for JSONPB renderings,
-- until code generation was extended. Now, it just holds a handful of doctests
-- to exercise the generated instances; we should probably relocate/rename this.

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

-- | Scalar64
-- prop> roundTrip omitDefaults (Scalar64 64 65 (-66) 67 68)
--
-- prop> encodesAs omitDefaults (Scalar64 64 65 (-66) 67 68) "{\"i64\":\"64\",\"u64\":\"65\",\"s64\":\"-66\",\"f64\":\"67\",\"sf64\":\"68\"}"
--
-- prop> decodesAs "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}" (Scalar64 64 65 (-66) 67 68)
-- prop> decodesAs "{\"u64\":\"65\",\"s64\":\"66\",\"f64\":\"67\",\"sf64\":\"68\"}" (Scalar64 0 65 66 67 68)
--

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

-- | Stringly
-- prop> roundTrip omitDefaults (Stringly "foo" "abc123!?$*&()'-=@~")
--
-- prop> encodesAs omitDefaults (Stringly "foo" "abc123!?$*&()'-=@~") "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
--
-- prop> decodesAs "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}" (Stringly "foo" "abc123!?$*&()'-=@~")
--

-- | Repeat
-- prop> roundTrip omitDefaults (Repeat xs ys)
--
-- prop> encodesAs omitDefaults (Repeat [4,5] [6,7]) "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
--
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}" (Repeat [4,5] [6,7])
-- prop> decodesAs "{\"i32s\":[4,5]}"                        (Repeat [4,5] [])
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":null}"          (Repeat [4,5] [])
-- prop> decodesAs "{\"i32s\":[4,5],\"i64s\":[]}"            (Repeat [4,5] [])
-- prop> decodesAs "{}"                                      (Repeat [] [])
--

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

-- | Trivial
-- prop> roundTrip omitDefaults (Trivial x)

-- | MultipleFields
-- prop> roundTrip omitDefaults (MultipleFields d f i32 i64 (TL.pack s) b)
-- prop> encodesAs omitDefaults (MultipleFields 0 0 0 0 "" False) "{}"
-- prop> encodesAs emitDefaults (MultipleFields 0 2.0 0 0 "" True) "{\"multiFieldDouble\":0.0,\"multiFieldFloat\":2.0,\"multiFieldInt32\":0,\"multiFieldInt64\":\"0\",\"multiFieldString\":\"\",\"multiFieldBool\":true}"

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

-- | WithNesting
-- prop> roundTrip emitDefaults (WithNesting $ Just $ WithNesting_Nested (TL.pack s) n packed unpacked)
--
-- prop> encodesAs omitDefaults (WithNesting $ Just $ WithNesting_Nested "" 0 [1,2] [66,99]) "{\"nestedMessage\":{\"nestedPacked\":[1,2],\"nestedUnpacked\":[66,99]}}"
--
-- prop> decodesAs "{\"nestedMessage\":{}}" (WithNesting $ Just $ WithNesting_Nested "" 0 [] [])

-- | WithNestingRepeated
-- prop> roundTrip omitDefaults (WithNestingRepeated [WithNestingRepeated_Nested (TL.pack s0) n0 packed0 unpacked0, WithNestingRepeated_Nested (TL.pack s1) n packed1 unpacked1])
-- prop> roundTrip emitDefaults (WithNestingRepeated [WithNestingRepeated_Nested (TL.pack s0) n0 packed0 unpacked0, WithNestingRepeated_Nested (TL.pack s1) n packed1 unpacked1])

-- | WithNestingRepeatedInts
-- prop> roundTrip omitDefaults (WithNestingRepeatedInts [NestedInts xs0 ys0, NestedInts xs1 ys1])
-- prop> roundTrip emitDefaults (WithNestingRepeatedInts [])

-- | WithBytes
-- prop> roundTrip omitDefaults (WithBytes bs0 (V.replicate n0 bs1 <> V.replicate n1 bs2))

-- | OutOfOrderFields
-- prop> roundTrip omitDefaults (OutOfOrderFields xs (TL.pack s) n (TL.pack <$> ss))
-- prop> roundTrip emitDefaults (OutOfOrderFields xs (TL.pack s) n (TL.pack <$> ss))

-- | UsingImported
-- prop> roundTrip omitDefaults $ UsingImported (Just (JSONPBTestTypesImport.WithNesting (Just (JSONPBTestTypesImport.WithNesting_Nested x0 y0)) (Just (JSONPBTestTypesImport.WithNesting_Nested x1 y1)))) (Just (WithNesting (Just (WithNesting_Nested (TL.pack "") n xs ys))))
-- prop> roundTrip emitDefaults $ UsingImported (Just (JSONPBTestTypesImport.WithNesting (Just (JSONPBTestTypesImport.WithNesting_Nested x0 y0)) (Just (JSONPBTestTypesImport.WithNesting_Nested x1 y1)))) (Just (WithNesting (Just (WithNesting_Nested (TL.pack "") n xs ys))))

-- | Wrapped
-- prop> roundTrip omitDefaults (Wrapped (Just (Wrapped (Just (Wrapped Nothing)))))
-- prop> roundTrip emitDefaults (Wrapped (Just (Wrapped (Just (Wrapped (Just (Wrapped Nothing)))))))

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

-- Doctest preamble
-- $setup
-- >>> import Data.Monoid ((<>))
-- >>> import qualified Data.Text.Lazy as TL
-- >>> import qualified Data.Vector    as V
-- >>> import Proto3.Suite.JSONPB (defaultOptions)
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> let omitDefaults = defaultOptions
-- >>> let emitDefaults = defaultOptions{ optEmitDefaultValuedFields = True }
