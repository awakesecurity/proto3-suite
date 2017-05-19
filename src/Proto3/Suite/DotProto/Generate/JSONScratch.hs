{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Proto3.Suite.DotProto.Generate.JSONScratch where

import           Proto3.Suite.DotProto.JSONPB

-- Import the generated code from JSON.proto. This can be regenerated via e.g.:
--   $ compile-proto-file --proto src/Proto3/Suite/DotProto/Generate/JSON.proto > src/Proto3/Suite/DotProto/Generate/JSONPBProto.hs

import           Proto3.Suite.DotProto.Generate.JSONPBProto

import           Proto3.Suite.DotProto.AST
import           Proto3.Suite.DotProto.Generate

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

readDotProto :: String -> IO (DotProto, TypeContext)
readDotProto path = either err pure =<< readDotProtoWithContext path
  where
    err e = fail ("readDotProto failed: " ++ show e)

jsonProtoPath, testProtoPath :: String
jsonProtoPath = "/w/proto3-suite/src/Proto3/Suite/DotProto/Generate/JSON.proto"
testProtoPath = "/w/proto3-suite/test-files/test.proto"

--------------------------------------------------------------------------------
-- TODOs
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
--     So the protobuf AST itself contains the correct Prim a types where a ~
--     SFixed32, Fixed32, and the like. I think it's the wrapper generation
--     where we're going astray. So let's with the simplest, which I think
--     should be SInt32, which should render as a Signed Int32 in the generated
--     Haskell ADT but currently renders as just Int32.
--
--     Perhaps we should start by checking for and/or adding encoding of sint32s
--     to the python tests as the first step.
--
--     Let's add explicit separate test cases for Signed32 and Signed64 maybe?
--     -- Actually just carry through the SignedInts version the whole way -- it
--     existed as a QC test but not the rest of the unit tests. So start
--     there. And, good news! It looks like the non-Signed version is indeed
--     failing to encode, so that's fucking brilliant.
--
--     We need to add tests in quite a few places:
--
--       encode unit tests: checkEncoding tests against golden .bin files from the python impl
--
--       decode unit tests: probably
--
--       parser unit tests
--
--       qc properties tests
--
--       haskell encode -> python decoder tests
--
--       python encoder -> haskell decoder tests
--
--  So:
--
--  HERE: sint32. Add a Signed32 message which contains a sint32. Add it to all
--  of the tests above, with the codegen test coming last. Actually see above,
--  just use the one for SignedInts, maybe after MultipleFields instead of
--  before? Or near the Fixed tests.
--
--    probably want to manage these in a separate pull request/branch, so move
--    these notes to .org files and split up the branch work a little bit, with
--    some cleanups and squashing.
--
-- [ ] Hook up the json round trip tests to the test.proto coverate types and
-- testbench logic; want to see if those tests can be extended to test the
-- jsonpb codec on both ends, possibly against golang instead of python?
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
-- [ ] Consider Generics-based implementation over bruce-force codegen, as we do
--     have use cases where a user may wish to emit JSON directly from protobuf
--     value AST without having to emit any code externally.

__unused_nowarn :: a
__unused_nowarn = undefined (ppShow :: String -> String)
