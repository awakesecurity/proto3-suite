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
import           Data.Aeson ((.=), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encoding as A
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
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import GHC.Generics as Hs
import GHC.Enum as Hs
import Debug.Trace

-- Import the generated code from JSON.proto. This can be regenerated via e.g.:
--   $ compile-proto-file --proto src/Proto3/Suite/DotProto/Generate/JSON.proto > src/Proto3/Suite/DotProto/Generate/JSONPBProto.hs
import Proto3.Suite.DotProto.Generate.JSONPBProto

--------------------------------------------------------------------------------
-- Begin hand-generated instances for JSON PB renderings; these instances will
-- be generated once their design is finalized, and live in the same module as
-- the other typedefs and instances (e.g.,
-- Proto3.Suite.DotProto.Generate.JSONPBProto in this case).

-- Scalar32
instance A.ToJSON Scalar32 where
  toJSON (Scalar32 i32 u32 s32 f32 sf32) = A.object . mconcat $
    [ fieldToJSON "i32"  i32
    , fieldToJSON "u32"  u32
    , fieldToJSON "s32"  s32
    , fieldToJSON "f32"  f32
    , fieldToJSON "sf32" sf32
    ]
  toEncoding (Scalar32 i32 u32 s32 f32 sf32) = A.pairs . mconcat $
    [ fieldToEnc "i32"  i32
    , fieldToEnc "u32"  u32
    , fieldToEnc "s32"  s32
    , fieldToEnc "f32"  f32
    , fieldToEnc "sf32" sf32
    ]
instance A.FromJSON Scalar32 where
  parseJSON = A.withObject "Scalar32" $ \obj ->
    pure Scalar32
    <*> parseField obj "i32"
    <*> parseField obj "u32"
    <*> parseField obj "s32"
    <*> parseField obj "f32"
    <*> parseField obj "sf32"

-- Scalar64
instance A.ToJSON Scalar64 where
  toJSON (Scalar64 i64 u64 s64 f64 sf64) = A.object . mconcat $
    [ fieldToJSON "i64"  i64
    , fieldToJSON "u64"  u64
    , fieldToJSON "s64"  s64
    , fieldToJSON "f64"  f64
    , fieldToJSON "sf64" sf64
    ]
  toEncoding (Scalar64 i64 u64 s64 f64 sf64) = A.pairs . mconcat $
    [ fieldToEnc "i64"  i64
    , fieldToEnc "u64"  u64
    , fieldToEnc "s64"  s64
    , fieldToEnc "f64"  f64
    , fieldToEnc "sf64" sf64
    ]
instance A.FromJSON Scalar64 where
  parseJSON = A.withObject "Scalar64" $ \obj ->
    pure Scalar64
    <*> parseField obj "i64"
    <*> parseField obj "u64"
    <*> parseField obj "s64"
    <*> parseField obj "f64"
    <*> parseField obj "sf64"

-- ScalarFP
instance A.ToJSON ScalarFP where
  toJSON (ScalarFP f d) = A.object . mconcat $
    [ fieldToJSON "f" f
    , fieldToJSON "d" d
    ]
  toEncoding (ScalarFP f d) = A.pairs . mconcat $
    [ fieldToEnc "f" f
    , fieldToEnc "d" d
    ]
instance A.FromJSON ScalarFP where
  parseJSON = A.withObject "ScalarFP" $ \obj ->
    pure ScalarFP
    <*> parseField obj "f"
    <*> parseField obj "d"

-- Stringly
instance A.ToJSON Stringly where
  toJSON (Stringly str bs) = A.object . mconcat $
    [ fieldToJSON "str" str
    , fieldToJSON "bs" bs
    ]
  toEncoding (Stringly str bs) = A.pairs . mconcat $
    [ fieldToEnc "str" str
    , fieldToEnc "bs" bs
    ]
instance A.FromJSON Stringly where
  parseJSON = A.withObject "Stringly" $ \obj ->
    pure Stringly
    <*> parseField obj "str"
    <*> parseField obj "bs"

-- Repeat
instance A.ToJSON Repeat where
  toJSON (Repeat i32s i64s) = A.object . mconcat $
    [ fieldToJSON "i32s" i32s
    , fieldToJSON "i64s" i64s
    ]
  toEncoding (Repeat i32s i64s) = A.pairs . mconcat $
    [ fieldToEnc "i32s" i32s
    , fieldToEnc "i64s" i64s
    ]
instance A.FromJSON Repeat where
  parseJSON = A.withObject "Repeat" $ \obj ->
    pure Repeat
    <*> parseField obj "i32s"
    <*> parseField obj "i64s"

-- Nested
instance A.ToJSON Nested where
  toJSON (Nested minner) = A.object . mconcat $
    [ nestedFieldToJSON "nestedInner" minner
    ]
  toEncoding (Nested minner) = A.pairs . mconcat $
    [ nestedFieldToEncoding "nestedInner" minner
    ]
instance A.FromJSON Nested where
  parseJSON = A.withObject "Nested" $ \obj ->
    pure Nested
    <*> decodeNested obj "nestedInner"

-- Nested_Inner
instance A.ToJSON Nested_Inner where
  toJSON (Nested_Inner i64) = A.object . mconcat $
    [ fieldToJSON "i64" i64
    ]
  toEncoding (Nested_Inner i64) = A.pairs . mconcat $
    [ fieldToEnc "i64" i64
    ]
instance A.FromJSON Nested_Inner where
  parseJSON = A.withObject "Nested_Inner" $ \obj ->
    pure Nested_Inner
    <*> parseField obj "i64"

-- Trivial
instance A.ToJSON Trivial where
  toJSON (Trivial i32 u32 s32 f32 sf32 i64 u64 s64 f64 sf64 v32 v64 mnest float double string bytes) = A.object . mconcat $
    [ fieldToJSON "trivialField32"      i32
    , fieldToJSON "trivialFieldU32"     u32
    , fieldToJSON "trivialFieldS32"     s32
    , fieldToJSON "trivialFieldF32"     f32
    , fieldToJSON "trivialFieldSF32"    sf32
    , fieldToJSON "trivialField64"      i64
    , fieldToJSON "trivialFieldU64"     u64
    , fieldToJSON "trivialFieldS64"     s64
    , fieldToJSON "trivialFieldF64"     f64
    , fieldToJSON "trivialFieldSF64"    sf64
    , fieldToJSON "repeatedField32"     v32
    , fieldToJSON "repeatedField64"     v64
    , nestedFieldToJSON "nestedMessage" mnest
    , fieldToJSON "trivialFieldFloat"   float
    , fieldToJSON "trivialFieldDouble"  double
    , fieldToJSON "trivialFieldString"  string
    , fieldToJSON "trivialFieldBytes"   bytes
    ]
  toEncoding (Trivial i32 u32 s32 f32 sf32 i64 u64 s64 f64 sf64 v32 v64 mnest float double string bytes) = A.pairs . mconcat $
    [ fieldToEnc "trivialField32"           i32
    , fieldToEnc "trivialFieldU32"          u32
    , fieldToEnc "trivialFieldS32"          s32
    , fieldToEnc "trivialFieldF32"          f32
    , fieldToEnc "trivialFieldSF32"         sf32
    , fieldToEnc "trivialField64"           i64
    , fieldToEnc "trivialFieldU64"          u64
    , fieldToEnc "trivialFieldS64"          s64
    , fieldToEnc "trivialFieldF64"          f64
    , fieldToEnc "trivialFieldSF64"         sf64
    , fieldToEnc "repeatedField32"          v32
    , fieldToEnc "repeatedField64"          v64
    , nestedFieldToEncoding "nestedMessage" mnest
    , fieldToEnc "trivialFieldFloat"        float
    , fieldToEnc "trivialFieldDouble"       double
    , fieldToEnc "trivialFieldString"       string
    , fieldToEnc "trivialFieldBytes"        bytes
    ]

instance A.FromJSON Trivial where
  parseJSON = A.withObject "Trivial" $ \obj ->
    pure Trivial
    <*> parseField obj "trivialField32"
    <*> parseField obj "trivialFieldU32"
    <*> parseField obj "trivialFieldS32"
    <*> parseField obj "trivialFieldF32"
    <*> parseField obj "trivialFieldSF32"
    <*> parseField obj "trivialField64"
    <*> parseField obj "trivialFieldU64"
    <*> parseField obj "trivialFieldS64"
    <*> parseField obj "trivialFieldF64"
    <*> parseField obj "trivialFieldSF64"
    <*> parseField obj "repeatedField32"
    <*> parseField obj "repeatedField64"
    <*> decodeNested obj "nestedMessage"
    <*> parseField obj "trivialFieldFloat"
    <*> parseField obj "trivialFieldDouble"
    <*> parseField obj "trivialFieldString"
    <*> parseField obj "trivialFieldBytes"

-- Trivial_Nested

instance A.ToJSON Trivial_Nested where
  toJSON (Trivial_Nested i64) = A.object . mconcat $
    [ fieldToJSON "nestedField64" i64
    ]
  toEncoding (Trivial_Nested i64) = A.pairs . mconcat $
    [ fieldToEnc "nestedField64" i64
    ]

instance A.FromJSON Trivial_Nested where
  parseJSON = A.withObject "Trivial_Nested" $ \obj ->
    pure Trivial_Nested
    <*> parseField obj "nestedField64"

-- End hand-generated instances for JSON PB renderings
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PB <-> JSON

-- | The class of types which have a "canonical protobuf to JSON encoding"
-- defined. We make use of a data family called PBR ("protobuf rep") to map
-- primitive types to/from their corresponding type wrappers for which
-- ToJSON/FromJSON instances are defined.
class PBRep a where
  data PBR a
  toPBR   :: a -> PBR a
  fromPBR :: PBR a -> a

--------------------------------------------------------------------------------
-- PBReps for int32, sint32, uint32, fixed32, sfixed32.
--
-- JSON value will be a decimal number. Either numbers or strings are accepted.

-- int32, sint32
instance PBRep Hs.Int32 where
  data PBR Hs.Int32   = PBInt32 Hs.Int32 deriving (Show, Generic)
  toPBR               = PBInt32
  fromPBR (PBInt32 x) = x
instance Monoid (PBR Hs.Int32) where
  mempty                                = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR Hs.Int32)
instance A.FromJSON (PBR Hs.Int32) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR Int32 {- int32 / sint32 -}" v

-- TODO: Spec seems to imply that "-10" is a valid value for a uint32; wha-huh?
-- Cf. go impl, I suppose.
--
-- uint32
instance PBRep Hs.Word32 where
  data PBR Hs.Word32   = PBUInt32 Hs.Word32 deriving (Show, Generic)
  toPBR                = PBUInt32
  fromPBR (PBUInt32 x) = x
instance Monoid (PBR Hs.Word32) where
  mempty                                = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR Hs.Word32)
instance A.FromJSON (PBR Hs.Word32) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR Word32 {- uint32 -}" v

-- fixed32
instance PBRep (HsProtobuf.Fixed Hs.Word32) where
  data PBR (HsProtobuf.Fixed Hs.Word32) = PBFixed32 (HsProtobuf.Fixed Hs.Word32) deriving (Show, Generic)
  toPBR                                 = PBFixed32
  fromPBR (PBFixed32 x)                 = x

instance Monoid (PBR (HsProtobuf.Fixed Hs.Word32)) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (HsProtobuf.Fixed Hs.Word32) where
  toJSON (HsProtobuf.Fixed n) = A.toJSON n
instance A.ToJSON (PBR (HsProtobuf.Fixed Hs.Word32))
instance A.FromJSON (HsProtobuf.Fixed Hs.Word32) where
  parseJSON = fmap HsProtobuf.Fixed . A.parseJSON
instance A.FromJSON (PBR (HsProtobuf.Fixed Hs.Word32)) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR (Fixed Word32) {- fixed32 -}" v

-- sfixed32
instance A.ToJSON (HsProtobuf.Fixed Hs.Int32) where
  toJSON (HsProtobuf.Fixed n) = A.toJSON n
instance PBRep (HsProtobuf.Fixed Hs.Int32) where
  data PBR (HsProtobuf.Fixed Hs.Int32) = PBSFixed32 (HsProtobuf.Fixed Hs.Int32) deriving (Show, Generic)
  toPBR                  = PBSFixed32
  fromPBR (PBSFixed32 x) = x
instance Monoid (PBR (HsProtobuf.Fixed Hs.Int32)) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR (HsProtobuf.Fixed Hs.Int32))
instance A.FromJSON (PBR (HsProtobuf.Fixed Hs.Int32)) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR (Fixed Int32) {- sfixed32 -}" v
instance A.FromJSON (HsProtobuf.Fixed Hs.Int32) where
  parseJSON = fmap HsProtobuf.Fixed . A.parseJSON

--------------------------------------------------------------------------------
-- PBReps for int64, sint64, uint64, fixed64, sfixed64.
--
-- JSON value will be a decimal string. Either numbers or strings are accepted.

-- int64, sint64
instance PBRep Hs.Int64 where
  data PBR Hs.Int64   = PBInt64 Hs.Int64 deriving (Show, Generic)
  toPBR               = PBInt64
  fromPBR (PBInt64 x) = x
instance Monoid (PBR Hs.Int64) where
  mempty                                = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR Hs.Int64) where
  toJSON = A.String . Text.pack . Hs.show . fromPBR
instance A.FromJSON (PBR Hs.Int64) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR Int64 {- int64 / sint64 -}" v

-- TODO: Spec seems to imply that "-10" is a valid value for a uint64; wha-huh?
-- Cf. go impl, I suppose.
--
-- uint64
instance PBRep Hs.Word64 where
  data PBR Hs.Word64   = PBUInt64 Hs.Word64 deriving (Show, Generic)
  toPBR                = PBUInt64
  fromPBR (PBUInt64 x) = x
instance Monoid (PBR Hs.Word64) where
  mempty                                = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR Hs.Word64) where
  toJSON = A.String . Text.pack . Hs.show . fromPBR
instance A.FromJSON (PBR Hs.Word64) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR Word64 {- uint64 -}" v

-- fixed64
instance A.ToJSON (HsProtobuf.Fixed Hs.Word64) where
  toJSON (HsProtobuf.Fixed n) = A.toJSON n
instance PBRep (HsProtobuf.Fixed Hs.Word64) where
  data PBR (HsProtobuf.Fixed Hs.Word64) = PBFixed64 (HsProtobuf.Fixed Hs.Word64) deriving (Show, Generic)
  toPBR                                 = PBFixed64
  fromPBR (PBFixed64 x)                 = x
instance Monoid (PBR (HsProtobuf.Fixed Hs.Word64)) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _              x              = x
instance A.ToJSON (PBR (HsProtobuf.Fixed Hs.Word64)) where
  toJSON = A.String . Text.pack . Hs.show . HsProtobuf.fixed . fromPBR
instance A.FromJSON (PBR (HsProtobuf.Fixed Hs.Word64)) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR (Fixed Word64) {- fixed64 -}" v
instance A.FromJSON (HsProtobuf.Fixed Hs.Word64) where
  parseJSON = fmap HsProtobuf.Fixed . A.parseJSON

-- sfixed64
instance A.ToJSON (HsProtobuf.Fixed Hs.Int64) where
  toJSON (HsProtobuf.Fixed n) = A.toJSON n
instance PBRep (HsProtobuf.Fixed Hs.Int64) where
  data PBR (HsProtobuf.Fixed Hs.Int64) = PBSFixed64 (HsProtobuf.Fixed Hs.Int64) deriving (Show, Generic)
  toPBR                                = PBSFixed64
  fromPBR (PBSFixed64 x)               = x
instance Monoid (PBR (HsProtobuf.Fixed Hs.Int64)) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR (HsProtobuf.Fixed Hs.Int64)) where
  toJSON = A.String . Text.pack . Hs.show . HsProtobuf.fixed . fromPBR
instance A.FromJSON (PBR (HsProtobuf.Fixed Hs.Int64)) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR (Fixed Int64) {- sfixed64 -}" v
instance A.FromJSON (HsProtobuf.Fixed Hs.Int64) where
  parseJSON = fmap HsProtobuf.Fixed . A.parseJSON

--------------------------------------------------------------------------------
-- PBReps for float and double
--
-- JSON value will be a number or one of the special string values "NaN",
-- "Infinity", and "-Infinity". Either numbers or strings are accepted. Exponent
-- notation is also accepted.

-- float
instance PBRep Hs.Float where
  data PBR Hs.Float   = PBFloat Hs.Float deriving (Show, Generic)
  toPBR               = PBFloat
  fromPBR (PBFloat x) = x
instance Monoid (PBR Hs.Float) where
  mempty                                = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR Hs.Float)
instance A.FromJSON (PBR Hs.Float) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = parseKeyPB t
  parseJSON v            = A.typeMismatch "PBR Float" v

-- double
instance PBRep Hs.Double where
  data PBR Hs.Double  = PBDouble Hs.Double deriving (Show, Generic)
  toPBR               = PBDouble
  fromPBR (PBDouble x) = x
instance Monoid (PBR Hs.Double) where
  mempty                                = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR Hs.Double)
instance A.FromJSON (PBR Hs.Double) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = parseKeyPB t
  parseJSON v            = A.typeMismatch "PBR Double" v

------------------------------------------------------------------------------------------
-- PBRep for string

instance PBRep Hs.Text where
  data PBR Hs.Text     = PBString Hs.Text deriving (Show, Generic)
  toPBR                = PBString
  fromPBR (PBString x) = x
instance Monoid (PBR (Hs.Text)) where
  mempty                                  = toPBR ""
  mappend (fromPBR -> "") (fromPBR -> y)  = toPBR y
  mappend (fromPBR -> x)  (fromPBR -> "") = toPBR x
  mappend _               x               = x
instance A.ToJSON (PBR Hs.Text)
instance A.FromJSON (PBR Hs.Text)

--------------------------------------------------------------------------------
-- PBRep for bytes
--
-- JSON value will be the data encoded as a string using standard base64
-- encoding with paddings. Either standard or URL-safe base64 encoding
-- with/without paddings are accepted.
--

instance PBRep Hs.ByteString where
  data PBR Hs.ByteString = PBBytes Hs.ByteString deriving (Show, Generic)
  toPBR                  = PBBytes
  fromPBR (PBBytes x)    = x
instance Monoid (PBR (Hs.ByteString)) where
  mempty                                  = toPBR ""
  mappend (fromPBR -> "") (fromPBR -> y)  = toPBR y
  mappend (fromPBR -> x)  (fromPBR -> "") = toPBR x
  mappend _               x               = x
instance A.ToJSON (PBR Hs.ByteString) where
  toJSON (PBBytes bs) = case Hs.decodeUtf8' (B64.encode bs) of
    Left e  -> error ("Failed to encode B64-encoded bytestring: " ++ show e)
               -- decodeUtf8' should never fail because we B64-encode the
               -- incoming bytestring, but we provide an explicit error here
               -- rather than using the partial decodeUf8.
    Right t -> A.toJSON t
instance A.FromJSON (PBR Hs.ByteString) where
  parseJSON (A.String b64enc) = pure . toPBR . B64.decodeLenient . Hs.encodeUtf8 $ b64enc
  parseJSON v                 = A.typeMismatch "PBR ByteString {- bytes -}" v

--------------------------------------------------------------------------------
-- PBRep for repeated

instance (PBRep a) => PBRep (Hs.Vector a) where
  data PBR (Hs.Vector a) = PBVec (Hs.Vector (PBR a))
  toPBR v                = PBVec (toPBR <$> v)
  fromPBR (PBVec v)      = fromPBR <$> v

instance Monoid (PBR (Hs.Vector a)) where
  mempty                        = PBVec mempty
  mappend (PBVec v0) (PBVec v1) = PBVec (mappend v0 v1)

instance (Eq a, PBRep a) => Eq (PBR a) where
  (fromPBR -> a) == (fromPBR -> b) = a == b

instance (A.FromJSON (PBR a), PBRep a) => A.FromJSON (PBR (Hs.Vector a)) where
  parseJSON = fmap (toPBR . fmap fromPBR) . A.parseJSON

instance (A.ToJSON (PBR a), PBRep a) => A.ToJSON (PBR (Hs.Vector a)) where
  toJSON = A.toJSON . fmap toPBR . fromPBR

--------------------------------------------------------------------------------
-- Helpers

parseKeyPB :: (PBRep a, A.FromJSONKey a) => Text.Text -> A.Parser (PBR a)
parseKeyPB t = case A.fromJSONKey of
  A.FromJSONKeyTextParser parse
    -> toPBR <$> parse t
  _ -> fail "internal: parseKeyPB: unexpected FromJSONKey summand"

fieldToJSON :: (PBRep a, A.KeyValue kv, A.ToJSON (PBR a), Monoid (PBR a), Monoid (f kv), Applicative f, Eq a)
            => Text.Text -> a -> f kv
fieldToJSON lab (toPBR -> x) = if x == mempty then mempty else pure (lab .= x)

fieldToEnc :: (Eq a, A.KeyValue m, Monoid m, Monoid (PBR a), A.ToJSON (PBR a), PBRep a)
           => Hs.Text -> a -> m
fieldToEnc lab (toPBR -> x) = if x == mempty then mempty else lab .= x

nestedFieldToJSON :: (A.KeyValue a, A.ToJSON v, Monoid (f a), Applicative f)
                  => Hs.Text -> Maybe v -> f a
nestedFieldToJSON fldSel = foldMap (pure . (fldSel .=))

nestedFieldToEncoding :: (A.KeyValue m, A.ToJSON a, Monoid m) => Hs.Text -> Maybe a -> m
nestedFieldToEncoding fldSel = foldMap (fldSel .=)

parseField :: (A.FromJSON (PBR a), Monoid (PBR a), PBRep a) => A.Object -> Hs.Text -> A.Parser a
parseField o fldSel = fromPBR <$> (o .:? fldSel .!= Hs.mempty)

decodeNested :: A.FromJSON a => A.Object -> Hs.Text -> A.Parser (Maybe a)
decodeNested o fldSel = o .:? fldSel .!= Nothing

fromDecimalString :: (A.FromJSON a, PBRep a) => Hs.Text -> A.Parser (PBR a)
fromDecimalString
  = either fail (pure . toPBR)
  . A.eitherDecode
  . LBS.fromStrict
  . Hs.encodeUtf8

-- Define some tests values for use in doctests
-- $setup
--
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> let scalar32 = Scalar32 32 33 (-34) 35 36
-- >>> let scalar64 = Scalar64 64 65 (-66) 67 68
-- >>> let floatingPoint = ScalarFP 98.6 255.16
-- >>> let stringly = Stringly "foo" "abc123!?$*&()'-=@~"
-- >>> let repeat' = Repeat [4,5] [6,7]
-- >>> let nestedAbsent = Nested Nothing
-- >>> let nestedPresent = Nested (Just (Nested_Inner 42))
-- >>> let myTrivial = Trivial 32 33 (-34) 35 36 64 65 (-66) 67 68 [4,5] [6,7] (Just (Trivial_Nested 101)) 98.6 255.16 "foo" "encodeme"

-- TODO test Repeat with empty lists serdes

-- | Ensure that we can decode what we encode; @Right True@ indicates success.
--
-- >>> roundTrip scalar32
-- Right True
--
-- >>> roundTrip scalar64
-- Right True
--
-- >>> roundTrip floatingPoint
-- Right True
--
-- >>> roundTrip stringly
-- Right True
--
-- >>> roundTrip repeat'
-- Right True
--
-- >>> roundTrip nestedAbsent
-- Right True
--
-- >>> roundTrip nestedPresent
-- Right True
--
-- >>> roundTrip myTrivial
-- Right True
--
-- >>> roundTrip myTrivial{ trivialNestedMessage = Nothing }
-- Right True
roundTrip :: (A.ToJSON a, A.FromJSON a, Eq a) => a -> Either String Bool
roundTrip x = either Left (Right . (x==)) . jsonToPB . pbToJSON $ x

-- | Converting a PB payload to JSON is just encoding via Aeson.
--
-- >>> pbToJSON scalar32
-- "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}"
--
-- >>> pbToJSON scalar64
-- "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}"
--
-- >>> pbToJSON floatingPoint
-- "{\"f\":98.6,\"d\":255.16}"
--
-- >>> pbToJSON stringly
-- "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}"
--
-- >>> pbToJSON repeat'
-- "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}"
--
-- >>> pbToJSON nestedAbsent
-- "{}"
--
-- >>> pbToJSON nestedPresent
-- "{\"nestedInner\":{\"i64\":\"42\"}}"
--
-- >>> pbToJSON myTrivial
-- "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":98.6,\"trivialFieldDouble\":255.16,\"trivialFieldString\":\"foo\",\"trivialFieldBytes\":\"ZW5jb2RlbWU=\"}"
--
-- >>> pbToJSON myTrivial{ trivialNestedMessage = Nothing}
-- "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"trivialFieldFloat\":98.6,\"trivialFieldDouble\":255.16,\"trivialFieldString\":\"foo\",\"trivialFieldBytes\":\"ZW5jb2RlbWU=\"}"
--
pbToJSON :: A.ToJSON a => a -> LBS.ByteString
pbToJSON = A.encode

-- TODO: a lot of these doctests could be simplified

-- | Converting from JSON to PB is just decoding via Aeson.
--
-- >>> jsonToPB "{\"i32\":32,\"u32\":33,\"s32\":-34,\"f32\":35,\"sf32\":36}" :: Either String Scalar32
-- Right (Scalar32 {scalar32I32 = 32, scalar32U32 = 33, scalar32S32 = -34, scalar32F32 = Fixed {fixed = 35}, scalar32Sf32 = Fixed {fixed = 36}})
--
-- >>> jsonToPB "{\"i64\":64,\"u64\":65,\"s64\":-66,\"f64\":67,\"sf64\":68}" :: Either String Scalar64
-- Right (Scalar64 {scalar64I64 = 64, scalar64U64 = 65, scalar64S64 = -66, scalar64F64 = Fixed {fixed = 67}, scalar64Sf64 = Fixed {fixed = 68}})
--
-- >>> jsonToPB "{\"f\":98.6,\"d\":255.16}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = 98.6, scalarFPD = 255.16})
--
-- >>> jsonToPB "{\"f\":\"23.6\",\"d\":\"-99.001\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = 23.6, scalarFPD = -99.001})
--
-- >>> jsonToPB "{\"f\":\"1e6\",\"d\":\"-0.1e4\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = 1000000.0, scalarFPD = -1000.0})
--
-- >>> jsonToPB "{\"f\":\"NaN\",\"d\":\"NaN\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = NaN, scalarFPD = NaN})
--
-- >>> jsonToPB "{\"f\":\"Infinity\",\"d\":\"Infinity\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = Infinity, scalarFPD = Infinity})
--
-- >>> jsonToPB "{\"f\":\"-Infinity\",\"d\":\"-Infinity\"}" :: Either String ScalarFP
-- Right (ScalarFP {scalarFPF = -Infinity, scalarFPD = -Infinity})
--
-- >>> jsonToPB "{\"str\":\"foo\",\"bs\":\"YWJjMTIzIT8kKiYoKSctPUB+\"}" :: Either String Stringly
-- Right (Stringly {stringlyStr = "foo", stringlyBs = "abc123!?$*&()'-=@~"})
--
-- >>> jsonToPB "{\"i32s\":[4,5],\"i64s\":[\"6\",\"7\"]}" :: Either String Repeat
-- Right (Repeat {repeatI32s = [4,5], repeatI64s = [6,7]})
--
-- >>> jsonToPB "{}" :: Either String Nested
-- Right (Nested {nestedNestedInner = Nothing})
--
-- >>> jsonToPB "{\"nestedInner\":{\"i64\":\"42\"}}" :: Either String Nested
-- Right (Nested {nestedNestedInner = Just (Nested_Inner {nested_InnerI64 = 42})})
--
--
--
--
-- >>> jsonToPB (pbToJSON myTrivial) :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = 98.6, trivialTrivialFieldDouble = 255.16, trivialTrivialFieldString = "foo", trivialTrivialFieldBytes = "encodeme"})
--
-- >>> jsonToPB (pbToJSON myTrivial{ trivialNestedMessage = Nothing }) :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Nothing, trivialTrivialFieldFloat = 98.6, trivialTrivialFieldDouble = 255.16, trivialTrivialFieldString = "foo", trivialTrivialFieldBytes = "encodeme"})
--
-- >>> jsonToPB "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":\"1e6\",\"trivialFieldDouble\":255.16,\"trivialFieldString\":\"foo\",\"trivialFieldBytes\":\"ZW5jb2RlbWU=\"}" :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = 1000000.0, trivialTrivialFieldDouble = 255.16, trivialTrivialFieldString = "foo", trivialTrivialFieldBytes = "encodeme"})
--
-- >>> jsonToPB "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":\"1e6\",\"trivialFieldDouble\":\"NaN\",\"trivialFieldString\":\"foo\",\"trivialFieldBytes\":\"ZW5jb2RlbWU=\"}" :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = 1000000.0, trivialTrivialFieldDouble = NaN, trivialTrivialFieldString = "foo", trivialTrivialFieldBytes = "encodeme"})
--
-- >>> jsonToPB "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":\"Infinity\",\"trivialFieldDouble\":\"-Infinity\",\"trivialFieldString\":\"foo\",\"trivialFieldBytes\":\"ZW5jb2RlbWU=\"}" :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = Infinity, trivialTrivialFieldDouble = -Infinity, trivialTrivialFieldString = "foo", trivialTrivialFieldBytes = "encodeme"})
--
jsonToPB :: A.FromJSON a => LBS.ByteString -> Hs.Either Hs.String a
jsonToPB = A.eitherDecode

dropFldPfx :: HsProtobuf.Named a => DP.Proxy a -> Hs.String -> Hs.String
dropFldPfx p s = case dropNamed s of [] -> []; (c:cs) -> Hs.toLower c : cs
  where
    dropNamed = drop (length (HsProtobuf.nameOf p :: String))

pbOpts :: (HsProtobuf.Named a) => DP.Proxy a -> A.Options
pbOpts p = A.defaultOptions{ A.fieldLabelModifier = dropFldPfx p }

-----------------------------------------------------------------------------------------
-- Generic parseJSONPB

genericParseJSONPB :: (Generic a, A.GFromJSON A.Zero (Rep a))
                   => A.Options -> A.Value -> A.Parser a
genericParseJSONPB opts v = to <$> A.gParseJSON opts A.NoFromArgs v

--------------------------------------------------------------------------------
-- TODOs
--
-- [ ] once bytes is supported, let's split the existing monolithic proto into a
-- bunch of substructures for better grouping of type variants and better
-- testing isolation. It will also give us much smaller instances to work with
-- when doing preliminary code generation. The current Trivial datatype is
-- becoming unwieldy.
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
--         nestedFieldTo{Encoding,JSON} and decodeNested
--
-- [ ] Determine if we really NEED/WANT the PBR / data family approach; it feels
--     like it might be extra cruft that isn't doing much heavy lifting for us.
--
-- [ ] Make sure we have all of the by-hand generation working, get it checked
--     with Gabriel, and then move onto the Generics-based implementation.
--
