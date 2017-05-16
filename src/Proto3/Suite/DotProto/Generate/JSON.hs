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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
import qualified Data.Text.Encoding as Hs (encodeUtf8)
import qualified Data.ByteString as Hs
import qualified Data.ByteString.Lazy as LBS
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import GHC.Generics as Hs
import GHC.Enum as Hs
import Debug.Trace

-- $setup
--
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings

-- Experiments scratch

--------------------------------------------------------------------------------
-- Trivial

-- Can obtain AST via e.g. (readDotProtoWithContext "/w/proto3-suite/t.proto")

-- Via eg. (renderHsModuleForDotProtoFile "/w/proto3-suite/t.proto" >>= \(Prelude.Right s) -> Prelude.putStrLn s)
data Trivial = Trivial{trivialTrivialField32 :: Hs.Int32,
                       trivialTrivialFieldU32 :: Hs.Word32,
                       trivialTrivialFieldS32 :: Hs.Int32,
                       trivialTrivialFieldF32 :: HsProtobuf.Fixed Hs.Word32,
                       trivialTrivialFieldSF32 :: HsProtobuf.Fixed Hs.Int32,
                       trivialTrivialField64 :: Hs.Int64,
                       trivialTrivialFieldU64 :: Hs.Word64,
                       trivialTrivialFieldS64 :: Hs.Int64,
                       trivialTrivialFieldF64 :: HsProtobuf.Fixed Hs.Word64,
                       trivialTrivialFieldSF64 :: HsProtobuf.Fixed Hs.Int64,
                       trivialRepeatedField32 :: Hs.Vector Hs.Int32,
                       trivialRepeatedField64 :: Hs.Vector Hs.Int64,
                       trivialNestedMessage :: Hs.Maybe Trivial_Nested,
                       trivialTrivialFieldFloat :: Hs.Float,
                       trivialTrivialFieldDouble :: Hs.Double}
             deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named Trivial where
        nameOf _ = (Hs.fromString "Trivial")

instance HsProtobuf.Message Trivial where
        encodeMessage _
          Trivial{trivialTrivialField32 = trivialTrivialField32,
                  trivialTrivialFieldU32 = trivialTrivialFieldU32,
                  trivialTrivialFieldS32 = trivialTrivialFieldS32,
                  trivialTrivialFieldF32 = trivialTrivialFieldF32,
                  trivialTrivialFieldSF32 = trivialTrivialFieldSF32,
                  trivialTrivialField64 = trivialTrivialField64,
                  trivialTrivialFieldU64 = trivialTrivialFieldU64,
                  trivialTrivialFieldS64 = trivialTrivialFieldS64,
                  trivialTrivialFieldF64 = trivialTrivialFieldF64,
                  trivialTrivialFieldSF64 = trivialTrivialFieldSF64,
                  trivialRepeatedField32 = trivialRepeatedField32,
                  trivialRepeatedField64 = trivialRepeatedField64,
                  trivialNestedMessage = trivialNestedMessage,
                  trivialTrivialFieldFloat = trivialTrivialFieldFloat,
                  trivialTrivialFieldDouble = trivialTrivialFieldDouble}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   trivialTrivialField32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   trivialTrivialFieldU32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 3)
                   trivialTrivialFieldS32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 4)
                   trivialTrivialFieldF32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 5)
                   (HsProtobuf.Signed trivialTrivialFieldSF32)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 6)
                   trivialTrivialField64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 7)
                   trivialTrivialFieldU64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 8)
                   trivialTrivialFieldS64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 9)
                   trivialTrivialFieldF64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 10)
                   (HsProtobuf.Signed trivialTrivialFieldSF64)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 11)
                   (HsProtobuf.PackedVec trivialRepeatedField32)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 12)
                   (HsProtobuf.PackedVec trivialRepeatedField64)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 13)
                   (HsProtobuf.Nested trivialNestedMessage)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 14)
                   trivialTrivialFieldFloat),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 15)
                   trivialTrivialFieldDouble)])
        decodeMessage _
          = (Hs.pure Trivial) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 3))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 4))
              <*>
              ((Hs.pure HsProtobuf.signed) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 5)))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 6))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 7))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 8))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 9))
              <*>
              ((Hs.pure HsProtobuf.signed) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 10)))
              <*>
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 11)))
              <*>
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 12)))
              <*>
              ((Hs.pure HsProtobuf.nested) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 13)))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 14))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 15))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "trivialField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.UInt32)
                (HsProtobuf.Single "trivialFieldU32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 3)
                (HsProtobuf.Prim HsProtobuf.SInt32)
                (HsProtobuf.Single "trivialFieldS32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 4)
                (HsProtobuf.Prim HsProtobuf.Fixed32)
                (HsProtobuf.Single "trivialFieldF32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 5)
                (HsProtobuf.Prim HsProtobuf.SFixed32)
                (HsProtobuf.Single "trivialFieldSF32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 6)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "trivialField64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 7)
                (HsProtobuf.Prim HsProtobuf.UInt64)
                (HsProtobuf.Single "trivialFieldU64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 8)
                (HsProtobuf.Prim HsProtobuf.SInt64)
                (HsProtobuf.Single "trivialFieldS64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 9)
                (HsProtobuf.Prim HsProtobuf.Fixed64)
                (HsProtobuf.Single "trivialFieldF64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 10)
                (HsProtobuf.Prim HsProtobuf.SFixed64)
                (HsProtobuf.Single "trivialFieldSF64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 11)
                (HsProtobuf.Repeated HsProtobuf.Int32)
                (HsProtobuf.Single "repeatedField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 12)
                (HsProtobuf.Repeated HsProtobuf.Int64)
                (HsProtobuf.Single "repeatedField64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 13)
                (HsProtobuf.Prim (HsProtobuf.Named (HsProtobuf.Single "Nested")))
                (HsProtobuf.Single "nestedMessage")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 14)
                (HsProtobuf.Prim HsProtobuf.Float)
                (HsProtobuf.Single "trivialFieldFloat")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 15)
                (HsProtobuf.Prim HsProtobuf.Double)
                (HsProtobuf.Single "trivialFieldDouble")
                []
                Hs.Nothing)]

data Trivial_Nested = Trivial_Nested{trivial_NestedNestedField64 ::
                                     Hs.Int64}
                    deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named Trivial_Nested where
        nameOf _ = (Hs.fromString "Trivial_Nested")

instance HsProtobuf.Message Trivial_Nested where
        encodeMessage _
          Trivial_Nested{trivial_NestedNestedField64 =
                           trivial_NestedNestedField64}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   trivial_NestedNestedField64)])
        decodeMessage _
          = (Hs.pure Trivial_Nested) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "nestedField64")
                []
                Hs.Nothing)]

--------------------------------------------------------------------------------
-- Instance for Trivial_Nested (these instances will be generated, eventually)

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

--------------------------------------------------------------------------------
-- Instance for Trivial (these instances will be generated, eventually)

instance A.ToJSON Trivial where
  toJSON (Trivial i32 u32 s32 f32 sf32 i64 u64 s64 f64 sf64 v32 v64 mnest float double) = A.object . mconcat $
    [ fieldToJSON "trivialField32"      i32
    , fieldToJSON "trivialFieldU32"     u32
    , fieldToJSON "trivialFieldS32"     s32
    , fieldToJSON "trivialFieldF32"     (F32 f32)
    , fieldToJSON "trivialFieldSF32"    (SF32 sf32)
    , fieldToJSON "trivialField64"      i64
    , fieldToJSON "trivialFieldU64"     u64
    , fieldToJSON "trivialFieldS64"     s64
    , fieldToJSON "trivialFieldF64"     (F64 f64)
    , fieldToJSON "trivialFieldSF64"    (SF64 sf64)
    , fieldToJSON "repeatedField32"     v32
    , fieldToJSON "repeatedField64"     v64
    , nestedFieldToJSON "nestedMessage" mnest
    , fieldToJSON "trivialFieldFloat"   float
    , fieldToJSON "trivialFieldDouble"  double
    ]
  toEncoding (Trivial i32 u32 s32 f32 sf32 i64 u64 s64 f64 sf64 v32 v64 mnest float double) = A.pairs . mconcat $
    [ fieldToEnc "trivialField32"           i32
    , fieldToEnc "trivialFieldU32"          u32
    , fieldToEnc "trivialFieldS32"          s32
    , fieldToEnc "trivialFieldF32"          (F32 f32)
    , fieldToEnc "trivialFieldSF32"         (SF32 sf32)
    , fieldToEnc "trivialField64"           i64
    , fieldToEnc "trivialFieldU64"          u64
    , fieldToEnc "trivialFieldS64"          s64
    , fieldToEnc "trivialFieldF64"          (F64 f64)
    , fieldToEnc "trivialFieldSF64"         (SF64 sf64)
    , fieldToEnc "repeatedField32"          v32
    , fieldToEnc "repeatedField64"          v64
    , nestedFieldToEncoding "nestedMessage" mnest
    , fieldToEnc "trivialFieldFloat"        float
    , fieldToEnc "trivialFieldDouble"       double
    ]

instance A.FromJSON Trivial where
  parseJSON = A.withObject "Trivial" $ \obj ->
    pure Trivial
    <*> parseField obj "trivialField32"
    <*> parseField obj "trivialFieldU32"
    <*> parseField obj "trivialFieldS32"
    <*> do F32  x <- parseField obj "trivialFieldF32"; pure x
    <*> do SF32 x <- parseField obj "trivialFieldSF32"; pure x
    <*> parseField obj "trivialField64"
    <*> parseField obj "trivialFieldU64"
    <*> parseField obj "trivialFieldS64"
    <*> do F64 x <- parseField obj "trivialFieldF64"; pure x
    <*> do SF64 x <- parseField obj "trivialFieldSF64"; pure x
    <*> parseField obj "repeatedField32"
    <*> parseField obj "repeatedField64"
    <*> decodeNested obj "nestedMessage"
    <*> parseField obj "trivialFieldFloat"
    <*> parseField obj "trivialFieldDouble"

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

-- Premature abstraction
-- instance (Functor f, PBRep a) => PBRep (f a) where
--   data PBR (f a)     = PBReps (f (PBR a))
--   toPBR v            = PBReps (toPBR <$> v)
--   fromPBR (PBReps v) = fromPBR <$> v
instance (PBRep a) => PBRep (Hs.Vector a) where
  data PBR (Hs.Vector a) = PBVec (Hs.Vector (PBR a))
  toPBR v                = PBVec (toPBR <$> v)
  fromPBR (PBVec v)      = fromPBR <$> v

-- Premature abstraction:
-- instance (Functor f, Monoid (f a), PBRep a, PBRep(f a)) => Monoid (PBR (f a)) where
--   mempty                                = toPBR mempty
--   mappend (fromPBR -> x) (fromPBR -> y) = toPBR (mappend x y)
instance Monoid (PBR (Hs.Vector a)) where
  mempty                        = PBVec mempty
  mappend (PBVec v0) (PBVec v1) = PBVec (mappend v0 v1)

instance (Eq a, PBRep a) => Eq (PBR a) where
  (fromPBR -> a) == (fromPBR -> b) = a == b

-- NB: Can probably quite safely abstract Vector a to Functor f => f a.
-- https://www.reddit.com/r/haskell/comments/64iuia/when_is_undecidableinstances_safe/

instance (A.FromJSON (PBR a), PBRep a) => A.FromJSON (PBR (Hs.Vector a)) where
  parseJSON = fmap (toPBR . fmap fromPBR) . A.parseJSON

instance (A.ToJSON (PBR a), PBRep a) => A.ToJSON (PBR (Hs.Vector a)) where
  toJSON = A.toJSON . fmap toPBR . fromPBR

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
  parseJSON v            = A.typeMismatch "PBR Int32" v

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
  parseJSON v            = A.typeMismatch "PBR Word32" v

-- fixed32
newtype F32 = F32 (HsProtobuf.Fixed Hs.Word32) deriving (Eq, Num, Show)
instance A.ToJSON F32 where
  toJSON (F32 (HsProtobuf.Fixed n)) = A.toJSON n
instance PBRep F32 where
  data PBR F32          = PBFixed32 F32 deriving (Show, Generic)
  toPBR                 = PBFixed32
  fromPBR (PBFixed32 x) = x
instance Monoid (PBR F32) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR F32)
instance A.FromJSON (PBR F32) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR F32" v
instance A.FromJSON F32 where
  parseJSON = fmap (F32 . HsProtobuf.Fixed) . A.parseJSON

-- sfixed32
newtype SF32 = SF32 (HsProtobuf.Fixed Hs.Int32) deriving (Eq, Num, Show)
instance A.ToJSON SF32 where
  toJSON (SF32 (HsProtobuf.Fixed n)) = A.toJSON n
instance PBRep SF32 where
  data PBR SF32          = PBSFixed32 SF32 deriving (Show, Generic)
  toPBR                  = PBSFixed32
  fromPBR (PBSFixed32 x) = x
instance Monoid (PBR SF32) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR SF32)
instance A.FromJSON (PBR SF32) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR SF32" v
instance A.FromJSON SF32 where
  parseJSON = fmap (SF32 . HsProtobuf.Fixed) . A.parseJSON

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
  parseJSON v            = A.typeMismatch "PBInt64" v

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
  parseJSON v            = A.typeMismatch "PBR Word64" v

-- fixed64
newtype F64 = F64 (HsProtobuf.Fixed Hs.Word64) deriving (Eq, Num, Show)
instance A.ToJSON F64 where
  toJSON (F64 (HsProtobuf.Fixed n)) = A.toJSON n
instance PBRep F64 where
  data PBR F64          = PBFixed64 F64 deriving (Show, Generic)
  toPBR                 = PBFixed64
  fromPBR (PBFixed64 x) = x
instance Monoid (PBR F64) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _              x              = x
instance A.ToJSON (PBR F64) where
  toJSON = A.String . Text.pack . Hs.show . HsProtobuf.fixed . (\(F64 x) -> x) . fromPBR
instance A.FromJSON (PBR F64) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR F64" v
instance A.FromJSON F64 where
  parseJSON = fmap (F64 . HsProtobuf.Fixed) . A.parseJSON

-- sfixed64
newtype SF64 = SF64 (HsProtobuf.Fixed Hs.Int64) deriving (Eq, Num, Show)
instance A.ToJSON SF64 where
  toJSON (SF64 (HsProtobuf.Fixed n)) = A.toJSON n
instance PBRep SF64 where
  data PBR SF64          = PBSFixed64 SF64 deriving (Show, Generic)
  toPBR                  = PBSFixed64
  fromPBR (PBSFixed64 x) = x
instance Monoid (PBR SF64) where
  mempty = toPBR 0
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x
instance A.ToJSON (PBR SF64) where
  toJSON = A.String . Text.pack . Hs.show . HsProtobuf.fixed . (\(SF64 x) -> x) . fromPBR
instance A.FromJSON (PBR SF64) where
  parseJSON v@A.Number{} = toPBR <$> A.parseJSON v
  parseJSON (A.String t) = fromDecimalString t
  parseJSON v            = A.typeMismatch "PBR SF64" v
instance A.FromJSON SF64 where
  parseJSON = fmap (SF64 . HsProtobuf.Fixed) . A.parseJSON

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

-- | Ensure that encoding and decoding are mutually inverse
--
-- >>> roundTrip (Trivial 32 33 (-34) 35 36 64 65 (-66) 67 68 [4,5] [6,7] (Just (Trivial_Nested 101)) 98.6 255.16)
-- Right True
roundTrip :: (A.ToJSON a, A.FromJSON a, Eq a) => a -> Either String Bool
roundTrip x = either Left (Right . (x==)) . jsonToPB . pbToJSON $ x

-- | Converting a PB payload to JSON is just encoding via Aeson.
--
-- >>> pbToJSON (Trivial 32 33 (-34) 35 36 64 65 (-66) 67 68 [4,5] [6,7] (Just (Trivial_Nested 101)) 195.6888 62.123)
-- "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":195.6888,\"trivialFieldDouble\":62.123}"
pbToJSON :: A.ToJSON a => a -> LBS.ByteString
pbToJSON = A.encode

-- | Converting from JSON to PB is just decoding via Aeson.
--
-- >>> jsonToPB "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":\"1e6\",\"trivialFieldDouble\":62.123}" :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = 1000000.0, trivialTrivialFieldDouble = 62.123})
-- >>> jsonToPB "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":\"1e6\",\"trivialFieldDouble\":\"NaN\"}" :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = 1000000.0, trivialTrivialFieldDouble = NaN})
-- >>> jsonToPB "{\"trivialField32\":32,\"trivialFieldU32\":33,\"trivialFieldS32\":-34,\"trivialFieldF32\":35,\"trivialFieldSF32\":36,\"trivialField64\":\"64\",\"trivialFieldU64\":\"65\",\"trivialFieldS64\":\"-66\",\"trivialFieldF64\":\"67\",\"trivialFieldSF64\":\"68\",\"repeatedField32\":[4,5],\"repeatedField64\":[\"6\",\"7\"],\"nestedMessage\":{\"nestedField64\":\"101\"},\"trivialFieldFloat\":\"-Infinity\",\"trivialFieldDouble\":\"Infinity\"}" :: Either String Trivial
-- Right (Trivial {trivialTrivialField32 = 32, trivialTrivialFieldU32 = 33, trivialTrivialFieldS32 = -34, trivialTrivialFieldF32 = Fixed {fixed = 35}, trivialTrivialFieldSF32 = Fixed {fixed = 36}, trivialTrivialField64 = 64, trivialTrivialFieldU64 = 65, trivialTrivialFieldS64 = -66, trivialTrivialFieldF64 = Fixed {fixed = 67}, trivialTrivialFieldSF64 = Fixed {fixed = 68}, trivialRepeatedField32 = [4,5], trivialRepeatedField64 = [6,7], trivialNestedMessage = Just (Trivial_Nested {trivial_NestedNestedField64 = 101}), trivialTrivialFieldFloat = -Infinity, trivialTrivialFieldDouble = Infinity})
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
-- Scratch
--
-- We should hand-elaborate a little further and make sure we have a strategy
-- for dealing with message nesting, repetition, packed vecs, etc., before we
-- push too much farther on the generic parser. I.e., we should be able to write
-- clean/simple "brute force" codecs before we worry about the generic parser.
--
-- [ ] Then let's extend Int32/Int64 support for the fixed/unsigned variants as
-- well and make sure we don't have any uncomfortable overlap or design flaws
-- for types which are encoded the same way...
--
-- HERE: test double type in Trivial value and add to doctests
--
-- TODO: See if we can remove the intermediate F32 newtype and/or accurately
--       describe why we cannot? I am starting to get the feeling that it is not
--       needed.
--
--   - [ ] remaining scalar types: double string bytes
--   - [ ] other aggregate types eg maps and crap
--   - [ ] 'any' type, blech
--
-- [ ] And then let's try our hand at gParseJSONPB after that
