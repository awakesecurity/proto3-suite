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

-- Experiments scratch

--------------------------------------------------------------------------------
-- Trivial

-- message Trivial {
--   int32 trivialField = 1;
-- }

-- Via eg. (readDotProtoWithContext "/w/proto3-suite/t.proto")
trivialDotProtoAST :: Hs.Either CompileError (DotProto, TypeContext)
trivialDotProtoAST =
  Hs.Right
    ( DotProto
        { protoImports = []
        , protoOptions = []
        , protoPackage = DotProtoPackageSpec (Single "tee")
        , protoDefinitions =
            [ DotProtoMessage
                (Single "Trivial")
                [ DotProtoMessageField
                    DotProtoField
                      { dotProtoFieldNumber = HsProtobuf.FieldNumber { HsProtobuf.getFieldNumber = 1 }
                      , dotProtoFieldType = Prim Int32
                      , dotProtoFieldName = Single "trivialField"
                      , dotProtoFieldOptions = []
                      , dotProtoFieldComment = Hs.Nothing
                      }
                ]
            ]
        }
    , Hs.fromList []
    )

-- Via eg. (renderHsModuleForDotProtoFile "/w/proto3-suite/t.proto" >>= \(Prelude.Right s) -> Prelude.putStrLn s)
data Trivial = Trivial{trivialTrivialField32 :: Hs.Int32,
                       trivialTrivialFieldF32 :: HsProtobuf.Fixed Hs.Word32,
                       trivialTrivialField64 :: Hs.Int64,
                       trivialRepeatedField32 :: Hs.Vector Hs.Int32,
                       trivialRepeatedField64 :: Hs.Vector Hs.Int64,
                       trivialNestedMessage :: Hs.Maybe Trivial_Nested}
             deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named Trivial where
        nameOf _ = (Hs.fromString "Trivial")

instance HsProtobuf.Message Trivial where
        encodeMessage _
          Trivial{trivialTrivialField32 = trivialTrivialField32,
                  trivialTrivialFieldF32 = trivialTrivialFieldF32,
                  trivialTrivialField64 = trivialTrivialField64,
                  trivialRepeatedField32 = trivialRepeatedField32,
                  trivialRepeatedField64 = trivialRepeatedField64,
                  trivialNestedMessage = trivialNestedMessage}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   trivialTrivialField32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   trivialTrivialFieldF32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 3)
                   trivialTrivialField64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 4)
                   (HsProtobuf.PackedVec trivialRepeatedField32)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 5)
                   (HsProtobuf.PackedVec trivialRepeatedField64)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 6)
                   (HsProtobuf.Nested trivialNestedMessage))])
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
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 4)))
              <*>
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 5)))
              <*>
              ((Hs.pure HsProtobuf.nested) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 6)))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "trivialField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Fixed32)
                (HsProtobuf.Single "trivialFieldF32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 3)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "trivialField64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 4)
                (HsProtobuf.Repeated HsProtobuf.Int32)
                (HsProtobuf.Single "repeatedField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 5)
                (HsProtobuf.Repeated HsProtobuf.Int64)
                (HsProtobuf.Single "repeatedField64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 6)
                (HsProtobuf.Prim (HsProtobuf.Named (HsProtobuf.Single "Nested")))
                (HsProtobuf.Single "nestedMessage")
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
-- Instance for Trivial_Nested (these will be generated eventually)

instance A.ToJSON Trivial_Nested where
  toEncoding (Trivial_Nested x) = A.pairs . mconcat $
    [ encodeField "nestedField64" x
    ]

instance A.FromJSON Trivial_Nested where
  parseJSON = A.withObject "Trivial_Nested" $ \obj ->
    pure Trivial_Nested
    <*> decodeField obj "nestedField64"

--------------------------------------------------------------------------------
-- Instance for Trivial (these instances will be generated eventually)

-- newtype F32 = F32 (HsProtobuf.Fixed Hs.Word32) deriving (Eq)

instance A.ToJSON Trivial where
  toEncoding (Trivial i32 (f32) i64 v32 v64 mnest) = A.pairs . mconcat $
    [ encodeField "trivialField32"  i32
    , encodeField "trivialFieldF32" f32 -- HERE: fixed32 field
    , encodeField "trivialField64"  i64
    , encodeField "repeatedField32" v32
    , encodeField "repeatedField64" v64
    , encodeNested "nestedMessage" mnest
    ]

instance PBRep (HsProtobuf.Fixed Hs.Word32) where
  data PBR (HsProtobuf.Fixed Hs.Word32) = PBFixed32 (HsProtobuf.Fixed Hs.Word32) deriving (Show, Generic)
  toPBR                                 = PBFixed32
  fromPBR (PBFixed32 x)                 = x
instance Monoid (PBR (HsProtobuf.Fixed Hs.Word32)) where
  mempty                                = toPBR (HsProtobuf.Fixed 0)
  mappend (fromPBR -> 0) (fromPBR -> y) = toPBR y
  mappend (fromPBR -> x) (fromPBR -> 0) = toPBR x
  mappend _               x             = x

instance A.ToJSON (PBR (HsProtobuf.Fixed Hs.Word32))

-- orphan instance for this, test encoding to see if anything got fucked up.
instance A.ToJSON (HsProtobuf.Fixed Hs.Word32) where
  -- HERE: FUUUUUCK! So it's the yiffff line that gets executed, which means
  -- that we need to (a) figure out whether we should be overloading toJSON or
  -- toEncoding or both -- I think we may have to implement both? (b) newtype
  -- wrap this locally so that it is not an orphan instance and we don't have to
  -- burden it with a ToJSON instance of its own.

  -- don't forget this piece -- when is it sufficient to only overload
  -- toEncoding!?

  toJSON = trace ("yiffffff") . A.toJSON . HsProtobuf.fixed
  toEncoding = trace ("yaaaaaaaaaaar") . A.toEncoding . HsProtobuf.fixed

instance A.FromJSON Trivial where
  parseJSON = A.withObject "Trivial" $ \obj ->
    pure Trivial
    <*> decodeField obj "trivialField32"
    <*> return 0 -- HERE: fixed32 field FIXME
    <*> decodeField obj "trivialField64"
    <*> decodeField obj "repeatedField32"
    <*> decodeField obj "repeatedField64"
    <*> decodeNested obj "nestedMessage"

--------------------------------------------------------------------------------
-- PB <-> JSON

-- | The class of types which have a "canonical protobuf to JSON encoding"
-- defined. We make use of a data family called PBR ("protobuf rep") to map
-- primitive types to/from their corresponding type wrappers.
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
  mempty                       = PBVec mempty
  mappend (PBVec v0)(PBVec v1) = PBVec (mappend v0 v1)

instance (Eq a, PBRep a) => Eq (PBR a) where
  (fromPBR -> a) == (fromPBR -> b) = a == b

-- NB: Can probably quite safely abstract Vector a to Functor f => f a.
-- https://www.reddit.com/r/haskell/comments/64iuia/when_is_undecidableinstances_safe/

instance (A.FromJSON (PBR a), PBRep a) => A.FromJSON (PBR (Hs.Vector a)) where
  parseJSON = fmap (toPBR . fmap fromPBR) . A.parseJSON

instance (A.ToJSON (PBR a), PBRep a) => A.ToJSON (PBR (Hs.Vector a)) where
  toJSON = A.toJSON . fmap toPBR . fromPBR

--------------------------------------------------------------------------------
-- PBRep Int32
--
-- int32, fixed32, uint32: JSON value will be a decimal number. Either numbers
-- or strings are accepted.

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
  parseJSON v            = A.typeMismatch "PBInt32" v

-- instance PBRep (HsProtobuf.Fixed Hs.Word32) where
--   data PBR (HsProtobuf.Fixed Hs.Word32) = PBFixed32 deriving (Show, Generic)
--   toPBR                                 = PBFixed32
--   fromPBR (PBFixed32 x)                 = x

-- instance A.ToJSON (PBR (HsProtobuf.Fixed Hs.Word32))
-- instance PBRep (HsProtobuf.Fixed Hs.Word32)


--------------------------------------------------------------------------------
-- PBRep Int64
--
-- int64, fixed64, uint64: JSON value will be a decimal string. Either numbers
-- or strings are accepted.

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

--------------------------------------------------------------------------------
-- Helpers

encodeField :: (Eq a, A.KeyValue m, Monoid m, Monoid (PBR a), A.ToJSON (PBR a), PBRep a)
          => Hs.Text -> a -> m
encodeField lab (toPBR -> x) = if x == mempty then mempty else lab .= x

encodeNested :: (A.KeyValue m, A.ToJSON a, Monoid m) => Hs.Text -> Maybe a -> m
encodeNested fldSel = foldMap (fldSel .=)

decodeField :: (A.FromJSON (PBR a), Monoid (PBR a), PBRep a) => A.Object -> Hs.Text -> A.Parser a
decodeField o fldSel = fromPBR <$> (o .:? fldSel .!= Hs.mempty)

decodeNested :: A.FromJSON a => A.Object -> Hs.Text -> A.Parser (Maybe a)
decodeNested o fldSel = o .:? fldSel .!= Nothing

fromDecimalString :: (A.FromJSON a, PBRep a) => Hs.Text -> A.Parser (PBR a)
fromDecimalString
  = either fail (pure . toPBR)
  . A.eitherDecode
  . LBS.fromStrict
  . Hs.encodeUtf8

roundTrip :: (A.ToJSON a, A.FromJSON a, Eq a) => a -> Either String Bool
roundTrip x = either Left (Right . (x==)) . jsonToPB . pbToJSON $ x

pbToJSON :: A.ToJSON a => a -> LBS.ByteString
pbToJSON = A.encode

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

-- INPR: We should hand-elaborate a little further and make sure we have a
-- strategy for dealing with message nesting, repetition, packed vecs, etc.,
-- before we push too much farther on the generic parser! I.e., we should be
-- able to write clean/simple "brute force" codecs before we worry about the
-- generic parser.
--
-- [x] Let's make sure we can do field nesting and repeating with our current
-- approach.
--
-- [ ] HERE: Then let's extend Int32/Int64 support for the fixed/unsigned variants as
-- well and make sure we don't have any uncomfortable overlap or design flaws
-- for types which are encoded the same way...
--
-- [ ] And then let's try our hand at gParseJSONPB slowly.
