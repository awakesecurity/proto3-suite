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
                       trivialTrivialField64 :: Hs.Int64,
                       trivialRepeatedField32 :: Hs.Vector Hs.Int32,
                       trivialRepeatedField64 :: Hs.Vector Hs.Int64}
             deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)

instance HsProtobuf.Named Trivial where
        nameOf _ = (Hs.fromString "Trivial")

instance HsProtobuf.Message Trivial where
        encodeMessage _
          Trivial{trivialTrivialField32 = trivialTrivialField32,
                  trivialTrivialField64 = trivialTrivialField64,
                  trivialRepeatedField32 = trivialRepeatedField32,
                  trivialRepeatedField64 = trivialRepeatedField64}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   trivialTrivialField32),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   trivialTrivialField64),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 3)
                   (HsProtobuf.PackedVec trivialRepeatedField32)),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 4)
                   (HsProtobuf.PackedVec trivialRepeatedField64))])
        decodeMessage _
          = (Hs.pure Trivial) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
              <*>
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 3)))
              <*>
              ((Hs.pure HsProtobuf.packedvec) <*>
                 (HsProtobuf.at HsProtobuf.decodeMessageField
                    (HsProtobuf.FieldNumber 4)))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "trivialField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Int64)
                (HsProtobuf.Single "trivialField64")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 3)
                (HsProtobuf.Repeated HsProtobuf.Int32)
                (HsProtobuf.Single "repeatedField32")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 4)
                (HsProtobuf.Repeated HsProtobuf.Int64)
                (HsProtobuf.Single "repeatedField64")
                []
                Hs.Nothing)]

--------------------------------------------------------------------------------
-- Instance for Trivial (these instances will be generated eventually)

      -- encodeFld :: forall a t.
      --              (PBRep a, A.KeyValue t, A.ToJSON (PBR a), Monoid (PBR a), Monoid t,
      --               Eq a) =>
      --              Text.Text -> a -> t

encodeFld :: (Eq a, A.KeyValue m, Monoid m, Monoid (PBR a), A.ToJSON (PBR a), PBRep a)
          => Hs.Text -> a -> m
encodeFld lab (toPBR -> x) = if x == mempty then mempty else lab .= x

instance A.ToJSON Trivial where
  toEncoding (Trivial x y v32 v64) = A.pairs . mconcat $
    [ encodeFld "trivialField32"  x
    , encodeFld "trivialField64"  y
    , encodeFld "repeatedField32" v32
    , encodeFld "repeatedField64" v64
    -- older
    --   if toPBR   x == mempty then mempty else "trivialField32"  .= toPBR x
    -- , if toPBR   y == mempty then mempty else "trivialField64"  .= toPBR y
    -- , if toPBR v32 == mempty then mempty else "repeatedField32" .= toPBR v32
    -- , if toPBR v64 == mempty then mempty else "repeatedField64" .= toPBR v64

    -- older:
    -- , "repeatedField32" .= (toPBR <$> v32)
    -- , "repeatedField64" .= (toPBR <$> v64)
    ]

instance A.FromJSON Trivial where
  parseJSON = A.withObject "Trivial" Hs.$ \obj ->
    pure Trivial
    <*> pbFld obj "trivialField32"
    <*> pbFld obj "trivialField64"
    <*> pbFld obj "repeatedField32"
    <*> pbFld obj "repeatedField64"
    -- <*> do fromPBR <$> pbFld obj "repeatedField32"
    -- <*> do fromPBR <$> pbFld obj "repeatedField64"

--------------------------------------------------------------------------------
-- PB <-> JSON

-- | The class of types which have a "canonical protobuf to JSON encoding"
-- defined. We make use of a data family called PBR ("protobuf rep") to map
-- primitive types to/from their corresponding type wrappers.
class PBRep a where
  data PBR a
  toPBR   :: a -> PBR a
  fromPBR :: PBR a -> a

instance (Functor f, PBRep a) => PBRep (f a) where
  data PBR (f a)     = PBReps (f (PBR a))
  toPBR v            = PBReps (toPBR <$> v)
  fromPBR (PBReps v) = fromPBR <$> v

instance (Functor f, Monoid (f a), PBRep a) => Monoid (PBR (f a)) where
  mempty                                = toPBR mempty
  mappend (fromPBR -> x) (fromPBR -> y) = toPBR (mappend x y)

instance (Eq a, PBRep a) => Eq (PBR a) where
  (fromPBR -> a) == (fromPBR -> b) = a == b

instance (A.FromJSON (PBR a), PBRep a) => A.FromJSON (PBR (Hs.Vector a)) where
  parseJSON = fmap (toPBR . fmap fromPBR) . A.parseJSON

instance (A.ToJSON (PBR a), PBRep a) => A.ToJSON (PBR (Hs.Vector a)) where
  toJSON = A.toJSON . fmap toPBR . fromPBR

--------------------------------------------------------------------------------
-- PBRep Int32

instance PBRep Hs.Int32 where
  data PBR Hs.Int32  = PBInt32 Hs.Int32 deriving (Show, Generic)
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

--------------------------------------------------------------------------------
-- PBRep Int64

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

pbFld :: (A.FromJSON (PBR a), Monoid (PBR a), PBRep a) => A.Object -> Hs.Text -> A.Parser a
pbFld o fldSel = fromPBR <$> (o .:? fldSel .!= Hs.mempty)
-- pbFld :: (A.FromJSON a, Monoid a) => A.Object -> Hs.Text -> A.Parser a
-- pbFld o fldSel = o .:? fldSel .!= Hs.mempty

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
-- strategy for dealing with message nesting before we push too much farther on
-- the generic parser!

-- (0) HERE: Let's make sure we can do field nesting and repeating with our current
-- approach.
--
-- (1) Let's try writing the CollectFieldNames function in the form of a Generic
-- pass, and then try it out with Trivial and a few similar types to make sure
-- we understand it. Use http://www.stephendiehl.com/posts/generics.html as a
-- basic guide.
--
-- (2) And then let's try our hand at gParseJSONPB slowly.
