{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- | This module has been initialized using
-- https://github.com/protocolbuffers/protobuf/blob/master/src/google/protobuf/timestamp.proto
--
-- The ToJSONPB and FromJSONPB instances have been modified to
-- be compatible with the json rfc3339 encoding
module Google.Protobuf.Timestamp where

import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Applicative as Hs
import qualified Control.DeepSeq as Hs
import qualified Control.Monad as Hs
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as Hs
import qualified Data.Coerce as Hs
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.List.NonEmpty as Hs (NonEmpty (..))
import qualified Data.Map as Hs (Map, mapKeysMonotonic)
import qualified Data.Proxy as Proxy
import qualified Data.String as Hs (fromString)
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Time.Format as Time
import qualified Data.Vector as Hs (Vector)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Enum as Hs
import qualified GHC.Generics as Hs
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobuf
import Proto3.Suite.JSONPB ((.:), (.=))
import qualified Proto3.Suite.JSONPB as HsJSONPB
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import qualified Unsafe.Coerce as Hs
import qualified Prelude as Hs

data Timestamp = Timestamp
  { timestampSeconds :: Hs.Int64,
    timestampNanos :: Hs.Int32
  }
  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic, Hs.NFData)

instance HsProtobuf.Named Timestamp where
  nameOf _ = Hs.fromString "Timestamp"

instance HsProtobuf.HasDefault Timestamp

instance HsProtobuf.Message Timestamp where
  encodeMessage
    _
    Timestamp
      { timestampSeconds = timestampSeconds,
        timestampNanos = timestampNanos
      } =
      Hs.mconcat
        [ HsProtobuf.encodeMessageField
            (HsProtobuf.FieldNumber 1)
            timestampSeconds,
          HsProtobuf.encodeMessageField
            (HsProtobuf.FieldNumber 2)
            timestampNanos
        ]

  decodeMessage _ =
    Timestamp
      <$> HsProtobuf.at
        HsProtobuf.decodeMessageField
        (HsProtobuf.FieldNumber 1)
      <*> HsProtobuf.at
        HsProtobuf.decodeMessageField
        (HsProtobuf.FieldNumber 2)

  dotProto _ =
    [ HsProtobuf.DotProtoField
        (HsProtobuf.FieldNumber 1)
        (HsProtobuf.Prim HsProtobuf.Int64)
        (HsProtobuf.Single "seconds")
        []
        "",
      HsProtobuf.DotProtoField
        (HsProtobuf.FieldNumber 2)
        (HsProtobuf.Prim HsProtobuf.Int32)
        (HsProtobuf.Single "nanos")
        []
        ""
    ]

rfc3339Format :: Hs.String
rfc3339Format = "%FT%TZ"

fromUTCTime :: Time.UTCTime -> Timestamp
fromUTCTime utc = Timestamp sec (Hs.fromInteger (Hs.toInteger nano))
  where
    Time.MkSystemTime sec nano = Time.utcToSystemTime utc

toUTCTime :: Timestamp -> Time.UTCTime
toUTCTime (Timestamp sec nano) = Time.systemToUTCTime systemTime
  where
    systemTime = Time.MkSystemTime sec (Hs.fromInteger (Hs.toInteger nano))

fromRFC3339 :: Hs.Text -> Hs.Maybe Timestamp
fromRFC3339 txt = do
  Time.MkSystemTime sec nano <- Time.utcToSystemTime <$> utcM
  Hs.pure (Timestamp sec (Hs.fromInteger (Hs.toInteger nano)))
  where
    utcM = Time.parseTimeM Hs.False Time.defaultTimeLocale rfc3339Format (Text.unpack txt)

toRFC3339 :: Timestamp -> Hs.Text
toRFC3339 ts = Text.pack (Time.formatTime Time.defaultTimeLocale rfc3339Format (toUTCTime ts))

instance HsJSONPB.ToJSONPB Timestamp where
  toJSONPB ts _opt = HsJSONPB.String (Text.toStrict (toRFC3339 ts))

instance HsJSONPB.FromJSONPB Timestamp where
  parseJSONPB = Aeson.withText "Timestamp" tryParse
    where
      tryParse txt = Hs.maybe Hs.mzero Hs.pure (fromRFC3339 (Text.fromStrict txt))

instance HsJSONPB.ToJSON Timestamp where
  toJSON = HsJSONPB.toAesonValue
  toEncoding = HsJSONPB.toAesonEncoding

instance HsJSONPB.FromJSON Timestamp where
  parseJSON = HsJSONPB.parseJSONPB

#ifdef SWAGGER
instance HsJSONPB.ToSchema Timestamp where
  declareNamedSchema _ =
    do
      let declare_seconds = HsJSONPB.declareSchemaRef
      timestampSeconds <- declare_seconds Proxy.Proxy
      let declare_nanos = HsJSONPB.declareSchemaRef
      timestampNanos <- declare_nanos Proxy.Proxy
      let _ =
            Timestamp <$> HsJSONPB.asProxy declare_seconds
              <*> HsJSONPB.asProxy declare_nanos
      Hs.return
        ( HsJSONPB.NamedSchema
            { HsJSONPB._namedSchemaName =
                Hs.Just "Timestamp",
              HsJSONPB._namedSchemaSchema =
                Hs.mempty
                  { HsJSONPB._schemaParamSchema =
                      Hs.mempty
                        { HsJSONPB._paramSchemaType =
                            Hs.Just HsJSONPB.SwaggerObject
                        },
                    HsJSONPB._schemaProperties =
                      HsJSONPB.insOrdFromList
                        [ ("seconds", timestampSeconds),
                          ("nanos", timestampNanos)
                        ]
                  }
            }
        )
#endif
