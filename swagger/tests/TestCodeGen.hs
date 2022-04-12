{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TestCodeGen where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as LBS8
import           Data.Proxy                     (Proxy(..))
import           Data.Swagger                   (ToSchema)
import qualified Data.Swagger

-- * Doctests for Swagger

-- $setup
-- >>> import qualified Data.Text.Lazy as TL
-- >>> import qualified Data.Vector    as V
-- >>> import Proto3.Suite
-- >>> import Proto3.Suite.JSONPB
-- >>> import TestProto
-- >>> import TestProtoOneof
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeApplications
-- >>> let jsonPB = jsonPBOptions
-- >>> let json = defaultOptions

-- | Swagger
--
-- >>> schemaOf @Something
-- {"properties":{"value":{"maximum":9223372036854775807,"format":"int64","minimum":-9223372036854775808,"type":"integer"},"another":{"maximum":2147483647,"format":"int32","minimum":-2147483648,"type":"integer"},"pickOne":{"$ref":"#/definitions/SomethingPickOne"}},"type":"object"}
-- >>> schemaOf @SomethingPickOne
-- {"properties":{"name":{"type":"string"},"someid":{"maximum":2147483647,"format":"int32","minimum":-2147483648,"type":"integer"},"dummyMsg1":{"$ref":"#/definitions/DummyMsg"},"dummyMsg2":{"$ref":"#/definitions/DummyMsg"},"dummyEnum":{"$ref":"#/definitions/DummyEnum"}},"maxProperties":1,"minProperties":1,"type":"object"}
-- >>> schemaOf @DummyMsg
-- {"properties":{"dummy":{"maximum":2147483647,"format":"int32","minimum":-2147483648,"type":"integer"}},"type":"object"}
-- >>> schemaOf @(Enumerated DummyEnum)
-- {"type":"string","enum":["DUMMY0","DUMMY1"]}
--

schemaOf :: forall a . ToSchema a => IO ()
schemaOf = LBS8.putStrLn (Data.Aeson.encode (Data.Swagger.toSchema (Proxy @a)))
