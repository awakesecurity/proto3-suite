-- | This module provides helper functions to generate Swagger schemas which
-- describe JSONPB encodings for protobuf types.

module Proto3.Suite.DotProto.Generate.Swagger
  ( ppSchema
  )
where

import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString.Lazy.Char8      as LC8
import           Data.Swagger
-- import qualified Data.Swagger.Declare            as Swagger
-- import qualified Data.Swagger.Internal.Schema    as Swagger.Internal
-- import qualified Data.Swagger.Internal.TypeShape as Swagger.Internal

-- | Pretty-prints a schema. Useful when playing around with schemas in the
-- REPL.
--
-- >>> ppSchema (Proxy @Enumerated MyEnum))
ppSchema :: ToSchema a => proxy a -> IO ()
ppSchema = LC8.putStrLn . encodePretty . toSchema
