{-# LANGUAGE CPP                       #-}

module Proto3.Suite.JSONPB
  ( -- * Typeclasses
    FromJSONPB(..)
  , ToJSONPB(..)
    -- * Operators
  , (.:)
  , (.=)
    -- * Options for controlling codec behavior (e.g., emitting default-valued
    --   fields in JSON payloads)
  , Options(..)
  , defaultOptions
  , jsonPBOptions
    -- * JSONPB codec entry points
  , eitherDecode
  , encode
    -- * Helper functions
  , enumFieldEncoding
  , enumFieldString
  , object
  , objectOrNull
  , pair
  , pairs
  , pairsOrNull
  , parseField
  , toAesonEncoding
  , toAesonValue
    -- * Aeson re-exports
  , A.Value(..)
  , A.ToJSON(..)
  , A.FromJSON(..)
  , A.typeMismatch
  , A.withObject
#ifdef SWAGGER
    -- * Swagger schema helpers
  , Swagger.ToSchema(..)
  , Swagger.NamedSchema(..)
  , Swagger.Schema(..)
  , Swagger.ParamSchema(..)
  , Swagger.SwaggerType(..)
  , Swagger.declareSchemaRef
  , Proto3.Suite.DotProto.Generate.Swagger.asProxy
  , Proto3.Suite.DotProto.Generate.Swagger.insOrdFromList
#endif
  )
where

import qualified Data.Aeson                             as A
import qualified Data.Aeson.Types                       as A
#ifdef SWAGGER
import qualified Data.Swagger                           as Swagger
import           Proto3.Suite.DotProto.Generate.Swagger
#endif
import           Proto3.Suite.JSONPB.Class
