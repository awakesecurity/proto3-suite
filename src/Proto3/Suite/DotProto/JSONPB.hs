module Proto3.Suite.DotProto.JSONPB
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
    -- * JSONPB codec entry points
  , eitherDecode
  , encode
    -- * Helper functions
  , fieldsPB
    -- * Aeson re-exports
  , A.withObject
  )
where

import qualified Data.Aeson                             as A
import           Proto3.Suite.DotProto.JSONPB.Class
import           Proto3.Suite.DotProto.JSONPB.Instances ()
