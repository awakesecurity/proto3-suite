module Proto3.Suite.DotProto.JSONPB
  ( -- * Typeclasses
    FromJSONPB(..)
  , ToJSONPB(..)
    -- * Operators
  , (.:)
  , (.=)
    -- * Aeson re-exports
  , A.withObject
    -- * jsonpb codec entry points
  , eitherDecode
  , encode
    -- * Helper functions
  , fieldsPB
  )
where

import qualified Data.Aeson                             as A
import           Proto3.Suite.DotProto.JSONPB.Class
import           Proto3.Suite.DotProto.JSONPB.Instances ()
