module Proto3.Suite.DotProto.JSONPB
  ( -- * Typeclasses
    ToJSONPB(..)
  , FromJSONPB(..)
  , (.:)
  , encode
  , eitherDecode
  )
where

import Proto3.Suite.DotProto.JSONPB.Class
