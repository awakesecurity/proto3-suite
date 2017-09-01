{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Common instances for jsonpb codec implementations

module Proto3.Suite.DotProto.JSONPB.Instances where

import qualified Data.Aeson                         as A (ToJSON (..),
                                                          Value (..))
import qualified Data.Aeson.Encoding                as E
import qualified Data.Aeson.Types                   as A (typeMismatch)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base64             as B64
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Vector                        as V
import           GHC.Int                            (Int32, Int64)
import           GHC.Word                           (Word32, Word64)
import           Proto3.Suite.Class                 (HasDefault(..), Named(..))
import           Proto3.Suite.DotProto.JSONPB.Class
import           Proto3.Suite.Types                 (Enumerated(..), Fixed (..))

-- * Instances for scalar types

--------------------------------------------------------------------------------
-- Boolean scalar type

instance ToJSONPB Bool
instance FromJSONPB Bool

--------------------------------------------------------------------------------
-- Integer scalar types
--
--   * 32 bit integer values render to JSON decimal numbers; either numbers or
--     strings are accepted.
--
--   * 64 bit integer values render to JSON decimal strings; either numbers
--     or strings are accepted.
--

-- int32 / sint32
instance ToJSONPB Int32
instance FromJSONPB Int32 where
  parseJSONPB = parseNumOrDecimalString "int32 / sint32"

-- uint32
instance ToJSONPB Word32
instance FromJSONPB Word32 where
  parseJSONPB = parseNumOrDecimalString "uint32"

-- int64 / sint64
instance ToJSONPB Int64 where
  toEncodingPB _ = E.string . show
instance FromJSONPB Int64 where
  parseJSONPB = parseNumOrDecimalString "int64 / sint64"

-- unit64
instance ToJSONPB Word64 where
  toEncodingPB _ = E.string . show
instance FromJSONPB Word64 where
  parseJSONPB = parseNumOrDecimalString "int64 / sint64"

-- fixed32
instance ToJSONPB (Fixed Word32) where
  toEncodingPB opts = toEncodingPB opts . fixed
instance FromJSONPB (Fixed Word32) where
  parseJSONPB = fmap Fixed . parseJSONPB

-- fixed64
instance ToJSONPB (Fixed Word64) where
  toEncodingPB opts = toEncodingPB opts . fixed
instance FromJSONPB (Fixed Word64) where
  parseJSONPB = fmap Fixed . parseJSONPB

-- sfixed32
instance ToJSONPB (Fixed Int32) where
  toEncodingPB opts = toEncodingPB opts . fixed
instance FromJSONPB (Fixed Int32) where
  parseJSONPB = fmap Fixed . parseJSONPB

-- sfixed64
instance ToJSONPB (Fixed Int64) where
  toEncodingPB opts = toEncodingPB opts . fixed
instance FromJSONPB (Fixed Int64) where
  parseJSONPB = fmap Fixed . parseJSONPB

--------------------------------------------------------------------------------
-- Floating point scalar types
--
-- JSON value will be a number or one of the special string values "NaN",
-- "Infinity", and "-Infinity". Either numbers or strings are accepted. Exponent
-- notation is also accepted.

-- float
instance ToJSONPB Float
instance FromJSONPB Float where
  parseJSONPB = parseFP "float"

-- double
instance ToJSONPB Double
instance FromJSONPB Double where
  parseJSONPB = parseFP "double"

--------------------------------------------------------------------------------
-- Stringly types (string and bytes)

-- string
instance ToJSONPB TL.Text
instance FromJSONPB TL.Text

-- bytes
instance ToJSONPB BS.ByteString where
  toEncodingPB _ bs = case T.decodeUtf8' (B64.encode bs) of
    Left e  -> error ("internal: failed to encode B64-encoded bytestring: " ++ show e)
               -- T.decodeUtf8' should never fail because we B64-encode the
               -- incoming bytestring.
    Right t -> E.value (A.toJSON t)

instance FromJSONPB BS.ByteString where
  parseJSONPB (A.String b64enc) = pure . B64.decodeLenient . T.encodeUtf8 $ b64enc
  parseJSONPB v                 = A.typeMismatch "bytes" v

--------------------------------------------------------------------------------
-- Enumerated types

instance (Named a, Show a, ToJSONPB a) => ToJSONPB (Enumerated a) where
  toEncodingPB opts (Enumerated e) = case e of
    Right x -> toEncodingPB opts x
    Left  0 ->
      {- TODO: Raise a compilation error when the first enum value in an
               enumeration is not zero.

         The proto3 spec states that the default value is the first defined enum
         value, which must be 0. Since we currently don't raise a compilation
         error for this like we should, we have to handle this case.

         For now, die horribly to mimic what should be a compilation error.
      -}
      error "toEncodingPB Enumerated: The first enum value must be zero in proto3"
    Left{}  ->
      {- From the JSONPB spec:

           If a value is missing in the JSON-encoded data or if its value is
           null, it will be interpreted as the appropriate default value when
           parsed into a protocol buffer.

         Thus, interpreting a wire value out of enum range as "missing", we
         yield the null encoding here to mean the default value.
       -}
      E.null_

instance (Bounded a, Enum a, FromJSONPB a) => FromJSONPB (Enumerated a) where
  parseJSONPB A.Null = pure def -- So CG does not have to handle this case in
                                -- every generated instance
  parseJSONPB v      = Enumerated . Right <$> parseJSONPB v

-- * Instances for composite types

--------------------------------------------------------------------------------
-- Instances for repeated messages
--
-- JSON value will be the vector elements encoded as a JSON array. The null
-- value is accepted as the empty list, @[]@.

instance ToJSONPB a => ToJSONPB (V.Vector a) where
  toEncodingPB opts = E.list (toEncodingPB opts) . V.toList
instance FromJSONPB a => FromJSONPB (V.Vector a) where
  parseJSONPB (A.Array vs) = mapM parseJSONPB vs
  parseJSONPB A.Null       = pure []
  parseJSONPB v            = A.typeMismatch "repeated" v

--------------------------------------------------------------------------------
-- Instances for nested messages

instance ToJSONPB a => ToJSONPB (Maybe a) where
  toEncodingPB opts = maybe E.null_ (toEncodingPB opts)
instance FromJSONPB a => FromJSONPB (Maybe a) where
  parseJSONPB A.Null = pure Nothing
  parseJSONPB v      = fmap Just (parseJSONPB v)
