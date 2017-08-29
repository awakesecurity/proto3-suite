{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Common instances for jsonpb codec implementations

module Proto3.Suite.DotProto.JSONPB.Instances where

import qualified Data.Aeson                         as A (FromJSON (..),
                                                          FromJSONKey (..),
                                                          FromJSONKeyFunction (..),
                                                          ToJSON (..),
                                                          Value (..),
                                                          eitherDecode)
import qualified Data.Aeson.Encoding                as E (pair)
import qualified Data.Aeson.Types                   as A (Parser, Pair, Series, typeMismatch)
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base64             as B64
import           Data.Maybe                         (isNothing)
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.Encoding            as TL
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Vector                        as V
import           GHC.Int                            (Int32, Int64)
import           GHC.Word                           (Word32, Word64)

import           Proto3.Suite.Class                 (HasDefault (def, isDefault))
import           Proto3.Suite.DotProto.JSONPB.Class (FromJSONPB (..),
                                                     KeyValuePB (..),
                                                     ToJSONPB (..))
import           Proto3.Suite.Types                 (Fixed (..))

-- | This instance allows us to use @key .= val@ with the correct jsonpb
-- semantics (default values are omitted via 'mempty) in 'toJSONPB'
-- implementations.
instance KeyValuePB [A.Pair] where
  k .= v
    | isDefault v = mempty
    | otherwise   = [(k, toJSONPB v)]

-- | This instance allows us to use @key .= val@ with the correct jsonpb
-- semantics (default values are omitted via 'mempty') in 'toEncodingPB'
-- implementations.
instance KeyValuePB A.Series where
  k .= v
    | isDefault v = mempty
    | otherwise   = E.pair k (toEncodingPB v)

--------------------------------------------------------------------------------
-- Integer scalar types
--
--   * 32 bit integer values render to JSON decimal numbers; either numbers or
--     strings are accepted.
--
--   * 64 bit integer values render to JSON decimal strings; either numbers
--     or strings are accepted.
--

-- NB: FIXME: I don't think sint32/sint64 should overlap with int32/int64
-- (possibly bug in CG); no Signed wrapper is generated? Likewise for
-- sfixed32/sfixed64: CG might be buggy here as well. NB: There are no
-- HasDefault instances for e.g. (Fixed Int32) which is another indication that
-- this is a bug, because there IS a HasDefault instance for (Signed (Fixed
-- Int32)) which I think is what should be generated here (representatively). As
-- a stopgap we'll go ahead and define some placeholder instances for (Fixed
-- Int32) and their ilk so we can make progress (see PLACEHOLDER INSTANCES down
-- below) -- but we should nuke these as soon as we fix CG! I think something
-- similiar might be happening for PackedVec/NestedVec/Nested/etc as well.

-- int32 / "sint32"
instance ToJSONPB Int32
instance FromJSONPB Int32 where
  parseJSONPB = parseNumOrDecimalString "int32 / sint32"

-- uint32
instance ToJSONPB Word32
instance FromJSONPB Word32 where
  parseJSONPB = parseNumOrDecimalString "uint32"

-- int64 / "sint64"
instance ToJSONPB Int64 where
  toJSONPB = showDecimalString
instance FromJSONPB Int64 where
  parseJSONPB = parseNumOrDecimalString "int64 / sint64"

instance ToJSONPB Word64 where
  toJSONPB = showDecimalString

instance FromJSONPB Word64 where
  parseJSONPB = parseNumOrDecimalString "int64 / sint64"

-- fixed32, fixed64, "sfixed32", "sfixed64"
instance ToJSONPB a => ToJSONPB (Fixed a) where
  toJSONPB = toJSONPB . fixed
instance FromJSONPB a => FromJSONPB (Fixed a) where
  parseJSONPB = fmap Fixed . parseJSONPB

--------------------------------------------------------------------------------
-- PLACEHOLDER INSTANCES
instance HasDefault (Fixed Int32)
instance HasDefault (Fixed Int64)

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
  toJSONPB bs = case T.decodeUtf8' (B64.encode bs) of
    Left e  -> error ("internal: failed to encode B64-encoded bytestring: " ++ show e)
               -- T.decodeUtf8' should never fail because we B64-encode the
               -- incoming bytestring.
    Right t -> A.toJSON t
instance FromJSONPB BS.ByteString where
  parseJSONPB (A.String b64enc) = pure . B64.decodeLenient . T.encodeUtf8 $ b64enc
  parseJSONPB v                 = A.typeMismatch "bytes" v

--------------------------------------------------------------------------------
-- Instances for repeated messages
--
-- JSON value will be the vector elements encoded as a JSON array. The null
-- value is accepted as the empty list, @[]@.

instance ToJSONPB a => ToJSONPB (V.Vector a) where
  toJSONPB = A.Array . fmap toJSONPB
instance FromJSONPB a => FromJSONPB (V.Vector a) where
  parseJSONPB (A.Array vs) = mapM parseJSONPB vs
  parseJSONPB A.Null       = pure []
  parseJSONPB v            = A.typeMismatch "repeated" v

-- As with the PLACEHOLDER INSTANCES for fixed/signed types, we have a similar
-- situation here; we're not getting NestedVec-wrapped values in CG (or
-- whatever) as I might expect would be the case...so we probably need to treat
-- this as as stopgap as well.
instance HasDefault (V.Vector a) where
  def       = V.empty
  isDefault = V.null

--------------------------------------------------------------------------------
-- Instances for nested messages

instance ToJSONPB a => ToJSONPB (Maybe a) where
  toJSONPB = maybe A.Null toJSONPB
instance FromJSONPB a => FromJSONPB (Maybe a) where
  parseJSONPB A.Null = pure Nothing
  parseJSONPB v      = fmap Just (parseJSONPB v)

-- As with the PLACEHOLDER INSTANCES for fixed/signed types, we have a similar
-- situation here; we're not getting Nested-wrapped values in CG as I might
-- expect would be the case; so we probably need to treat this as as stopgap as
-- well. Basically I'm not sure why this is a Maybe instead of Nested.

instance HasDefault (Maybe a) where
  def       = Nothing
  isDefault = isNothing

--------------------------------------------------------------------------------
-- Helpers

parseFP :: (A.FromJSON a, A.FromJSONKey a) => String -> A.Value -> A.Parser a
parseFP tyDesc v = case v of
  A.Number{} -> A.parseJSON v
  A.String t -> case A.fromJSONKey of
                  A.FromJSONKeyTextParser p
                    -> p t
                  _ -> fail "internal: parseKeyPB: unexpected FromJSONKey summand"
  _          -> A.typeMismatch tyDesc v

parseNumOrDecimalString :: (A.FromJSON a) => String -> A.Value -> A.Parser a
parseNumOrDecimalString tyDesc v = case v of
  A.Number{} -> A.parseJSON v
  A.String t -> either fail pure . A.eitherDecode . TL.encodeUtf8 . TL.fromStrict $ t
  _          -> A.typeMismatch tyDesc v

showDecimalString :: Show a => a -> A.Value
showDecimalString = A.String . T.pack . show
