{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Google.Protobuf.Wrappers.Polymorphic
  ( Wrapped(..)
  , NameOfWrapperFor(..)
  ) where

import           Control.DeepSeq (NFData)
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.Int (Int32, Int64)
import           Data.String (IsString(..))
import qualified Data.Text (Text)
import qualified Data.Text.Lazy (Text)
import           Data.Word (Word32, Word64)
import           GHC.Exts (Proxy#, proxy#)
import           GHC.Generics (Generic)
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.DotProto as HsProtobuf
import           Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Suite.JSONPB as HsJSONPB
import qualified Proto3.Wire as HsProtobuf

-- | A Haskell type representing the standard protobuf wrapper
-- message that is associated with the given Haskell type.
--
-- Note that if Google ever adds wrappers for "sint..." or "..fixed..."
-- then this newtype could still be used, provided its type parameter
-- involves the appropriate combination of `Proto3.Suite.Types.Signed`
-- and/or `Proto3.Suite.Types.Fixed`.  Because the latter two types
-- are themselves newtypes, the corresponding coercions should work.
newtype Wrapped a = Wrapped a
  deriving (Show, Eq, Ord, Generic, NFData)

instance NameOfWrapperFor a => HsProtobuf.Named (Wrapped a) where
  nameOf _ = nameOfWrapperFor (proxy# :: Proxy# a)
  {-# INLINE nameOf #-}

instance (HsProtobuf.HasDefault a, Eq a) =>
         HsProtobuf.HasDefault (Wrapped a)

instance (HsProtobuf.MessageField a, HsProtobuf.Primitive a) =>
         HsProtobuf.Message (Wrapped a) where
  encodeMessage _ (Wrapped v) =
    HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1) v
  {-# INLINABLE encodeMessage #-}
  decodeMessage _ = Wrapped <$>
    HsProtobuf.at HsProtobuf.decodeMessageField (HsProtobuf.FieldNumber 1)
  {-# INLINABLE decodeMessage #-}
  dotProto _ =
    [ HsProtobuf.DotProtoField
        (HsProtobuf.FieldNumber 1)
        (HsProtobuf.Prim (HsProtobuf.primType (proxy# :: Proxy# a)))
        (HsProtobuf.Single "value")
        []
        ""
    ]

instance (HsJSONPB.ToJSONPB a, HsProtobuf.HasDefault a) =>
         HsJSONPB.ToJSONPB (Wrapped a) where
  toJSONPB (Wrapped f1) = HsJSONPB.object ["value" .= f1]
  {-# INLINABLE toJSONPB #-}
  toEncodingPB (Wrapped f1) = HsJSONPB.pairs ["value" .= f1]
  {-# INLINABLE toEncodingPB #-}

instance (HsJSONPB.FromJSONPB a, HsProtobuf.HasDefault a, NameOfWrapperFor a) =>
         HsJSONPB.FromJSONPB (Wrapped a) where
  parseJSONPB = HsJSONPB.withObject
    (nameOfWrapperFor (proxy# :: Proxy# a))
    (\obj -> Wrapped <$> obj .: "value")
  {-# INLINABLE parseJSONPB #-}

instance (HsJSONPB.ToJSONPB a, HsProtobuf.HasDefault a) =>
         HsJSONPB.ToJSON (Wrapped a) where
  toJSON = HsJSONPB.toAesonValue
  {-# INLINE toJSON #-}
  toEncoding = HsJSONPB.toAesonEncoding
  {-# INLINE toEncoding #-}

instance (HsJSONPB.FromJSONPB a, HsProtobuf.HasDefault a, NameOfWrapperFor a) =>
         HsJSONPB.FromJSON (Wrapped a) where
  parseJSON = HsJSONPB.parseJSONPB
  {-# INLINE parseJSON #-}

-- | Defines the name to be returned by @`HsProtobuf.Named` (`Wrapped` a)@.
class NameOfWrapperFor a where
  nameOfWrapperFor :: IsString string => Proxy# a -> string

instance NameOfWrapperFor Double where
  nameOfWrapperFor _ = "DoubleValue"

instance NameOfWrapperFor Float where
  nameOfWrapperFor _ = "FloatValue"

instance NameOfWrapperFor Int64 where
  nameOfWrapperFor _ = "Int64Value"

instance NameOfWrapperFor Word64 where
  nameOfWrapperFor _ = "UInt64Value"

instance NameOfWrapperFor Int32 where
  nameOfWrapperFor _ = "Int32Value"

instance NameOfWrapperFor Word32 where
  nameOfWrapperFor _ = "UInt32Value"

instance NameOfWrapperFor Bool where
  nameOfWrapperFor _ = "BoolValue"

instance NameOfWrapperFor Data.Text.Text where
  nameOfWrapperFor _ = "StringValue"

instance NameOfWrapperFor Data.Text.Lazy.Text where
  nameOfWrapperFor _ = "StringValue"

instance NameOfWrapperFor Data.ByteString.ByteString where
  nameOfWrapperFor _ = "BytesValue"

instance NameOfWrapperFor Data.ByteString.Lazy.ByteString where
  nameOfWrapperFor _ = "BytesValue"
