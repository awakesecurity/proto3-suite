{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Protobuf.Wire.Shared (
-- * Message Structure
  FieldNumber(..)
, fieldNumber
, WireType(..)
, ProtobufMerge(..)

-- * Integral Types
, Fixed(..)
, Signed(..)
  ) where

import           Control.DeepSeq (NFData)
import           Data.Word (Word8, Word32, Word64)
import           GHC.Generics

-- | A 'FieldNumber' identifies a field inside a protobufs message.
--
-- This library makes no attempt to generate these automatically, or even make
-- sure that field numbers are provided in increasing order. Such things are
-- left to other, higher-level libraries.
newtype FieldNumber = FieldNumber { getFieldNumber :: Word64 } deriving (Show, Eq, Ord, Enum, NFData)

-- | Create a 'FieldNumber' given the (one-based) integer which would label
-- the field in the corresponding .proto file.
fieldNumber :: Word64 -> FieldNumber
fieldNumber = FieldNumber

data WireType
  = Varint
  | Fixed32
  | Fixed64
  | LengthDelimited
  deriving (Show, Eq, Ord)

-- | Class for merging protobuf messages. Merging protobuf messages must satisfy
-- the following:
-- 1. `protobufMerge x y` overwrites the singular fields of x with those of y.
-- 2. `protobufMerge x y` recurses on the embedded messages in x and y.
-- 3. `protobufMerge x y` concatenates all list fields in x and y.
class ProtobufMerge a where
  protobufMerge :: a -> a -> a

  -- | 'Fixed' provides a way to encode integers in the fixed-width wire formats.
newtype Fixed a = Fixed { fixed :: a } deriving (Show, Eq, Ord, Generic, NFData)

-- | 'Signed' provides a way to encode integers in the signed wire formats.
newtype Signed a = Signed { signed :: a } deriving (Show, Eq, Ord, Generic, NFData)
