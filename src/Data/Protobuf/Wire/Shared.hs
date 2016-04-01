{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Protobuf.Wire.Shared (
-- * Message Structure
  FieldNumber(..)
, fieldNumber
, WireType(..)
  ) where

import           Data.Word (Word8, Word32, Word64)

-- | A 'FieldNumber' identifies a field inside a protobufs message.
--
-- This library makes no attempt to generate these automatically, or even make
-- sure that field numbers are provided in increasing order. Such things are
-- left to other, higher-level libraries.
newtype FieldNumber = FieldNumber { getFieldNumber :: Word64 } deriving (Show, Eq, Ord, Enum)

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
