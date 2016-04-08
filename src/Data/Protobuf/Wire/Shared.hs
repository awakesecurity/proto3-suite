{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Protobuf.Wire.Shared (
-- * Message Structure
  FieldNumber(..)
, fieldNumber
, WireType(..)
, ProtobufMerge(..)
, Fixed(..)
  ) where

import           Data.Word (Word8, Word32, Word64)
import           GHC.Generics

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

-- | Class for merging protobuf messages. Merging protobuf messages must satisfy
-- the following:
-- 1. `protobufMerge x y` overwrites the singular fields of x with those of y.
-- 2. `protobufMerge x y` recurses on the embedded messages in x and y.
-- 3. `protobufMerge x y` concatenates all list fields in x and y.
class ProtobufMerge a where
  protobufMerge :: a -> a -> a

  -- | 'Fixed' provides a way to encode integers in the fixed-width wire formats.
newtype Fixed a = Fixed { fixed :: a } deriving (Show, Eq, Ord, Generic)

-- The instances below are to make it easier to define ProtobufParsable
-- and ProtobufPackable instances for Fixed numeric types.

instance Num a => Num (Fixed a) where
  x + y = Fixed $ fixed x + fixed y
  x * y = Fixed $ fixed x + fixed y
  abs = Fixed . abs . fixed
  signum = Fixed . signum . fixed
  fromInteger = Fixed . fromInteger
  negate = Fixed . negate . fixed

instance Real a => Real (Fixed a) where
  toRational = toRational . fixed

instance Enum a => Enum (Fixed a) where
  toEnum = Fixed . toEnum
  fromEnum = fromEnum . fixed

instance Integral a => Integral (Fixed a) where
  quotRem x y = (\(p,q) -> (Fixed p, Fixed q)) $
                fixed x `quotRem` fixed y
  toInteger = toInteger . fixed
