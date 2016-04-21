{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Protobuf.Wire.Shared (
-- * Message Structure
  FieldNumber(..)
, fieldNumber
, WireType(..)

-- * Integral Types
, Fixed(..)
, Signed(..)

-- * Enumerable Types
, Enumerated(..)

, Packed(..)
, Nested(..)
, UnpackedVec(..)
, PackedVec(..)
, NestedVec(..)
  ) where

import           Control.DeepSeq (NFData)
import           Data.Word (Word8, Word32, Word64)
import           GHC.Exts (IsList(..))
import           GHC.Generics
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

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

-- | 'Fixed' provides a way to encode integers in the fixed-width wire formats.
newtype Fixed a = Fixed { fixed :: a } deriving (Show, Eq, Ord, Generic, NFData)

-- | 'Signed' provides a way to encode integers in the signed wire formats.
newtype Signed a = Signed { signed :: a } deriving (Show, Eq, Ord, Generic, NFData)

-- | 'Enumerated' lifts any type with an 'IsEnum' instance so that it can be encoded
-- with 'HasEncoding'.
newtype Enumerated a = Enumerated { enumerated :: a } deriving (Show, Eq, Ord, Generic, NFData)

-- | 'Packed' provides a way to encode packed lists of basic protobuf types into
-- the wire format.
newtype Packed a = Packed { packed :: a } deriving (Show, Eq, Ord, Generic, NFData)

newtype PackedVec a = PackedVec { packedvec :: V.Vector a }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance IsList (PackedVec a) where
  type Item (PackedVec a) = a
  fromList = PackedVec . V.fromList
  toList = V.toList . packedvec

newtype UnpackedVec a = UnpackedVec {unpackedvec :: V.Vector a }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance IsList (UnpackedVec a) where
  type Item (UnpackedVec a) = a
  fromList = UnpackedVec . V.fromList
  toList = V.toList . unpackedvec

newtype NestedVec a =
  NestedVec { nestedvec :: V.Vector a }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance IsList (NestedVec a) where
  type Item (NestedVec a) = a
  fromList = NestedVec . V.fromList
  toList = V.toList . nestedvec

-- | 'Nested' provides a way to nest protobuf messages within protobuf messages.
newtype Nested a = Nested { nested :: a } deriving (Show, Eq, Ord, Generic, NFData, Monoid)
