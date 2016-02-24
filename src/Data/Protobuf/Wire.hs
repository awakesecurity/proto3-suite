-- | A set of low level functions for writing the protobufs wire format.
--
-- No attempt is made to ensure correctness. Such things are left to
-- higher-level libraries.

module Data.Protobuf.Wire
  (
  -- * Message Structure
    FieldNumber
  , fieldNumber
  -- * Standard Integers
  , int32
  , int64
  -- * Unsigned Integers
  , uint32
  , uint64
  -- * Signed Integers
  , sint32
  , sint64
  -- * Non-varint Numbers
  , fixed32
  , fixed64
  , sfixed32
  , sfixed64
  , float
  , double
  , enum
  -- * Strings
  , string
  , bytes
  -- * Embedded Messages
  , embedded
  ) where

import           Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import           Data.Monoid ((<>))
import           Data.Word (Word8, Word32, Word64)

base128Varint :: Integer -> BB.Builder
base128Varint i
  | i < 128 = BB.word8 (fromIntegral i)
  | otherwise = BB.word8 (0x80 .|. (fromIntegral i .&. 0x7f)) <> base128Varint (i `shiftR` 7)

newtype FieldNumber = FieldNumber { getFieldNumber :: Integer } deriving (Show, Eq, Ord)

fieldNumber :: Integer -> FieldNumber
fieldNumber = FieldNumber

data WireType
  = Varint
  | Fixed32
  | Fixed64
  | LengthDelimited
  | StartGroup
  | EndGroup
  deriving (Show, Eq, Ord)

wireType :: WireType -> Word8
wireType Varint           = 0
wireType Fixed32          = 5
wireType Fixed64          = 1
wireType LengthDelimited  = 2
wireType StartGroup       = 3
wireType EndGroup         = 4

fieldHeader :: FieldNumber -> WireType -> BB.Builder
fieldHeader num wt = base128Varint ((getFieldNumber num `shiftL` 3) .|. fromIntegral (wireType wt))

int32 :: FieldNumber -> Int32 -> BB.Builder
int32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

int64 :: FieldNumber -> Int64 -> BB.Builder
int64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

uint32 :: FieldNumber -> Word32 -> BB.Builder
uint32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

uint64 :: FieldNumber -> Word64 -> BB.Builder
uint64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

sint32 :: FieldNumber -> Int32 -> BB.Builder
sint32 num i = int32 num ((i `shiftL` 1) `xor` (i `shiftR` 31))

sint64 :: FieldNumber -> Int64 -> BB.Builder
sint64 num i = int64 num ((i `shiftL` 1) `xor` (i `shiftR` 63))

fixed32 :: FieldNumber -> Word32 -> BB.Builder
fixed32 num i = fieldHeader num Fixed32 <> BB.word32LE i

fixed64 :: FieldNumber -> Word64 -> BB.Builder
fixed64 num i = fieldHeader num Fixed64 <> BB.word64LE i

sfixed32 :: FieldNumber -> Int32 -> BB.Builder
sfixed32 num i = fieldHeader num Fixed32 <> BB.int32LE i

sfixed64 :: FieldNumber -> Int64 -> BB.Builder
sfixed64 num i = fieldHeader num Fixed64 <> BB.int64LE i

float :: FieldNumber -> Float -> BB.Builder
float num f = fieldHeader num Fixed32 <> BB.floatLE f

double :: FieldNumber -> Double -> BB.Builder
double num d = fieldHeader num Fixed64 <> BB.doubleLE d

enum :: (Enum e) => FieldNumber -> e -> BB.Builder
enum num e = fieldHeader num Varint <> base128Varint (fromIntegral (fromEnum e))

string :: FieldNumber -> String -> BB.Builder
string num = embedded num . BB.stringUtf8

bytes :: FieldNumber -> B.ByteString -> BB.Builder
bytes num = embedded num . BB.byteString

embedded :: FieldNumber -> BB.Builder -> BB.Builder
embedded num bb = fieldHeader num LengthDelimited <> base128Varint (fromIntegral len) <> bb where
  len = BL.length (BB.toLazyByteString bb)
