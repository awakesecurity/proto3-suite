-- | A set of low level functions for writing the protobufs wire format.
--
-- No attempt is made to ensure correctness. Such things are left to
-- higher-level libraries.
--
-- Because protobuf messages are encoded as a collection of fields, one
-- can use the 'Monoid' instance for 'BB.Builder' to encode multiple
-- fields.
--
-- One should be careful to make sure that 'FieldNumber's appear in increasing
-- order.
--
-- In protocol buffers version 3, all fields are optional. To omit a value
-- for a field, simply do not append it to the 'BB.Builder'. One can
-- create functions for wrapping optional fields with a 'Maybe' type.
--
-- Similarly, repeated fields can be encoded by concatenating several values
-- with the same 'FieldNumber'.
--
-- For example:
--
-- > strings :: Foldable f => FieldNumber -> f String -> BB.Builder
-- > strings = foldMap . string
-- >
-- > fieldNumber 1 `strings` Just "some string" <>
-- > fieldNumber 2 `strings` [ "foo", "bar", "baz" ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Protobuf.Wire.Encode.Internal where

import           Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32, Int64)
import           Data.Monoid ((<>))
import           Data.Protobuf.Wire.Shared
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import           Data.Word (Word8, Word32, Word64)

base128Varint :: Word64 -> BB.Builder
base128Varint i
  | i .&. 0x7f == i = BB.word8 (fromIntegral i)
  | otherwise = BB.word8 (0x80 .|. (fromIntegral i .&. 0x7f)) <> base128Varint (i `shiftR` 7)

wireType :: WireType -> Word8
wireType Varint           = 0
wireType Fixed32          = 5
wireType Fixed64          = 1
wireType LengthDelimited  = 2

fieldHeader :: FieldNumber -> WireType -> BB.Builder
fieldHeader num wt = base128Varint ((getFieldNumber num `shiftL` 3) .|. fromIntegral (wireType wt))

-- | Encode a 32-bit "standard" integer
--
-- For example:
--
-- > fieldNumber 1 `int32` 42
int32 :: FieldNumber -> Int32 -> BB.Builder
int32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 64-bit "standard" integer
--
-- For example:
--
-- > fieldNumber 1 `int64` negate 42
int64 :: FieldNumber -> Int64 -> BB.Builder
int64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 32-bit unsigned integer
--
-- For example:
--
-- > fieldNumber 1 `uint32` 42
uint32 :: FieldNumber -> Word32 -> BB.Builder
uint32 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 64-bit unsigned integer
--
-- For example:
--
-- > fieldNumber 1 `uint64` 42
uint64 :: FieldNumber -> Word64 -> BB.Builder
uint64 num i = fieldHeader num Varint <> base128Varint (fromIntegral i)

-- | Encode a 32-bit signed integer
--
-- For example:
--
-- > fieldNumber 1 `sint32` negate 42
sint32 :: FieldNumber -> Int32 -> BB.Builder
sint32 num i = int32 num ((i `shiftL` 1) `xor` (i `shiftR` 31))

-- | Encode a 64-bit signed integer
--
-- For example:
--
-- > fieldNumber 1 `sint64` negate 42
sint64 :: FieldNumber -> Int64 -> BB.Builder
sint64 num i = int64 num ((i `shiftL` 1) `xor` (i `shiftR` 63))

-- | Encode a fixed-width 32-bit integer
--
-- For example:
--
-- > fieldNumber 1 `fixed32` 42
fixed32 :: FieldNumber -> Word32 -> BB.Builder
fixed32 num i = fieldHeader num Fixed32 <> BB.word32LE i

-- | Encode a fixed-width 64-bit integer
--
-- For example:
--
-- > fieldNumber 1 `fixed64` 42
fixed64 :: FieldNumber -> Word64 -> BB.Builder
fixed64 num i = fieldHeader num Fixed64 <> BB.word64LE i

-- | Encode a fixed-width signed 32-bit integer
--
-- For example:
--
-- > fieldNumber 1 `sfixed32` negate 42
sfixed32 :: FieldNumber -> Int32 -> BB.Builder
sfixed32 num i = fieldHeader num Fixed32 <> BB.int32LE i

-- | Encode a fixed-width signed 64-bit integer
--
-- For example:
--
-- > fieldNumber 1 `sfixed64` negate 42
sfixed64 :: FieldNumber -> Int64 -> BB.Builder
sfixed64 num i = fieldHeader num Fixed64 <> BB.int64LE i

-- | Encode a floating point number
--
-- For example:
--
-- > fieldNumber 1 `float` 3.14
float :: FieldNumber -> Float -> BB.Builder
float num f = fieldHeader num Fixed32 <> BB.floatLE f

-- | Encode a double-precision number
--
-- For example:
--
-- > fieldNumber 1 `double` 3.14
double :: FieldNumber -> Double -> BB.Builder
double num d = fieldHeader num Fixed64 <> BB.doubleLE d

-- | Encode a value with an enumerable type.
--
-- It can be useful to derive an 'Enum' instance for a type in order to
-- emulate enums appearing in .proto files.
--
-- For example:
--
-- > data Shape = Circle | Square | Triangle
-- >   deriving (Show, Eq, Ord, Enum)
-- >
-- > fieldNumber 1 `enum` True <>
-- > fieldNumber 2 `enum` Circle
enum :: (Enum e) => FieldNumber -> e -> BB.Builder
enum num e = fieldHeader num Varint <> base128Varint (fromIntegral (fromEnum e))

-- | Encode a UTF-8 string.
--
-- For example:
--
-- > fieldNumber 1 `string` "testing"
string :: FieldNumber -> String -> BB.Builder
string num = embedded num . BB.stringUtf8

-- | Encode lazy `Text` as UTF-8
--
-- For example:
--
-- > fieldNumber 1 `text` "testing"
text :: FieldNumber -> Text.Lazy.Text -> BB.Builder
text num txt =
    embedded num (BB.lazyByteString (Text.Lazy.Encoding.encodeUtf8 txt))

-- | Encode a collection of bytes in the form of a strict 'B.ByteString'.
--
-- For example:
--
-- > fieldNumber 1 `bytes` fromString "some bytes"
bytes :: FieldNumber -> B.ByteString -> BB.Builder
bytes num = embedded num . BB.byteString

-- | Encode a lazy bytestring.
--
-- For example:
--
-- > fieldNumber 1 `bytes'` fromString "some bytes"
bytes' :: FieldNumber -> BL.ByteString -> BB.Builder
bytes' num = embedded num . BB.lazyByteString

-- | Encode varints in the space-efficient packed format.
packedVarints :: Traversable t => FieldNumber -> t Word64 -> BB.Builder
packedVarints num = embedded num . foldMap base128Varint

-- | Encode fixed-width Word32s in the space-efficient packed format.
packedFixed32s :: Traversable t => FieldNumber -> t Word32 -> BB.Builder
packedFixed32s num = embedded num . foldMap BB.word32LE

-- | Encode fixed-width Word64s in the space-efficient packed format.
packedFixed64s :: Traversable t => FieldNumber -> t Word64 -> BB.Builder
packedFixed64s num = embedded num . foldMap BB.word64LE

-- | Encode floats in the space-efficient packed format.
packedFloats :: Traversable t => FieldNumber -> t Float -> BB.Builder
packedFloats num = embedded num . foldMap BB.floatLE

-- | Encode doubles in the space-efficient packed format.
packedDoubles :: Traversable t => FieldNumber -> t Double -> BB.Builder
packedDoubles num = embedded num . foldMap BB.doubleLE

-- | Encode an embedded message.
--
-- The message is represented as a 'BB.Builder', so it is possible to chain
-- encoding functions.
--
-- For example:
--
-- > embedded (fieldNumber 1) $
-- >   fieldNumber (fieldNumber 1) `string` "this message" <>
-- >   fieldNumber (fieldNumber 2) `string` " is embedded"
embedded :: FieldNumber -> BB.Builder -> BB.Builder
embedded num bb = fieldHeader num LengthDelimited <> base128Varint (fromIntegral len) <> bb where
  len = BL.length (BB.toLazyByteString bb)
