{-# LANGUAGE BangPatterns #-}

module Data.Protobuf.Wire.Decode.Internal where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (groupBy)
import qualified Data.Map.Strict as M
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get
import           Data.Word (Word8, Word32, Word64)
import           Control.Applicative

-- | Get a base128 varint. Handles delimitation by MSB.
getBase128Varint :: Get Word64
getBase128Varint = loop 0 0 --untilM peek base128Terminal >>= decodeBase128Varint
  where loop !i !w64 = do
          w8 <- getWord8
          if base128Terminal w8
              then return $ combine i w64 w8
              else loop
                   (i + 1)
                   (combine i w64 w8)
        base128Terminal w8 = (not . (`testBit` 7)) $ w8
        combine i w64 w8 = (w64 .|. (fromIntegral (w8 `clearBit` 7)
                           `shiftL`
                           (i * 7)))

-- | Parse a WireType. Call 'fail' if the parsed wire type is unknown.
wireType :: Word8 -> Get WireType
wireType 0 = return Varint
wireType 5 = return Fixed32
wireType 1 = return Fixed64
wireType 2 = return LengthDelimited
wireType wt = fail $ "Decode got unknown wire type: " ++ show wt

getFieldHeader :: Get (FieldNumber, WireType)
getFieldHeader = getBase128Varint >>= fieldHeader
  where fieldHeader word = do
          wt <- wireType $ fromIntegral (word .&. 7)
          return (decodeFieldNumber word, wt)
          where decodeFieldNumber = FieldNumber . (`shiftR` 3)

-- | Decode a zigzag-encoded numeric type.
-- See: http://stackoverflow.com/questions/2210923/zig-zag-decoding
zigZagDecode :: (Num a, Bits a) => a -> a
zigZagDecode i = shiftR i 1 `xor` (-(i .&. 1))

-- | Represents a shallow deserialization of one field in a protobuf message.
-- We don't know what's inside some of these fields until we know what type
-- we're deserializing to, so we leave them as 'ByteString' until a later step
-- in the process.
data ParsedField =
  VarintField Word64
  | Fixed32Field B.ByteString
  | Fixed64Field B.ByteString
  | LengthDelimitedField B.ByteString
  deriving (Show, Eq)

-- | Parse a length-delimited field.
getLengthDelimited :: Get B.ByteString
getLengthDelimited = getBase128Varint >>= (getByteString . fromIntegral)

-- | Parse a field based on its 'WireType'.
getParsedField :: WireType -> Get ParsedField
getParsedField Varint = VarintField <$> getBase128Varint
getParsedField Fixed32 = Fixed32Field <$> getByteString 4
getParsedField Fixed64 = Fixed64Field <$> getByteString 8
getParsedField LengthDelimited = LengthDelimitedField <$> getLengthDelimited

-- | Parse one key/value pair in a protobuf message.
getKeyVal :: Get (FieldNumber, ParsedField)
getKeyVal = do (fn, wt) <- getFieldHeader
               parsedField <- getParsedField wt
               return (fn, parsedField)

-- | Deserializes a protobuf message into a map from field number to all fields
-- labeled with that field number, in their original order. This is necessary
-- because of repeated fields, as well as the protobuf requirement that we honor
-- only the last element with a given field number.
-- This is as much structure as we can recover without knowing the type of the
-- message.
getTuples :: Get (M.Map FieldNumber [ParsedField])
getTuples = do
  keyvals <- many getKeyVal
  let grouped = groupBy ((==) `on` fst) keyvals
  return $ M.fromList $ map (\kvs@(kv:_) -> (fst kv, map snd kvs)) grouped

-- | Turns a raw protobuf message into a map from 'FieldNumber' to a list
-- of all 'ParsedField' values that are labeled with that number.
parseTuples :: B.ByteString -> Either String (M.Map FieldNumber [ParsedField])
parseTuples = runGet getTuples
