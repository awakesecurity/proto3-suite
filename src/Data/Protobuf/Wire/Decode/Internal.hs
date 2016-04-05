module Data.Protobuf.Wire.Decode.Internal where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get
import           Data.Word (Word8, Word32, Word64)
import           Control.Monad.Loops
import           Control.Applicative

-- | Decode a ByteString containing a base128 varint,
-- assuming that the input list contains only a valid varint and
-- nothing else.
decodeBase128Varint :: [Word8] -> Get Word64
decodeBase128Varint [] = fail "tried to decode base128Varint of empty string"
decodeBase128Varint bs = return $ foldr1 (.|.) shiftedBytes
  where rawBytes = map (fromIntegral . flip clearBit 7) bs
        shifts = map (flip shiftL . (*7)) [0..]
        shiftedBytes = zipWith ($) shifts rawBytes

-- | Get a base128 varint. Handles delimitation by MSB.
getBase128Varint :: Get Word64
getBase128Varint = untilM peek base128Terminal >>= decodeBase128Varint
  where peek = lookAhead getWord8 :: Get Word8
        base128Terminal = (not . (`testBit` 7)) <$> getWord8

wireType :: Word8 -> Get WireType
wireType 0 = return Varint
wireType 5 = return Fixed32
wireType 1 = return Fixed64
wireType 2 = return LengthDelimited
wireType wt = fail $ "Decode got unknown wire type: " ++ show wt

fieldHeader :: Word64 -> Get (FieldNumber, WireType)
fieldHeader word = do
  wt <- wireType $ fromIntegral (word .&. 7)
  return (decodeFieldNumber word, wt)
  where decodeFieldNumber = FieldNumber . (`shiftR` 3)

getFieldHeader :: Get (FieldNumber, WireType)
getFieldHeader = getBase128Varint >>= fieldHeader

zigZagDecode :: (Num a, Bits a) => a -> a
zigZagDecode i = shiftR i 1 `xor` (-(i .&. 1))

-- | Represents a shallow deserialization of a field. We don't know what's
-- inside some of these fields until we know what type we're deserialzing to,
-- so we leave them as 'ByteString' until a later step in the process.
data ParsedField =
  VarintField Word64
  | Fixed32Field B.ByteString
  | Fixed64Field B.ByteString
  | LengthDelimitedField B.ByteString
  deriving (Show, Eq)

getLengthDelimited :: Get B.ByteString
getLengthDelimited = getBase128Varint >>= (getByteString . fromIntegral)

-- | Parse a field based on its 'WireType'.
getParsedField :: WireType -> Get ParsedField
getParsedField Varint = VarintField <$> getBase128Varint
getParsedField Fixed32 = Fixed32Field <$> getByteString 4
getParsedField Fixed64 = Fixed64Field <$> getByteString 8
getParsedField LengthDelimited = LengthDelimitedField <$> getLengthDelimited

getKeyVal :: Get (FieldNumber, ParsedField)
getKeyVal = do (fn, wt) <- getFieldHeader
               parsedField <- getParsedField wt
               return (fn, parsedField)

-- | Deserializes a protobuf message into a list of field number, field tuples.
-- This is as much structure as we can recover without knowing the type of the
-- message.
getTuples :: Get [(FieldNumber, ParsedField)]
getTuples = many getKeyVal
