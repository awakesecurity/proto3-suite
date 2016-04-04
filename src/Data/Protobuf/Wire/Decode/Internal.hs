module Data.Protobuf.Wire.Decode.Internal where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get
import           Data.Word (Word8, Word32, Word64)
import           Control.Monad.Loops
import           Control.Applicative

-- | Decode a ByteString containing a base128 varint,
-- assuming that the ByteString contains only a valid varint and
-- nothing else. Warning: partial!
base128Varint :: [Word8] -> Word64
base128Varint [] = error "tried to decode base128Varint of empty string"
base128Varint bs = foldr1 (.|.) shiftedBytes
  where rawBytes = map (fromIntegral . flip clearBit 7) bs
        shifts = map (flip shiftL . (*7)) [0..]
        shiftedBytes = zipWith ($) shifts rawBytes

getBase128Varint :: Get Word64
getBase128Varint = base128Varint <$> untilM peek base128Terminal
  where peek = lookAhead getWord8 :: Get Word8
        base128Terminal = (not . (`testBit` 7)) <$> getWord8

wireType :: Word8 -> WireType
wireType 0 = Varint
wireType 5 = Fixed32
wireType 1 = Fixed64
wireType 2 = LengthDelimited
wireType wt = error $ "Decode got unknown wire type: " ++ (show wt)

fieldHeader :: Word64 -> (FieldNumber, WireType)
fieldHeader word = (decodeFieldNumber word, decodeWireType word)
  where decodeFieldNumber = FieldNumber . (flip shiftR 3)
        decodeWireType = wireType . fromIntegral . (.&. 7)

getFieldHeader :: Get (FieldNumber, WireType)
getFieldHeader = getBase128Varint >>= (return . fieldHeader)

zigZagDecode :: (Num a, Bits a) => a -> a
zigZagDecode i = (shiftR i 1) `xor` (-(i .&. 1))

data ParsedField =
  VarintField Word64
  | Fixed32Field B.ByteString
  | Fixed64Field B.ByteString
  | LengthDelimitedField B.ByteString
  deriving (Show, Eq)

getLengthDelimited :: Get B.ByteString
getLengthDelimited = getBase128Varint >>= (getByteString . fromIntegral)

getParsedField :: WireType -> Get ParsedField
getParsedField Varint = VarintField <$> getBase128Varint
getParsedField Fixed32 = Fixed32Field <$> getByteString 4
getParsedField Fixed64 = Fixed64Field <$> getByteString 8
getParsedField LengthDelimited = LengthDelimitedField <$> getLengthDelimited

getKeyVal :: Get (FieldNumber, ParsedField)
getKeyVal = do (fn, wt) <- getFieldHeader
               parsedField <- getParsedField wt
               return (fn, parsedField)

getTuples :: Get [(FieldNumber, ParsedField)]
getTuples = getListOf getKeyVal
