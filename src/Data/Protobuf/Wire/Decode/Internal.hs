module Data.Protobuf.Wire.Decode.Internal where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get
import           Data.Word (Word8, Word32, Word64)

base128Varint :: B.ByteString -> Word64
base128Varint bs = reassembled
  where rawBytes = map (fromIntegral . flip clearBit 7) (B.unpack bs)
        shifts = map (flip shiftL . (*7)) [0..]
        shifted = zipWith ($) shifts rawBytes
        reassembled = foldr1 (.|.) shifted

wireType :: Word8 -> WireType
wireType 0 = Varint
wireType 5 = Fixed32
wireType 1 = Fixed64
wireType 2 = LengthDelimited
wireType wt = error $ "Decode got unknown wire type: " ++ (show wt)

fieldHeader :: B.ByteString -> (FieldNumber, WireType)
fieldHeader bs = (decodeFieldNumber word, decodeWireType word)
  where word = base128Varint bs
        decodeFieldNumber = FieldNumber . (flip shiftR 3)
        decodeWireType = wireType . fromIntegral . (.&. 7)
