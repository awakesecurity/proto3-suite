{-# LANGUAGE FlexibleInstances #-}

module Data.Protobuf.Wire.Decode.Parser (
Parser,
-- * General functions
parse,

-- * Combinators
repeatedUnpacked,
repeatedPacked,
parseEmbedded,

-- * Basic types
ProtobufParsable(..),
ProtobufPackable,
Fixed(..),
field,
enumField
) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity(runIdentity)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Protobuf.Wire.Decode.Internal
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get(runGet, getWord32le, getWord64le)
import           Data.Serialize.IEEE754(getFloat32le, getFloat64le)
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8')
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)

type Parser a = ReaderT (M.Map FieldNumber [ParsedField]) (Except String) a


-- | Runs a 'Parser'.
parse :: Parser a -> B.ByteString -> Either String a
parse parser bs = parseTuples bs >>=
                  runIdentity . runExceptT . runReaderT parser

-- |
-- To comply with the protobuf spec, if there are multiple fields with the same
-- field number, this will always return the last one. While this is worst case
-- O(n), in practice the worst case will only happen when a field in the .proto
-- file has been changed from singular to repeated, but the deserializer hasn't
-- been made aware of the change.
parsedField :: FieldNumber -> Parser (Maybe ParsedField)
parsedField fn = do
  currMap <- ask
  let pfs = M.lookup fn currMap
  case pfs of
    Just xs@(_:_) -> return $ Just $ last xs
    _ -> return Nothing

-- |
-- Consumes all fields with the given field number. This is primarily for
-- unpacked repeated fields. This is also useful for parsing
-- embedded messages, where the spec says that if more than one instance of an
-- embedded message for a given field number is present in the outer message,
-- then they must all be merged.
parsedFields :: FieldNumber -> Parser [ParsedField]
parsedFields fn = do
  currMap <- ask
  let pfs = M.lookup fn currMap
  case pfs of
    Just xs -> return xs
    Nothing -> return []

throwWireTypeError :: Show a => String -> a -> Parser b
throwWireTypeError expected wrong =
  throwError $ "Wrong wiretype. Expected " ++ expected ++
               " but got " ++ show wrong

throwCerealError :: String -> String -> Parser b
throwCerealError expected cerealErr =
  throwError $ "Failed to parse contents of " ++ expected ++ " field. "
               ++ "Error from cereal was: " ++ cerealErr

parseVarInt :: Integral a => ParsedField -> Parser a
parseVarInt (VarintField i) = return $ fromIntegral i
parseVarInt wrong = throwWireTypeError "varint" wrong

parsePackedVarInt :: Integral a => ParsedField -> Parser [a]
parsePackedVarInt (LengthDelimitedField bs) =
  case runGet (many getBase128Varint) bs of
    Left e -> throwCerealError "packed varints" e
    Right xs -> return $ map fromIntegral xs
parsePackedVarInt wrong = throwWireTypeError "packed varints" wrong

parsePackedFixed32 :: Integral a => ParsedField -> Parser [a]
parsePackedFixed32 (LengthDelimitedField bs) =
  case runGet (many getWord32le) bs of
    Left e -> throwCerealError "fixed32 packed" e
    Right xs -> return $ map fromIntegral xs
parsePackedFixed32 wrong = throwWireTypeError "fixed32 packed" wrong

parseFixed32 :: Integral a => ParsedField -> Parser a
parseFixed32 (Fixed32Field bs) =
  case runGet getWord32le bs of
    Left e -> throwCerealError "fixed32" e
    Right i -> return $ fromIntegral i
parseFixed32 wrong = throwWireTypeError "fixed32" wrong

parsePackedFixed32Float :: ParsedField -> Parser [Float]
parsePackedFixed32Float (LengthDelimitedField bs) =
  case runGet (many getFloat32le) bs of
    Left e -> throwCerealError "fixed32 packed" e
    Right xs -> return xs
parsePackedFixed32Float wrong = throwWireTypeError "fixed32 packed" wrong

parseFixed32Float :: ParsedField -> Parser Float
parseFixed32Float (Fixed32Field bs) =
  case runGet getFloat32le bs of
    Left e -> throwCerealError "fixed32" e
    Right f -> return f
parseFixed32Float wrong = throwWireTypeError "fixed32" wrong

parsePackedFixed64 :: Integral a => ParsedField -> Parser [a]
parsePackedFixed64 (LengthDelimitedField bs) =
  case runGet (many getWord64le) bs of
    Left e -> throwCerealError "fixed64 packed" e
    Right xs -> return $ map fromIntegral xs
parsePackedFixed64 wrong = throwWireTypeError "fixed 64 packed" wrong

parseFixed64 :: Integral a => ParsedField -> Parser a
parseFixed64 (Fixed64Field bs) =
  case runGet getWord64le bs of
    Left e -> throwCerealError "fixed64" e
    Right i -> return $ fromIntegral i
parseFixed64 wrong = throwWireTypeError "fixed64" wrong

parsePackedFixed64Double :: ParsedField -> Parser [Double]
parsePackedFixed64Double (LengthDelimitedField bs) =
  case runGet (many getFloat64le) bs of
    Left e -> throwCerealError "fixed64 doubles packed" e
    Right xs -> return xs
parsePackedFixed64Double wrong = throwWireTypeError "doubles packed" wrong

parseFixed64Double :: ParsedField -> Parser Double
parseFixed64Double (Fixed64Field bs) =
  case runGet getFloat64le bs of
    Left e -> throwCerealError "fixed64" e
    Right f -> return f
parseFixed64Double wrong = throwWireTypeError "fixed64" wrong

parseText :: ParsedField -> Parser Text
parseText (LengthDelimitedField bs) =
  case decodeUtf8' $ BL.fromStrict bs of
    Left err -> throwError $ "Failed to decode UTF-8: " ++ show err
    Right txt -> return txt
parseText wrong = throwWireTypeError "string" wrong

parseBytes :: ParsedField -> Parser B.ByteString
parseBytes (LengthDelimitedField bs) = return bs
parseBytes wrong = throwWireTypeError "bytes" wrong

-- | Create a parser for embedded fields from a message parser. This can
-- be used to easily create an instance of 'ProtobufParsable' for a user-defined
-- type.
parseEmbedded :: Parser a -> ParsedField -> Parser a
parseEmbedded parser (LengthDelimitedField bs) =
  case parse parser bs of
    Left err -> throwError $ "Failed to parse embedded message: " ++ show err
    Right result -> return result
parseEmbedded _ wrong = throwWireTypeError "embedded" wrong

-- | Specify that one value is expected from this field. Used to ensure that we
-- return the last value with the given field number in the message, in
-- compliance with the protobuf standard.
one :: (ParsedField -> Parser a) -> FieldNumber -> Parser (Maybe a)
one rawParser fn = parsedField fn >>= mapM rawParser

-- | Type class for types that can be parsed from the fields of protobuf
-- messages. This includes singular types like 'Bool' or 'Double', as well as
-- user-defined embedded messages.
class ProtobufParsable a where
  -- | Parse a type from a field. To implement this for embedded messages, see
  -- the helper function 'parseEmbedded'.
  fromField :: ParsedField -> Parser a
  -- | Specifies the default value when a field is missing from the message.
  -- Note: this is used by protobufs to save space on messages by omitting them
  -- if they happen to be set to the default.
  protoDefault :: a

instance ProtobufParsable Bool where
  fromField = fmap toEnum . parseVarInt
  protoDefault = False

instance ProtobufParsable Int32 where
  fromField = parseVarInt
  protoDefault = 0

instance ProtobufParsable Word32 where
  fromField = parseVarInt
  protoDefault = 0

instance ProtobufParsable Int64 where
  fromField = parseVarInt
  protoDefault = 0

instance ProtobufParsable Word64 where
  fromField = parseVarInt
  protoDefault = 0

instance ProtobufParsable (Fixed Word32) where
  fromField = parseFixed32
  protoDefault = 0

instance ProtobufParsable (Fixed Int32) where
  fromField = parseFixed32
  protoDefault = 0

instance ProtobufParsable (Fixed Word64) where
  fromField = parseFixed64
  protoDefault = 0

instance ProtobufParsable (Fixed Int64) where
  fromField = parseFixed64
  protoDefault = 0

instance ProtobufParsable Float where
  fromField = parseFixed32Float
  protoDefault = 0

instance ProtobufParsable Double where
  fromField = parseFixed64Double
  protoDefault = 0

instance ProtobufParsable Text where
  fromField = parseText
  protoDefault = mempty

instance ProtobufParsable B.ByteString where
  fromField = parseBytes
  protoDefault = mempty

field :: ProtobufParsable a => FieldNumber -> Parser a
field = fmap (fromMaybe protoDefault) . one fromField

--TODO: 'enumField' function, or 'instance ProtoEnum a => ProtobufParsable a'?
--      Or something else?

-- | Parses an enumerated field. Because it seems that Google's implementation
-- always serializes the 0th case as a missing field (for compactness), we
-- handle that within this function. Thus, this function returns 'a' instead of
-- 'Maybe a' like the other functions.
enumField :: Enum a => FieldNumber -> Parser a
enumField fn = do
  varint <- one parseVarInt fn
  return $ fromMaybe (toEnum 0) (toEnum <$> varint)

-- | Type class for fields that can be repeated in the more efficient packed
-- format. This is limited to primitive numeric types.
class ProtobufPackable a where
  parsePacked :: ParsedField -> Parser [a]

instance ProtobufPackable Word32 where
  parsePacked = parsePackedVarInt

instance ProtobufPackable Word64 where
  parsePacked = parsePackedVarInt

instance ProtobufPackable Int32 where
  parsePacked = parsePackedVarInt

instance ProtobufPackable Int64 where
  parsePacked = parsePackedVarInt

instance ProtobufPackable (Fixed Word32) where
  parsePacked = parsePackedFixed32

instance ProtobufPackable (Fixed Int32) where
  parsePacked = parsePackedFixed32

instance ProtobufPackable (Fixed Word64) where
  parsePacked = parsePackedFixed64

instance ProtobufPackable (Fixed Int64) where
  parsePacked = parsePackedFixed64

instance ProtobufPackable Float where
  parsePacked = parsePackedFixed32Float

instance ProtobufPackable Double where
  parsePacked = parsePackedFixed64Double

-- | Parses an unpacked repeated field.
repeatedUnpacked :: ProtobufParsable a => FieldNumber -> Parser [a]
repeatedUnpacked fn = parsedFields fn >>= mapM fromField

-- | Parses a packed repeated field. Correctly handles the case where a packed
-- field has been split across multiple key/value pairs in the encoded message.
repeatedPacked :: ProtobufPackable a => FieldNumber -> Parser [a]
repeatedPacked fn = fmap concat $ parsedFields fn >>= mapM parsePacked
