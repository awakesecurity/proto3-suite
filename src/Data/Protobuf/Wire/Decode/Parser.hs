-- | Module containing the means to build parsers that decode protobuf messages.
-- Usually, one should avoid writing these parsers by hand. Instead, use the
-- generic interface in "Data.Protobuf.Wire.Generic".

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Protobuf.Wire.Decode.Parser
  (
  -- * The Parser type
    Parser
  , ParsedField()
  , ParsedFields
  , parse
  , ParseError(..)
  -- * Primitives
  , parseBool
  , parseInt32
  , parseWord32
  , parseInt64
  , parseWord64
  , parseSignedInt32
  , parseSignedInt64
  , parseFixedWord32
  , parseFixedInt32
  , parseFixedWord64
  , parseFixedInt64
  , parseByteString
  , parseLazyByteString
  , parseEnum
  , parsePackedVarInt
  , parsePackedFixed32
  , parsePackedFixed32Float
  , parsePackedFixed64
  , parsePackedFixed64Double
  , parseFixed32
  , parseFixed32Float
  , parseFixed64
  , parseFixed64Double
  , parseText
  -- * Decoding fields
  , at
  , one
  , disembed
  , atomicEmbedded
  , repeatedUnpackedList
  -- , repeatedPackedList
  , parseEmbeddedList
  ) where

import           Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (toList, foldl')
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Protobuf.Wire.Decode.Internal
import           Data.Protobuf.Wire.Shared
import           Data.Sequence (Seq, ViewR(..), viewr, fromList)
import           Data.Serialize.Get (Get, runGet, getWord32le, getWord64le)
import           Data.Serialize.IEEE754(getFloat32le, getFloat64le)
import           Data.Text.Lazy (Text, pack)
import           Data.Text.Lazy.Encoding (decodeUtf8')
import qualified Data.Traversable as T
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)
import           Pipes
import qualified Pipes.Prelude as P

-- | Type describing possible errors that can be encountered while parsing.
data ParseError
  -- | A 'WireTypeError' occurs when the type of the data in the protobuf
  -- binary format does not match the type encountered by the parser. This can
  -- indicate that the type of a field has changed or is incorrect.
  = WireTypeError Text
  -- | A 'BinaryError' occurs when we can't successfully parse the contents of
  -- the field.
  | BinaryError Text
  -- | An 'EmbeddedError' occurs when we encounter an error while parsing an
  -- | embedded message.
  | EmbeddedError Text [ParseError]
  deriving (Show, Eq, Ord)

-- | A parsing function type synonym, to tidy up type signatures.
type Parser input a = input -> Either [ParseError] a

-- | A 'Map' from field numbers to the fields whose headers were associated with
-- that field number. Typically used as the first argument to the 'Parser' type
-- synonym.
type ParsedFields = M.Map FieldNumber (Seq ParsedField)

-- | Runs a 'Parser'
parse
  :: Parser ParsedFields a
  -> B.ByteString
  -> Either [ParseError] a
parse parser bs =
  case parseTuples bs of
    Left err -> Left [ BinaryError (pack err) ]
    Right res -> parser res

-- | To comply with the protobuf spec, if there are multiple fields with the same
-- field number, this will always return the last one. While this is worst case
-- O(n), in practice the worst case will only happen when a field in the .proto
-- file has been changed from singular to repeated, but the deserializer hasn't
-- been made aware of the change.
parsedField :: (Seq ParsedField) -> Maybe ParsedField
parsedField xs = case viewr xs of
                   EmptyR -> Nothing
                   _ :> x -> Just x

throwWireTypeError :: Show input => String -> Parser input expected
throwWireTypeError expected wrong =
    Left [ WireTypeError (pack msg) ]
  where
    msg = "Wrong wiretype. Expected " ++ expected ++ " but got " ++ show wrong

throwCerealError :: String -> String -> Either [ParseError] a
throwCerealError expected cerealErr =
    Left [ BinaryError (pack msg) ]
  where
    msg = "Failed to parse contents of " ++ expected ++ " field. " ++ "Error from cereal was: " ++ cerealErr

parseVarInt :: Integral a => Parser ParsedField a
parseVarInt (VarintField i) = Right (fromIntegral i)
parseVarInt wrong = throwWireTypeError "varint" wrong

runGetPacked :: Get [a] -> Parser ParsedField [a]
runGetPacked g (LengthDelimitedField bs) =
  case runGet g bs of
    Left e -> throwCerealError "packed repeated field" e
    Right xs -> return xs
runGetPacked _ wrong =
  throwWireTypeError "packed repeated field" wrong

runGetFixed32 :: Get a -> Parser ParsedField a
runGetFixed32 g (Fixed32Field bs) =
  case runGet g bs of
    Left e -> throwCerealError "fixed32 field" e
    Right x -> return x
runGetFixed32 _ wrong =
  throwWireTypeError "fixed 32 field" wrong

runGetFixed64 :: Get a -> Parser ParsedField a
runGetFixed64 g (Fixed64Field bs) =
  case runGet g bs of
    Left e -> throwCerealError "fixed 64 field" e
    Right x -> return x
runGetFixed64 _ wrong =
  throwWireTypeError "fixed 64 field" wrong

parsePackedVarInt :: Integral a => Parser ParsedField [a]
parsePackedVarInt = fmap (fmap fromIntegral)
                    . runGetPacked (many getBase128Varint)

parsePackedFixed32 :: Integral a => Parser ParsedField [a]
parsePackedFixed32 = fmap (fmap fromIntegral) . runGetPacked (many getWord32le)

parsePackedFixed32Float :: Parser ParsedField [Float]
parsePackedFixed32Float = runGetPacked (many getFloat32le)

parsePackedFixed64 :: Integral a => Parser ParsedField [a]
parsePackedFixed64 = fmap (fmap fromIntegral) . runGetPacked (many getWord64le)

parsePackedFixed64Double :: Parser ParsedField [Double]
parsePackedFixed64Double = runGetPacked (many getFloat64le)

parseFixed32 :: Integral a => Parser ParsedField a
parseFixed32 = fmap fromIntegral . runGetFixed32 getWord32le

parseFixed32Float :: Parser ParsedField Float
parseFixed32Float = runGetFixed32 getFloat32le

parseFixed64 :: Integral a => Parser ParsedField a
parseFixed64 = fmap fromIntegral . runGetFixed64 getWord64le

parseFixed64Double :: Parser ParsedField Double
parseFixed64Double = runGetFixed64 getFloat64le

parseText :: Parser ParsedField Text
parseText (LengthDelimitedField bs) =
  case decodeUtf8' $ BL.fromStrict bs of
    Left err -> Left [ BinaryError (pack ("Failed to decode UTF-8: " ++ show err)) ]
    Right txt -> return txt
parseText wrong = throwWireTypeError "string" wrong

parseBytes :: Parser ParsedField B.ByteString
parseBytes (LengthDelimitedField bs) = return bs
parseBytes wrong = throwWireTypeError "bytes" wrong

-- | Create a parser for embedded fields from a message parser. This can
-- be used to easily create an instance of 'AtomicParsable' for a user-defined
-- type. For embedded messages, this does not handle combining messages in the
-- way that 'disembed' does. This function is simply a helper function for
-- parsing lists of embedded messages.
atomicEmbedded :: Parser ParsedFields a -> Parser ParsedField a
atomicEmbedded parser (LengthDelimitedField bs) =
  case parse parser bs of
    Left err -> Left [ EmbeddedError "Failed to parse embedded message." err ]
    Right result -> return result
atomicEmbedded _ wrong = throwWireTypeError "embedded" wrong

-- | For a field containing an embedded message, parse as far as getting the
-- wire-level fields out of the message. This is a helper function for
-- 'disembed'.
embeddedToParsedFields :: Parser ParsedField ParsedFields
embeddedToParsedFields (LengthDelimitedField bs) =
  case parseTuples bs of
    Left err -> Left [EmbeddedError ("Failed to parse embedded message: "
                                     <> (pack err))
                                    []]
    Right result -> return result
embeddedToParsedFields wrong = throwWireTypeError "embedded" wrong

parseEmbeddedList
  :: Parser ParsedField a
  -> Parser (Seq ParsedField) (Seq a)
parseEmbeddedList parser fields = fmap fromList $ listify $ do
  x <- delistify (toList fields)
  y <- lift (parser x)
  return y

-- | Helper function for making user-defined types instances of 'HasDecoding'.
--
-- The protobuf spec requires that embedded messages be mergeable, so that
-- protobuf encoding has the flexibility to transmit embedded messages in
-- pieces. This function reassembles the pieces, and must be used to parse all
-- embedded non-repeated messages.
--
-- If the embedded message is not found in the outer message, this function
-- returns 'Nothing'.
disembed :: Parser ParsedFields a -> Parser (Seq ParsedField) (Maybe a)
disembed p xs =
  if xs == empty
     then return Nothing
     else do innerMaps <- T.mapM embeddedToParsedFields xs
             let combinedMap = foldl' (M.unionWith (<>)) M.empty innerMaps
             parsed <- p combinedMap
             return $ Just parsed

at :: Parser (Seq ParsedField) a -> FieldNumber -> a -> Parser ParsedFields a
at parser fn dflt m =
  case M.lookup fn m of
    Just fields -> parser fields
    Nothing -> Right dflt

-- | Specify that one value is expected from this field. Used to ensure that we
-- return the last value with the given field number in the message, in
-- compliance with the protobuf standard.
one :: Parser ParsedField a -> Parser (Seq ParsedField) (Maybe a)
one parser = traverse parser . parsedField

parseBool :: Parser ParsedField Bool
parseBool = fmap toEnum . parseVarInt

parseInt32 :: Parser ParsedField Int32
parseInt32 = parseVarInt

parseWord32 :: Parser ParsedField Word32
parseWord32 = parseVarInt

parseInt64 :: Parser ParsedField Int64
parseInt64 = parseVarInt

parseWord64 :: Parser ParsedField Word64
parseWord64 = parseVarInt

parseSignedInt32 :: Parser ParsedField (Signed Int32)
parseSignedInt32 = fmap (Signed . fromIntegral
                        . (zigZagDecode :: Word32 -> Word32))
                   . parseVarInt

parseSignedInt64 :: Parser ParsedField (Signed Int64)
parseSignedInt64 = fmap (Signed . fromIntegral
                        . (zigZagDecode :: Word64 -> Word64))
                   . parseVarInt

parseFixedWord32 :: Parser ParsedField (Fixed Word32)
parseFixedWord32 = fmap Fixed . parseFixed32

parseFixedInt32 :: Parser ParsedField (Signed (Fixed Int32))
parseFixedInt32 = fmap (Signed . Fixed) . parseFixed32

parseFixedWord64 :: Parser ParsedField (Fixed Word64)
parseFixedWord64 = fmap Fixed . parseFixed64

parseFixedInt64 :: Parser ParsedField (Signed (Fixed Int64))
parseFixedInt64 = fmap (Signed . Fixed) . parseFixed64

parseByteString :: Parser ParsedField B.ByteString
parseByteString = parseBytes

parseLazyByteString :: Parser ParsedField BL.ByteString
parseLazyByteString = fmap BL.fromStrict . parseBytes

parseEnum :: Enum e => Parser ParsedField (Enumerated e)
parseEnum = fmap (Enumerated . toEnum) . parseVarInt

-- | Parses an unpacked repeated field.
repeatedUnpackedList
  :: Parser ParsedField a
  -> Parser (Seq ParsedField) (Seq a)
repeatedUnpackedList = parseEmbeddedList

listify :: Monad m => ListT m a -> m [a]
listify = P.toListM . enumerate

delistify :: Monad m => [a] -> ListT m a
delistify = Select . each
