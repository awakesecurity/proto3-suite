-- | Module containing the means to build parsers that decode protobuf messages.
-- Usually, one should avoid writing these parsers by hand. Instead, use the
-- generic interface in "Data.Protobuf.Wire.Generic".

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Protobuf.Wire.Decode.Parser (

  -- * The Parser type
  Parser,
  parse,
  ParseError(..),

  -- * Decoding fields
  disembed,
  atomicEmbedded,
  field,
  repeatedUnpackedList,
  repeatedPackedList,
  parseEmbeddedList,

  -- * Helper types
  AtomicParsable(..),
  Packable(..)
) where

import           Control.Applicative
import           Control.Monad (join)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity(runIdentity)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Protobuf.Wire.Decode.Internal
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get(Get, runGet, getWord32le, getWord64le)
import           Data.Serialize.IEEE754(getFloat32le, getFloat64le)
import           Data.Text.Lazy (Text, pack)
import           Data.Text.Lazy.Encoding (decodeUtf8')
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)
import           Pipes
import qualified Pipes.Prelude as P
import           Safe

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

-- | Parser of protobuf messages and their fields. Intended for use primarily
-- in an Applicative style. For example:
--
--
-- > data MultipleFields =
-- >   MultipleFields {multiFieldDouble :: Double,
-- >                   multiFieldFloat :: Float,
-- >                   multiFieldInt32 :: Int32,
-- >                   multiFieldInt64 :: Int64,
-- >                   multiFieldString :: Text,
-- >                   multiFieldBool :: Bool}
-- >                   deriving (Show, Generic, Eq)
-- >
-- > multipleFieldsParser :: Parser MultipleFields
-- > multipleFieldsParser = MultipleFields
-- >                        <$> field (FieldNumber 1)
-- >                        <*> field (FieldNumber 2)
-- >                        <*> field (FieldNumber 3)
-- >                        <*> field (FieldNumber 4)
-- >                        <*> field (FieldNumber 5)
-- >                        <*> field (FieldNumber 6)
--
newtype Parser a = Parser
  { unParser :: ReaderT
                  (M.Map FieldNumber [ParsedField]) (Except [ParseError]) a}
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus,
            MonadReader (M.Map FieldNumber [ParsedField]),
            MonadError [ParseError])

-- | Runs a 'Parser'.
parse :: Parser a -> B.ByteString -> Either [ParseError] a
parse parser bs = case parseTuples bs of
                  Left err -> throwError [BinaryError $ pack err]
                  Right res -> runIdentity $ runExceptT $
                                runReaderT (unParser parser) res

-- |
-- To comply with the protobuf spec, if there are multiple fields with the same
-- field number, this will always return the last one. While this is worst case
-- O(n), in practice the worst case will only happen when a field in the .proto
-- file has been changed from singular to repeated, but the deserializer hasn't
-- been made aware of the change.
parsedField :: FieldNumber -> Parser (Maybe ParsedField)
parsedField fn = do
  currMap <- ask
  return $ M.lookup fn currMap >>= lastMay

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
  return $ concat pfs

throwWireTypeError :: Show a => String -> a -> Parser b
throwWireTypeError expected wrong =
  throwError [WireTypeError $ pack $
              "Wrong wiretype. Expected "
              ++ expected ++ " but got " ++ show wrong]

throwCerealError :: String -> String -> Parser b
throwCerealError expected cerealErr =
  throwError [BinaryError $ pack $
              "Failed to parse contents of " ++ expected ++ " field. "
              ++ "Error from cereal was: " ++ cerealErr]

parseVarInt :: Integral a => ParsedField -> Parser a
parseVarInt (VarintField i) = return $ fromIntegral i
parseVarInt wrong = throwWireTypeError "varint" wrong

runGetPacked :: Get [a] -> ParsedField -> Parser [a]
runGetPacked g (LengthDelimitedField bs) =
  case runGet g bs of
    Left e -> throwCerealError "packed repeated field" e
    Right xs -> return xs
runGetPacked g wrong =
  throwWireTypeError "packed repeated field" wrong

runGetFixed32 :: Get a -> ParsedField -> Parser a
runGetFixed32 g (Fixed32Field bs) =
  case runGet g bs of
    Left e -> throwCerealError "fixed32 field" e
    Right x -> return x
runGetFixed32 g wrong =
  throwWireTypeError "fixed 32 field" wrong

runGetFixed64 :: Get a -> ParsedField -> Parser a
runGetFixed64 g (Fixed64Field bs) =
  case runGet g bs of
    Left e -> throwCerealError "fixed 64 field" e
    Right x -> return x
runGetFixed64 g wrong =
  throwWireTypeError "fixed 64 field" wrong

parsePackedVarInt :: Integral a => ParsedField -> Parser [a]
parsePackedVarInt = fmap (fmap fromIntegral)
                    . runGetPacked (many getBase128Varint)

parsePackedFixed32 :: Integral a => ParsedField -> Parser [a]
parsePackedFixed32 = fmap (fmap fromIntegral) . runGetPacked (many getWord32le)

parsePackedFixed32Float :: ParsedField -> Parser [Float]
parsePackedFixed32Float = runGetPacked (many getFloat32le)

parsePackedFixed64 :: Integral a => ParsedField -> Parser [a]
parsePackedFixed64 = fmap (fmap fromIntegral) . runGetPacked (many getWord64le)

parsePackedFixed64Double :: ParsedField -> Parser [Double]
parsePackedFixed64Double = runGetPacked (many getFloat64le)

parseFixed32 :: Integral a => ParsedField -> Parser a
parseFixed32 = fmap fromIntegral . runGetFixed32 getWord32le

parseFixed32Float :: ParsedField -> Parser Float
parseFixed32Float = runGetFixed32 getFloat32le

parseFixed64 :: Integral a => ParsedField -> Parser a
parseFixed64 = fmap fromIntegral . runGetFixed64 getWord64le

parseFixed64Double :: ParsedField -> Parser Double
parseFixed64Double = runGetFixed64 getFloat64le

parseText :: ParsedField -> Parser Text
parseText (LengthDelimitedField bs) =
  case decodeUtf8' $ BL.fromStrict bs of
    Left err -> throwError [BinaryError $ pack $
                            "Failed to decode UTF-8: " ++ show err]
    Right txt -> return txt
parseText wrong = throwWireTypeError "string" wrong

parseBytes :: ParsedField -> Parser B.ByteString
parseBytes (LengthDelimitedField bs) = return bs
parseBytes wrong = throwWireTypeError "bytes" wrong

-- | Create a parser for embedded fields from a message parser. This can
-- be used to easily create an instance of 'AtomicParsable' for a user-defined
-- type. For embedded messages, this does not handle combining messages in the
-- way that 'disembed' does. This function is simply a helper function for
-- parsing lists of embedded messages.
atomicEmbedded :: Parser a -> ParsedField -> Parser a
atomicEmbedded parser (LengthDelimitedField bs) =
  case parse parser bs of
    Left err -> throwError
                  [EmbeddedError "Failed to parse embedded message." err]
    Right result -> return result
atomicEmbedded _ wrong = throwWireTypeError "embedded" wrong

parseEmbeddedListT :: Monoid a => (ParsedField -> Parser a)
                                 -> FieldNumber -> ListT Parser a
parseEmbeddedListT p fn = do x <- delistify $ parsedFields fn
                             y <- lift (p x)
                             return y

parseEmbeddedList :: Monoid a => (ParsedField -> Parser a)
                                 -> FieldNumber -> Parser [a]
parseEmbeddedList p fn = listify $ parseEmbeddedListT p fn

-- | Helper function for making user-defined types instances of 'HasDecoding'.
--
-- The protobuf spec requires that embedded messages be mergeable, so that
-- protobuf encoding has the flexibility to transmit embedded messages in
-- pieces. This function reassembles the pieces, and must be used to parse all
-- embedded non-repeated messages. The rules for the Monoid instance (as
-- stated in the protobuf spec) are:
--
-- * @x <> y@ overwrites the singular fields of @x@ with those of @y@.
-- * @x <> y@ recurses on the embedded messages in @x@ and @y@.
-- * @x <> y@ concatenates all list fields in @x@ and @y@.
--
-- If the embedded message is not found in the outer message, this function
-- returns 'mempty'.
disembed :: Monoid a => Parser a -> FieldNumber -> Parser a
disembed parser =
  P.foldM f (return mempty) return . enumerate . delistify . parsedFields
  where f x pf = do
          y <- atomicEmbedded parser pf
          return $ x <> y

-- | Specify that one value is expected from this field. Used to ensure that we
-- return the last value with the given field number in the message, in
-- compliance with the protobuf standard.
one :: (ParsedField -> Parser a) -> FieldNumber -> Parser (Maybe a)
one rawParser fn = parsedField fn >>= mapM rawParser

-- | Type class for types that can be parsed from a single field from a single
-- key/value pair in a wire-encoded protobuf
-- message. This includes singular types like 'Bool' or 'Double', as well as
-- user-defined embedded messages.
class AtomicParsable a where
  -- | Parse a type from one field. To implement this for embedded messages, see
  -- the helper function 'atomicEmbedded'.
  fromField :: ParsedField -> Parser a
  -- | Specifies the default value when a field is missing from the message.
  -- Note: this is used by protobufs to save space on messages by omitting them
  -- if they happen to be set to the default. For more information on default
  -- values in protobufs, see the
  -- <https://developers.google.com/protocol-buffers/docs/proto3#default official docs>.
  protoDefault :: a

instance AtomicParsable Bool where
  fromField = fmap toEnum . parseVarInt
  protoDefault = False

instance AtomicParsable Int32 where
  fromField = parseVarInt
  protoDefault = 0

instance AtomicParsable Word32 where
  fromField = parseVarInt
  protoDefault = 0

instance AtomicParsable Int64 where
  fromField = parseVarInt
  protoDefault = 0

instance AtomicParsable Word64 where
  fromField = parseVarInt
  protoDefault = 0

instance AtomicParsable (Signed Int32) where
  fromField = fmap (Signed . fromIntegral
                    . (zigZagDecode :: Word32 -> Word32) . fromIntegral)
              . parseVarInt
  protoDefault = Signed 0

instance AtomicParsable (Signed Int64) where
  fromField = fmap (Signed . fromIntegral
                    . (zigZagDecode :: Word64 -> Word64) . fromIntegral)
              . parseVarInt
  protoDefault = Signed 0

instance AtomicParsable (Fixed Word32) where
  fromField = fmap (Fixed . fromIntegral) . parseFixed32
  protoDefault = Fixed 0

instance AtomicParsable (Signed (Fixed Int32)) where
  fromField = fmap (Signed . Fixed . fromIntegral) . parseFixed32
  protoDefault = Signed $ Fixed 0

instance AtomicParsable (Fixed Word64) where
  fromField = fmap (Fixed . fromIntegral) . parseFixed64
  protoDefault = Fixed 0

instance AtomicParsable (Signed (Fixed Int64)) where
  fromField = fmap (Signed . Fixed . fromIntegral) . parseFixed64
  protoDefault = Signed $ Fixed 0

instance AtomicParsable Float where
  fromField = parseFixed32Float
  protoDefault = 0

instance AtomicParsable Double where
  fromField = parseFixed64Double
  protoDefault = 0

instance AtomicParsable Text where
  fromField = parseText
  protoDefault = mempty

instance AtomicParsable B.ByteString where
  fromField = parseBytes
  protoDefault = mempty

instance AtomicParsable BL.ByteString where
  fromField = fmap BL.fromStrict . parseBytes
  protoDefault = mempty

instance Enum e => AtomicParsable (Enumerated e) where
  fromField = fmap (Enumerated . toEnum . fromIntegral) . parseVarInt
  protoDefault = Enumerated $ toEnum 0

instance AtomicParsable a => AtomicParsable (Maybe a) where
  fromField = fmap Just . fromField
  protoDefault = Nothing

-- | Parse a singular protobuf message field, such as a Bool or Fixed32.
field :: AtomicParsable a => FieldNumber -> Parser a
field = fmap (fromMaybe protoDefault) . one fromField

-- | Type class for fields that can be repeated in the more efficient packed
-- format. This is limited to primitive numeric types.
class Packable a where
  parsePacked :: ParsedField -> Parser [a]

instance Packable Word32 where
  parsePacked = parsePackedVarInt

instance Packable Word64 where
  parsePacked = parsePackedVarInt

instance Packable Int32 where
  parsePacked = parsePackedVarInt

instance Packable Int64 where
  parsePacked = parsePackedVarInt

instance Packable (Fixed Word32) where
  parsePacked = fmap (fmap (Fixed . fromIntegral)) . parsePackedFixed32

instance Packable (Signed (Fixed Int32)) where
  parsePacked = fmap (fmap (Signed . Fixed . fromIntegral)) . parsePackedFixed32

instance Packable (Fixed Word64) where
  parsePacked = fmap (fmap (Fixed . fromIntegral)) . parsePackedFixed64

instance Packable (Signed (Fixed Int64)) where
  parsePacked = fmap (fmap (Signed . Fixed . fromIntegral)) . parsePackedFixed64

instance Packable Float where
  parsePacked = parsePackedFixed32Float

instance Packable Double where
  parsePacked = parsePackedFixed64Double

-- | Parses an unpacked repeated field. See 'repeatedUnpackedList' for a
-- non-streaming version.
repeatedUnpacked :: AtomicParsable a => FieldNumber -> ListT Parser a
repeatedUnpacked fn = do
  pf <- delistify $ parsedFields fn
  lift (fromField pf)

listify :: ListT Parser a -> Parser [a]
listify = P.toListM . enumerate

delistify :: Parser [a] -> ListT Parser a
delistify x = lift x >>= (Select . each)

repeatedUnpackedList :: AtomicParsable a => FieldNumber -> Parser [a]
repeatedUnpackedList = P.toListM . enumerate . repeatedUnpacked

-- | Parses a packed repeated field. Correctly handles the case where a packed
-- field has been split across multiple key/value pairs in the encoded message.
-- Falls back to trying to parse the field as unpacked if packed parsing fails,
-- matching the official implementation's behavior. See 'repeatedPackedList' for
-- a non-streaming version.
repeatedPacked :: (AtomicParsable a, Packable a)
                  => FieldNumber -> ListT Parser a
repeatedPacked fn = delistify $ listify (repeatedPacked' fn)
                                <|> listify (repeatedUnpacked fn)
  where repeatedPacked' fn = (delistify $ parsedFields fn)
                             >>= (delistify . parsePacked)

repeatedPackedList :: (AtomicParsable a, Packable a)
                      => FieldNumber -> Parser [a]
repeatedPackedList = listify . repeatedPacked

-- | Higher-level class for handling decoding any field type in a protobuf
-- message.
class Deserializable a where
  deserialize :: FieldNumber -> Parser a

instance Deserializable Bool where
  deserialize = field

instance Deserializable Int32 where
  deserialize = field

instance Deserializable (Signed Int32) where
  deserialize = field

instance Deserializable Word32 where
  deserialize = field

instance Deserializable Int64 where
  deserialize = field

instance Deserializable (Signed Int64) where
  deserialize = field

instance Deserializable Word64 where
  deserialize = field

instance Deserializable (Fixed Word32) where
  deserialize = field

instance Deserializable (Signed (Fixed Int32)) where
  deserialize = field

instance Deserializable (Fixed Word64) where
  deserialize = field

instance Deserializable (Signed (Fixed Int64)) where
  deserialize = field

instance Deserializable Float where
  deserialize = field

instance Deserializable Double where
  deserialize = field

instance Deserializable Text where
  deserialize = field

instance Deserializable B.ByteString where
  deserialize = field

instance Deserializable BL.ByteString where
  deserialize = field

instance Enum e => Deserializable (Enumerated e) where
  deserialize = field

instance AtomicParsable a => Deserializable [a] where
  deserialize = repeatedUnpackedList

instance (AtomicParsable a, Packable a)
         => Deserializable (Packed [a]) where
  deserialize = fmap Packed . repeatedPackedList
