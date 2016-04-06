{-# LANGUAGE RankNTypes #-}

module Data.Protobuf.Wire.Decode.Parser (
Parser,
-- * General functions
parse,

-- * Combinators
require,
requireMsg,
one,
repeatedUnpacked,

-- * Basic types
int32,
int64,
uint32,
uint64,
fixed32,
fixed64,
sfixed32,
sfixed64,
float,
double,
text,
embedded
) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Loops (whileJust)
import           Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity(runIdentity)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, isNothing)
import           Data.Protobuf.Wire.Decode.Internal
import           Data.Protobuf.Wire.Shared
import           Data.Serialize.Get(runGet, getWord32le, getWord64le)
import           Data.Serialize.IEEE754(getFloat32le, getFloat64le)
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8')
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)

type Parser a = StateT (M.Map FieldNumber [ParsedField]) (Except String) a

parse :: Parser a -> B.ByteString -> Either String a
parse parser bs = do
  tuples <- parseTuples bs
  result <- runIdentity $ runExceptT $ runStateT parser tuples
  return $ fst result

-- |
-- = Decoding 'ParsedField'
-- It is assumed that decisions about how to handle missing data will be handled
-- at a higher level, so our results are in 'Maybe', rather than erroring out.

parsedField :: FieldNumber -> Parser (Maybe ParsedField)
parsedField fn = do
  currMap <- get
  let pfs = M.lookup fn currMap
  case pfs of
    Just (x:xs) -> do let newMap = M.insert fn xs currMap
                      put newMap
                      return $ Just x
    _ -> return Nothing

-- |
-- Consumes all fields with the given field number. This is primarily for
-- unpacked repeated fields. This is also useful for parsing
-- embedded messages, where the spec says that if more than one instance of an
-- embedded message for a given field number is present in the outer message,
-- then they must all be merged.
parsedFields :: FieldNumber -> Parser [ParsedField]
parsedFields fn = do
  currMap <- get
  let pfs = M.lookup fn currMap
  case pfs of
    Just xs -> do let newMap = M.insert fn [] currMap
                  put newMap
                  return xs
    Nothing -> return []

-- |
-- The protobuf standard specifies that if a recipient is expecting only one
-- value in a single field (i.e., not a list of values), but it receives more
-- than one, then the last value should be kept. This function implements that
-- functionality. Discards all but the last value for the given 'FieldNumber'.
dropInits :: FieldNumber -> Parser ()
dropInits fn = do
  currMap <- get
  case M.lookup fn currMap of
    Nothing -> return ()
    Just pfs -> do let pfs' = drop (length pfs - 1) pfs
                   let newMap = M.insert fn pfs' currMap
                   put newMap

-- |
-- Requires a field to be present.
require :: Parser (Maybe a) -> Parser a
require p = do
  result <- p
  case result of
    Nothing -> throwError "Required field missing."
    Just x -> return x

-- |
-- Requires a field to be present, with custom error message.
requireMsg :: Parser (Maybe a) -> String -> Parser a
requireMsg p str = do
  result <- p
  case result of
    Nothing -> throwError str
    Just x -> return x

-- |
-- Specify that one value is expected from this field. Used to ensure that we
-- return the last value with the given field number in the message, in
-- compliance with the protobuf standard.
one :: (FieldNumber -> Parser a) -> (FieldNumber -> Parser a)
one parser fn = dropInits fn >> parser fn

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

parseFixed32 :: Integral a => ParsedField -> Parser a
parseFixed32 (Fixed32Field bs) =
  case runGet getWord32le bs of
    Left e -> throwCerealError "fixed32" e
    Right i -> return $ fromIntegral i
parseFixed32 wrong = throwWireTypeError "fixed32" wrong

parseFixed32Float :: ParsedField -> Parser Float
parseFixed32Float (Fixed32Field bs) =
  case runGet getFloat32le bs of
    Left e -> throwCerealError "fixed32" e
    Right f -> return f
parseFixed32Float wrong = throwWireTypeError "fixed32" wrong

parseFixed64 :: Integral a => ParsedField -> Parser a
parseFixed64 (Fixed64Field bs) =
  case runGet getWord64le bs of
    Left e -> throwCerealError "fixed64" e
    Right i -> return $ fromIntegral i
parseFixed64 wrong = throwWireTypeError "fixed64" wrong

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

parseEmbedded :: Parser a -> ParsedField -> Parser a
parseEmbedded parser (LengthDelimitedField bs) =
  case parse parser bs of
    Left err -> throwError $ "Failed to parse embedded message: " ++ show err
    Right result -> return result
parseEmbedded _ wrong = throwWireTypeError "embedded" wrong

int32 :: FieldNumber -> Parser (Maybe Int32)
int32 fn = parsedField fn >>= mapM parseVarInt

int64 :: FieldNumber -> Parser (Maybe Int64)
int64 fn = parsedField fn >>= mapM parseVarInt

uint32 :: FieldNumber -> Parser (Maybe Word32)
uint32 fn = parsedField fn >>= mapM parseVarInt

uint64:: FieldNumber -> Parser (Maybe Word64)
uint64 fn = parsedField fn >>= mapM parseVarInt

fixed32 :: FieldNumber -> Parser (Maybe Word32)
fixed32 fn = parsedField fn >>= mapM parseFixed32

sfixed32 :: FieldNumber -> Parser (Maybe Int32)
sfixed32 fn = parsedField fn >>= mapM parseFixed32

fixed64 :: FieldNumber -> Parser (Maybe Word64)
fixed64 fn = parsedField fn >>= mapM parseFixed64

sfixed64 :: FieldNumber -> Parser (Maybe Int64)
sfixed64 fn = parsedField fn >>= mapM parseFixed64

float :: FieldNumber -> Parser (Maybe Float)
float fn = parsedField fn >>= mapM parseFixed32Float

double :: FieldNumber -> Parser (Maybe Double)
double fn = parsedField fn >>= mapM parseFixed64Double

text :: FieldNumber -> Parser (Maybe Text)
text fn = parsedField fn >>= mapM parseText

-- | Parses an embedded message. The ProtobufMerge constraint is to satisfy the
-- specification, which states that if the field number of the embedded message
-- is repeated (i.e., multiple embedded messages are provided), the messages
-- are merged.
--
-- Specifically, the protobufs specification states
-- that the latter singular fields should overwrite the former, singular
-- embedded messages are merged, and repeated fields are concatenated.

-- TODO: it's currently possible for someone to try to decode embedded fields
-- incorrectly by just binding 'parser' without using 'embedded', causing an
-- error at runtime. Can we do anything to prevent that with the types?
embedded :: ProtobufMerge a => Parser a -> FieldNumber -> Parser (Maybe a)
embedded parser fn = do
  pfs <-parsedFields fn
  parsedResults <- mapM (parseEmbedded parser) pfs
  case parsedResults of
    [] -> return Nothing
    xs -> return $ Just $ foldl1 protobufMerge xs

repeatedUnpacked :: (FieldNumber -> Parser (Maybe a))
                    -> FieldNumber -> Parser [a]
repeatedUnpacked single fn = whileJust (single fn) return
