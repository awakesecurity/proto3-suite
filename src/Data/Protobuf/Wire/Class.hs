-- | This module provides type classes for encoding and decoding protocol
-- buffers message, as well as a safer alternative to the raw 'Data.Protobuf.Wire'
-- library based on 'GHC.Generics'.
--
-- = Classes
--
-- The 'Primitive' class captures those types which correspond to primitive field
-- types, as defined by the protocol buffers specification. A 'Primitive' type is
-- one which can always be encoded as a single key/value pair in the wire format.
--
-- The 'MessageField' class captures those types which are encoded under a single
-- key in the wire format, i.e. primitives, packed and unpacked lists, and
-- embedded messages.
--
-- The 'Message' class captures types which correspond to protocol buffers messages.
-- Instances of 'Message' can be written by hand for your types by using the
-- functions in the 'Data.Protobuf.Wire.Encode' and 'Data.Protobuf.Wire.Decode'
-- modules. In the case where the message format is determined by your Haskell code,
-- you might prefer to derive your 'Message' instances using generic deriving.
--
-- = Generic Instances
--
-- Using the 'GHC.Generics' approach, instead of generating Haskell code from a
-- .proto file, we write our message formats as Haskell types, and generate a
-- serializer/deserializer pair.
--
-- To use this library, simply derive a 'Generic' instance for your type(s), and
-- use the default `Message` instance.
--
-- For generic 'Message' instances, field numbers are automatically generated,
-- starting at 1. Therefore, adding new fields is a compatible change only at the
-- end of a record. Renaming fields is also safe. You should not use the generic
-- instances if you are starting from an existing .proto file.
--
-- = Strings
--
-- Use 'TL.Text' instead of 'String' for string types inside messages.
--
-- = Example
--
-- > data MultipleFields =
-- >   MultipleFields { multiFieldDouble :: Double
-- >                  , multiFieldFloat  :: Float
-- >                  , multiFieldInt32  :: Int32
-- >                  , multiFieldInt64  :: Int64
-- >                  , multiFieldString :: TL.Text
-- >                  , multiFieldBool   :: Bool
-- >                  } deriving (Show, Generic, Eq)
-- >
-- > instance Message MultipleFields
-- >
-- > serialized = toLazyByteString $ MultipleFields 1.0 1.0 1 1 "hi" True
-- >
-- > deserialized :: MultipleFields
-- > deserialized = case parse (toStrict serialized) of
-- >                  Left e -> error e
-- >                  Right msg -> msg

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Protobuf.Wire.Class
  ( Primitive(..)
  , MessageField(..)
  , Message(..)

  -- * Encoding
  , toLazyByteString

  -- * Decoding
  , HasDefault(..)
  , fromByteString

  -- * Documentation
  , Named(..)
  , Finite(..)
  , message
  , Data.Protobuf.Wire.Class.enum

  -- * Generic Classes
  , GenericMessage(..)
  ) where

import           Control.Arrow (second)
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.Int (Int32, Int64)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import           Data.String (IsString(..))
import           Data.Protobuf.Wire.Encode as Wire
import           Data.Protobuf.Wire.Decode.Parser
import           Data.Protobuf.Wire.Types as Wire
import           Data.Protobuf.Wire.DotProto as DotProto
import           Data.Proxy (Proxy(..))
import           Data.Vector (Vector)
import           Data.Word (Word32, Word64)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Traversable as TR
import           GHC.Generics
import           GHC.TypeLits
import           GHC.Exts (fromList)
import           Safe (toEnumMay)

-- | A class for types with default values per the protocol buffers spec.
class HasDefault a where
  -- | The default value for this type.
  def :: a

  -- | Numeric types default to zero
  default def :: Num a => a
  def = 0

  isDefault :: a -> Bool

  default isDefault :: Eq a => a -> Bool
  isDefault = (== def)

instance HasDefault Int32
instance HasDefault Int64
instance HasDefault Word32
instance HasDefault Word64
instance HasDefault (Signed Int32)
instance HasDefault (Signed Int64)
instance HasDefault (Fixed Word32)
instance HasDefault (Fixed Word64)
instance HasDefault (Signed (Fixed Int32))
instance HasDefault (Signed (Fixed Int64))
instance HasDefault Float
instance HasDefault Double

instance HasDefault Bool where
  def = False

instance HasDefault T.Text where
  def = mempty

instance HasDefault TL.Text where
  def = mempty

instance HasDefault B.ByteString where
  def = mempty

instance HasDefault BL.ByteString where
  def = mempty

instance (Bounded e, Enum e) => HasDefault (Enumerated e) where
  def = case toEnumMay 0 of
          Nothing -> Enumerated (Left 0)
          Just x -> Enumerated (Right x)
  isDefault = on (==) (fmap fromEnum) (enumerated def) . enumerated

instance Eq a => HasDefault (UnpackedVec a) where
  def = mempty

instance Eq a => HasDefault (PackedVec a) where
  def = mempty

instance Eq a => HasDefault (NestedVec a) where
  def = mempty

instance (Eq a, Message a) => HasDefault (Nested a) where
  def = Nested Nothing

-- | This class captures those types whose names need to appear in .proto files.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the name of the type constructor.
class Named a where
  -- | Get the name of a type constructor
  nameOf :: IsString string => Proxy a -> string

  default nameOf :: (IsString string, Generic a, GenericNamed (Rep a)) => Proxy a -> string
  nameOf _ = genericNameOf (Proxy :: Proxy (Rep a))

class GenericNamed (f :: * -> *) where
  genericNameOf :: IsString string => Proxy f -> string

instance Datatype d => GenericNamed (M1 D d f) where
  genericNameOf _ = fromString (datatypeName (undefined :: M1 D d f ()))

-- | Enumerable types with finitely many values.
--
-- This class can be derived whenever a sum type is an instance of 'Generic',
-- and only consists of zero-argument constructors. The derived instance should
-- be compatible with derived `Enum` instances, in the sense that
--
-- > map (toEnum . fst) enumerate
--
-- should enumerate all values of the type without runtime errors.
class Enum a => Finite a where
  -- | Enumerate values of a finite type, along with names of constructors.
  enumerate :: IsString string => Proxy a -> [(Int, string)]

  default enumerate :: (IsString string, Generic a, GenericFinite (Rep a)) => Proxy a -> [(Int, string)]
  enumerate _ = snd (genericEnumerate (Proxy :: Proxy (Rep a)) 0)

-- | Generate metadata for an enum type.
enum :: (Finite e, Named e) => Proxy e -> DotProto
enum pr =
  DotProto
  . (: [])
  . DotProtoPart (MessageName (nameOf pr))
  . Right
  . DotProtoEnum
  . map (second FieldName)
  $ enumerate pr

class GenericFinite (f :: * -> *) where
  genericEnumerate :: IsString string => Proxy f -> Int -> (Int, [(Int, string)])

instance ( GenericFinite f
         , GenericFinite g
         ) => GenericFinite (f :+: g) where
  genericEnumerate _ i =
    let (j, e1) = genericEnumerate (Proxy :: Proxy f) i
        (k, e2) = genericEnumerate (Proxy :: Proxy g) j
    in (k, e1 <> e2)

instance Constructor c => GenericFinite (M1 C c f) where
  genericEnumerate _ i = (i + 1, [ (i, fromString name) ])
    where
      name = conName (undefined :: M1 C c f ())

instance GenericFinite f => GenericFinite (M1 D t f) where
  genericEnumerate _ = genericEnumerate (Proxy :: Proxy f)

instance GenericFinite f => GenericFinite (M1 S t f) where
  genericEnumerate _ = genericEnumerate (Proxy :: Proxy f)

-- | This class captures those types which correspond to primitives in
-- the protocol buffers specification.
--
-- It should be possible to fully reconstruct values of these types from
-- a single 'ParsedField'. Notably, then, `Nested` is not `Primitive` even
-- though it can be 'embedded', since a nested message may by split up over
-- multiple 'embedded' fields.
class Primitive a where
  -- | Encode a primitive value
  encodePrimitive :: FieldNumber -> a -> BB.Builder
  -- | Decode a primitive value
  decodePrimitive :: Parser ParsedField a
  -- | Get the type which represents this type inside another message.
  primType :: Proxy a -> DotProtoPrimType

  default primType :: Named a => Proxy a -> DotProtoPrimType
  primType pr = Named (MessageName (nameOf pr))

-- | Serialize a message as a lazy 'BL.ByteString'.
toLazyByteString :: Message a => a -> BL.ByteString
toLazyByteString = BB.toLazyByteString . encodeMessage (fieldNumber 1)

-- | Parse any message that can be decoded.
fromByteString :: Message a => B.ByteString -> Either [ParseError] a
fromByteString = parse (decodeMessage (fieldNumber 1))

instance Primitive Int32 where
  encodePrimitive = int32
  decodePrimitive = parseInt32
  primType _ = Int32

instance Primitive Int64 where
  encodePrimitive = int64
  decodePrimitive = parseInt64
  primType _ = Int64

instance Primitive Word32 where
  encodePrimitive = uint32
  decodePrimitive = parseWord32
  primType _ = UInt32

instance Primitive Word64 where
  encodePrimitive = uint64
  decodePrimitive = parseWord64
  primType _ = UInt64

instance Primitive (Signed Int32) where
  encodePrimitive num = sint32 num . signed
  decodePrimitive = parseSignedInt32
  primType _ = SInt32

instance Primitive (Signed Int64) where
  encodePrimitive num = sint64 num . signed
  decodePrimitive = parseSignedInt64
  primType _ = SInt64

instance Primitive (Fixed Word32) where
  encodePrimitive num = fixed32 num . fixed
  decodePrimitive = fmap Fixed parseFixed32
  primType _ = DotProto.Fixed32

instance Primitive (Fixed Word64) where
  encodePrimitive num = fixed64 num . fixed
  decodePrimitive = fmap Fixed parseFixed64
  primType _ = DotProto.Fixed64

instance Primitive (Signed (Fixed Int32)) where
  encodePrimitive num = sfixed32 num . fixed . signed
  decodePrimitive = parseFixedInt32
  primType _ = SFixed32

instance Primitive (Signed (Fixed Int64)) where
  encodePrimitive num = sfixed64 num . fixed . signed
  decodePrimitive = parseFixedInt64
  primType _ = SFixed64

instance Primitive Bool where
  encodePrimitive = Wire.enum
  decodePrimitive = parseBool
  primType _ = Bool

instance Primitive Float where
  encodePrimitive = float
  decodePrimitive = parseFixed32Float
  primType _ = Float

instance Primitive Double where
  encodePrimitive = double
  decodePrimitive = parseFixed64Double
  primType _ = Double

instance Primitive T.Text where
  encodePrimitive fn = text fn . TL.fromStrict
  decodePrimitive = fmap TL.toStrict parseText
  primType _ = String

instance Primitive TL.Text where
  encodePrimitive = text
  decodePrimitive = parseText
  primType _ = String

instance Primitive B.ByteString where
  encodePrimitive = bytes
  decodePrimitive = parseByteString
  primType _ = Bytes

instance Primitive BL.ByteString where
  encodePrimitive = bytes'
  decodePrimitive = parseLazyByteString
  primType _ = Bytes

instance (Bounded e, Named e, Enum e) => Primitive (Enumerated e) where
  encodePrimitive num = Wire.enum num . enumify . enumerated
    where enumify (Left i) = i
          enumify (Right x) = fromEnum x
  decodePrimitive = parseEnum
  primType _ = Named (MessageName (nameOf (Proxy :: Proxy e)))

-- | This class captures those types which can appear as message fields in
-- the protocol buffers specification, i.e. 'Primitive' types, or lists of
-- 'Primitive' types
class MessageField a where
  -- | Encode a message field
  encodeMessageField :: FieldNumber -> a -> BB.Builder
  -- | Decode a message field
  decodeMessageField :: Parser (Seq ParsedField) a

  default encodeMessageField :: Primitive a => FieldNumber -> a -> BB.Builder
  encodeMessageField = encodePrimitive

  default decodeMessageField :: (HasDefault a, Primitive a) => Parser (Seq ParsedField) a
  decodeMessageField = fmap (fromMaybe def) $ one decodePrimitive

  -- | Get the type which represents this type inside another message.
  protoType :: Proxy a -> DotProtoType
  default protoType :: Primitive a => Proxy a -> DotProtoType
  protoType = Prim . primType

primDotProto :: DotProtoType -> DotProtoMessage
primDotProto ty = DotProtoMessage [ DotProtoMessagePart (fieldNumber 1) Nothing ty ]

instance MessageField Int32
instance MessageField Int64
instance MessageField Word32
instance MessageField Word64
instance MessageField (Signed Int32)
instance MessageField (Signed Int64)
instance MessageField (Fixed Word32)
instance MessageField (Fixed Word64)
instance MessageField (Signed (Fixed Int32))
instance MessageField (Signed (Fixed Int64))
instance MessageField Bool
instance MessageField Float
instance MessageField Double
instance MessageField T.Text
instance MessageField TL.Text
instance MessageField B.ByteString
instance MessageField BL.ByteString
instance (Bounded e, Named e, Enum e) => MessageField (Enumerated e)

seqToVec :: Seq a -> Vector a
seqToVec = fromList . F.toList

instance (Named a, Message a) => MessageField (Nested a) where
  encodeMessageField _ (Nested Nothing) = mempty
  encodeMessageField fn (Nested (Just x)) = embedded fn . encodeMessage (fieldNumber 1) $ x
  decodeMessageField = fmap Nested
                            (disembed (decodeMessage (fieldNumber 1)))
  protoType _ = Prim (Named (MessageName (nameOf (Proxy :: Proxy a))))

instance Primitive a => MessageField (UnpackedVec a) where
  encodeMessageField fn = foldMap (encodePrimitive fn)
  decodeMessageField = fmap (UnpackedVec . seqToVec) $ repeatedUnpackedList decodePrimitive
  protoType _ = Repeated (primType (Proxy :: Proxy a)) DotProto.Unpacked

instance (Named a, Message a) => MessageField (NestedVec a) where
  encodeMessageField fn = foldMap (embedded fn . encodeMessage (fieldNumber 1))
                          . nestedvec
  decodeMessageField = fmap (NestedVec . seqToVec) $ parseEmbeddedList oneMsg
    where oneMsg = atomicEmbedded (decodeMessage (fieldNumber 1))
  protoType _ = NestedRepeated (Named (MessageName (nameOf (Proxy :: Proxy a))))

instance MessageField (PackedVec Word32) where
  encodeMessageField fn = packedVarints fn . fmap fromIntegral
  decodeMessageField = parsePackedVec parsePackedVarInt
  protoType _ = Repeated UInt32 DotProto.Packed

instance MessageField (PackedVec Word64) where
  encodeMessageField fn = packedVarints fn . fmap fromIntegral
  decodeMessageField = parsePackedVec parsePackedVarInt
  protoType _ = Repeated UInt64 DotProto.Packed

instance MessageField (PackedVec Int32) where
  encodeMessageField fn = packedVarints fn . fmap fromIntegral
  decodeMessageField = parsePackedVec parsePackedVarInt
  protoType _ = Repeated Int32 DotProto.Packed

instance MessageField (PackedVec Int64) where
  encodeMessageField fn = packedVarints fn . fmap fromIntegral
  decodeMessageField = parsePackedVec parsePackedVarInt
  protoType _ = Repeated Int64 DotProto.Packed

instance MessageField (PackedVec (Fixed Word32)) where
  encodeMessageField fn = packedFixed32s fn . fmap fixed
  decodeMessageField = fmap (fmap Fixed) $ parsePackedVec parsePackedFixed32
  protoType _ = Repeated DotProto.Fixed32 DotProto.Packed

instance MessageField (PackedVec (Fixed Word64)) where
  encodeMessageField fn = packedFixed64s fn . fmap fixed
  decodeMessageField = fmap (fmap Fixed) $ parsePackedVec parsePackedFixed64
  protoType _ = Repeated DotProto.Fixed64 DotProto.Packed

instance MessageField (PackedVec (Signed (Fixed Int32))) where
  encodeMessageField fn = packedFixed32s fn . fmap (fromIntegral . fixed . signed)
  decodeMessageField = fmap (fmap (Signed . Fixed)) $ parsePackedVec parsePackedFixed32
  protoType _ = Repeated SFixed32 DotProto.Packed

instance MessageField (PackedVec (Signed (Fixed Int64))) where
  encodeMessageField fn = packedFixed64s fn . fmap (fromIntegral . fixed . signed)
  decodeMessageField = fmap (fmap (Signed . Fixed)) $ parsePackedVec parsePackedFixed64
  protoType _ = Repeated SFixed64 DotProto.Packed

instance MessageField (PackedVec Float) where
  encodeMessageField fn = packedFloats fn
  decodeMessageField = parsePackedVec parsePackedFixed32Float
  protoType _ = Repeated Float DotProto.Packed

instance MessageField (PackedVec Double) where
  encodeMessageField fn = packedDoubles fn
  decodeMessageField = parsePackedVec parsePackedFixed64Double
  protoType _ = Repeated Double DotProto.Packed

parsePackedVec :: Parser ParsedField [a] -> Parser (Seq ParsedField) (PackedVec a)
parsePackedVec p = Parser $ \fs -> fmap (fromList . join . F.toList) $ TR.sequence $ fmap (runParser p) fs

-- | This class captures those types which correspond to protocol buffer messages.
class Message a where
  -- | Encode a message
  encodeMessage :: FieldNumber -> a -> BB.Builder
  -- | Decode a message
  decodeMessage :: FieldNumber -> Parser ParsedFields a
  -- | Generate a .proto message from the type information.
  dotProto :: Proxy a -> DotProtoMessage

  default encodeMessage :: (Generic a, GenericMessage (Rep a)) => FieldNumber -> a -> BB.Builder
  encodeMessage num = genericEncodeMessage num . from

  default decodeMessage :: (Generic a, GenericMessage (Rep a)) => FieldNumber -> Parser ParsedFields a
  decodeMessage = (fmap to .) genericDecodeMessage

  default dotProto :: (Generic a, GenericMessage (Rep a)) => Proxy a -> DotProtoMessage
  dotProto _ = genericDotProto (Proxy :: Proxy (Rep a))

-- | Generate metadata for a message type.
message :: (Message a, Named a) => Proxy a -> DotProto
message pr =
  DotProto
  . (: [])
  . DotProtoPart (MessageName (nameOf pr))
  . Left
  . dotProto
  $ pr

-- * Generic Instances

class GenericMessage f where
  type GenericFieldCount f :: Nat

  genericEncodeMessage :: FieldNumber -> f a -> BB.Builder
  genericDecodeMessage :: FieldNumber -> Parser ParsedFields (f a)
  genericDotProto      :: Proxy f -> DotProtoMessage

instance GenericMessage U1 where
  type GenericFieldCount U1 = 0
  genericEncodeMessage _ = mempty
  genericDecodeMessage _ = return U1
  genericDotProto _ = mempty

instance (KnownNat (GenericFieldCount f), GenericMessage f, GenericMessage g) => GenericMessage (f :*: g) where
  type GenericFieldCount (f :*: g) = GenericFieldCount f + GenericFieldCount g
  genericEncodeMessage num (x :*: y) = genericEncodeMessage num x <> genericEncodeMessage (FieldNumber (getFieldNumber num + offset)) y
    where
      offset = fromIntegral $ natVal (Proxy :: Proxy (GenericFieldCount f))
  genericDecodeMessage num = liftM2 (:*:) (genericDecodeMessage num) (genericDecodeMessage num2)
    where num2 = FieldNumber $ getFieldNumber num + offset
          offset = fromIntegral $ natVal (Proxy :: Proxy (GenericFieldCount f))
  genericDotProto _ = genericDotProto (Proxy :: Proxy f) <> adjust (genericDotProto (Proxy :: Proxy g))
    where
      offset = fromIntegral $ natVal (Proxy :: Proxy (GenericFieldCount f))
      adjust = DotProtoMessage . map adjustPart . runDotProtoMessage
      adjustPart part = part { dotProtoMessagePartFieldNumber = (FieldNumber . (offset +) . getFieldNumber . dotProtoMessagePartFieldNumber) part }

instance (MessageField c, HasDefault c) => GenericMessage (K1 i c) where
  type GenericFieldCount (K1 i c) = 1
  genericEncodeMessage num (K1 x) = if isDefault x
                                       then mempty
                                       else encodeMessageField num x
  genericDecodeMessage num = fmap K1 $ at decodeMessageField num def
  genericDotProto _ = primDotProto (protoType (Proxy :: Proxy c))

instance (Selector s, GenericMessage f) => GenericMessage (M1 S s f) where
  type GenericFieldCount (M1 S t f) = GenericFieldCount f
  genericEncodeMessage num (M1 x) = genericEncodeMessage num x
  genericDecodeMessage num = fmap M1 $ genericDecodeMessage num
  genericDotProto _ = DotProtoMessage
                      . map applyName
                      . runDotProtoMessage
                      $ genericDotProto (Proxy :: Proxy f)
    where
      applyName :: DotProtoMessagePart -> DotProtoMessagePart
      applyName mp = mp { dotProtoMessagePartFieldName = newName }

      newName :: Maybe FieldName
      newName = guard (not (null name)) $> FieldName name
        where
          name = selName (undefined :: S1 s f ())

instance GenericMessage f => GenericMessage (M1 C t f) where
  type GenericFieldCount (M1 C t f) = GenericFieldCount f
  genericEncodeMessage num (M1 x) = genericEncodeMessage num x
  genericDecodeMessage num = fmap M1 $ genericDecodeMessage num
  genericDotProto _ = genericDotProto (Proxy :: Proxy f)

instance GenericMessage f => GenericMessage (M1 D t f) where
  type GenericFieldCount (M1 D t f) = GenericFieldCount f
  genericEncodeMessage num (M1 x) = genericEncodeMessage num x
  genericDecodeMessage num = fmap M1 $ genericDecodeMessage num
  genericDotProto _ = genericDotProto (Proxy :: Proxy f)
