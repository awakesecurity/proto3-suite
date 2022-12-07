-- | This module provides type classes for encoding and decoding protocol
-- buffers message, as well as a safer alternative to the raw 'Proto3.Wire'
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
-- functions in the 'Proto3.Suite.Encode' and 'Proto3.Suite.Decode'
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

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Proto3.Suite.Class
  ( Primitive(..)
  , MessageField(..)
  , Message(..)

  -- * Encoding
  , toLazyByteString

  -- * Decoding
  , HasDefault(..)
  , fromByteString
  , fromB64
  , coerceOver
  , unsafeCoerceOver

  -- * Documentation
  , Named(..)
  , Finite(..)
  , message
  , Proto3.Suite.Class.enum

  -- * Generic Classes
  , GenericMessage(..)
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BL
import           Data.Coerce            (Coercible, coerce)
import qualified Data.Foldable          as Foldable
import           Data.Functor           (($>))
import           Data.Int               (Int32, Int64)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Proxy             (Proxy (..))
import           Data.String            (IsString (..))
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Short        as TS
import qualified Data.Traversable       as TR
import qualified Data.Vector            as Vector
import           Data.Vector            (Vector)
import           Data.Word              (Word32, Word64)
import           GHC.Exts               (fromList, Proxy#, proxy#)
import           GHC.Generics
import           GHC.TypeLits
import           Google.Protobuf.Wrappers.Polymorphic (Wrapped(..))
import           Proto3.Suite.DotProto
import qualified Proto3.Suite.Types
import           Proto3.Suite.Types     hiding (Bytes, String)
import           Proto3.Wire
import           Proto3.Wire.Decode     (ParseError, Parser (..), RawField,
                                         RawMessage, RawPrimitive, runParser)
import qualified Proto3.Wire.Decode     as Decode
import qualified Proto3.Wire.Encode     as Encode
import           Unsafe.Coerce          (unsafeCoerce)

#ifdef LARGE_RECORDS
import qualified Data.Record.Generic as LG
import qualified Data.Record.Generic.GHC as LG
import qualified Data.Record.Generic.Rep as LG
#endif

-- | Pass through those values that are outside the enum range;
-- this is for forward compatibility as enumerations are extended.
codeFromEnumerated :: ProtoEnum e => Enumerated e -> Int32
codeFromEnumerated = either id fromProtoEnum . enumerated
{-# INLINE codeFromEnumerated #-}

-- | Values inside the enum range are in Right, the rest in Left;
-- this is for forward compatibility as enumerations are extended.
codeToEnumerated :: ProtoEnum e => Int32 -> Enumerated e
codeToEnumerated code =
  Enumerated $ maybe (Left code) Right (toProtoEnumMay code)
{-# INLINE codeToEnumerated #-}

-- | A class for types with default values per the protocol buffers spec.
class HasDefault a where
  -- | The default value for this type.
  def :: a

  default def :: (Generic a, GenericHasDefault (Rep a)) => a
  def = to (genericDef @(Rep a))

  isDefault :: a -> Bool

  default isDefault :: Eq a => a -> Bool
  isDefault = (== def)

-- | Do not encode the default value
omittingDefault
  :: HasDefault a
  => (a -> Encode.MessageBuilder)
  -> a
  -> Encode.MessageBuilder
omittingDefault f p
  | isDefault p = mempty
  | otherwise = f p

-- -- | Numeric types default to zero
-- instance Num a => HasDefault a where def = 0

instance HasDefault Int where def = 0
instance HasDefault Integer where def = 0

instance HasDefault Int32 where def = 0
instance HasDefault Int64 where def = 0
instance HasDefault Word32 where def = 0
instance HasDefault Word64 where def = 0
instance HasDefault (Signed Int32) where def = 0
instance HasDefault (Signed Int64) where def = 0
-- | Used in generated records to represent @sfixed32@
instance HasDefault (Fixed Int32) where def = 0
-- | Used in generated records to represent @sfixed64@
instance HasDefault (Fixed Int64) where def = 0
instance HasDefault (Fixed Word32) where def = 0
instance HasDefault (Fixed Word64) where def = 0
instance HasDefault (Signed (Fixed Int32)) where def = 0
instance HasDefault (Signed (Fixed Int64)) where def = 0
instance HasDefault Float where def = 0
instance HasDefault Double where def = 0

instance HasDefault Bool where
  def = False

instance HasDefault T.Text where
  def = mempty

deriving via T.Text instance HasDefault (Proto3.Suite.Types.String T.Text)

instance HasDefault TL.Text where
  def = mempty

deriving via TL.Text instance HasDefault (Proto3.Suite.Types.String TL.Text)

instance HasDefault TS.ShortText where
  def = mempty

deriving via TS.ShortText instance HasDefault (Proto3.Suite.Types.String TS.ShortText)

instance HasDefault B.ByteString where
  def = mempty

deriving via B.ByteString instance HasDefault (Proto3.Suite.Types.Bytes B.ByteString)

instance HasDefault BL.ByteString where
  def = mempty

deriving via BL.ByteString instance HasDefault (Proto3.Suite.Types.Bytes BL.ByteString)

instance ProtoEnum e => HasDefault (Enumerated e) where
  def = codeToEnumerated 0
  isDefault = (== 0) . codeFromEnumerated

deriving via (a :: *) instance HasDefault a => HasDefault (Wrapped a)

instance HasDefault (UnpackedVec a) where
  def = mempty
  isDefault = null . unpackedvec

instance HasDefault (PackedVec a) where
  def = mempty
  isDefault = null . packedvec

instance HasDefault (NestedVec a) where
  def = mempty
  isDefault = null . nestedvec

instance HasDefault (Nested a) where
  def = Nested Nothing
  isDefault = isNothing . nested

instance (HasDefault a) => HasDefault (ForceEmit a) where
  def       = ForceEmit def
  isDefault = isDefault . forceEmit

-- | Used in fields of generated records to represent an unwrapped
-- 'PackedVec'/'UnpackedVec'
instance HasDefault (Vector a) where
  def       = mempty
  isDefault = null

-- | Used in generated records to represent an unwrapped 'Nested'
instance HasDefault (Maybe a) where
  def       = Nothing
  isDefault = isNothing

instance HasDefault (M.Map k v) where
  def = M.empty
  isDefault = M.null

class GenericHasDefault (f :: * -> *) where
  genericDef :: f x
instance HasDefault f => GenericHasDefault (K1 i f) where
  genericDef = K1 (def @f)
instance (GenericHasDefault f, GenericHasDefault g) => GenericHasDefault (f :*: g) where
  genericDef = genericDef @f :*: genericDef @g
instance (GenericHasDefault f, GenericHasDefault g) => GenericHasDefault (f :+: g) where
  genericDef = L1 (genericDef @f)
instance GenericHasDefault U1 where
  genericDef = U1 -- unit constructor
instance (Constructor i, GenericHasDefault f) => GenericHasDefault (C1 i f) where
  genericDef = M1 (genericDef @f)
instance (Datatype i, GenericHasDefault f) => GenericHasDefault (D1 i f) where
  genericDef = M1 (genericDef @f)
instance (Selector i, GenericHasDefault f) => GenericHasDefault (S1 i f) where
  genericDef = M1 (genericDef @f)

#ifdef LARGE_RECORDS

instance (LG.Generic a, LG.Constraints a HasDefault) => GenericHasDefault (LG.ThroughLRGenerics a) where
  genericDef = LG.WrapThroughLRGenerics $ LG.to $ LG.cpure (Proxy @HasDefault) (pure def)

#endif

-- | This class captures those types whose names need to appear in .proto files.
--
-- It has a default implementation for any data type which is an instance of the
-- 'Generic' class, which will extract the name of the type constructor.
class Named a where
  -- | Get the name of a type constructor
  nameOf :: IsString string => Proxy# a -> string

  default nameOf :: (IsString string, GenericNamed (Rep a)) => Proxy# a -> string
  nameOf _ = genericNameOf (proxy# :: Proxy# (Rep a))

class GenericNamed (f :: * -> *) where
  genericNameOf :: IsString string => Proxy# f -> string

instance Datatype d => GenericNamed (M1 D d f) where
  genericNameOf _ = fromString (datatypeName (undefined :: M1 D d f ()))

instance NameOfWrapperFor a => Named (Wrapped a) where
  nameOf _ = nameOfWrapperFor @a
  {-# INLINE nameOf #-}

-- | Defines the name to be returned by @`HsProtobuf.Named` (`Wrapped` a)@.
class NameOfWrapperFor a where
  nameOfWrapperFor :: forall string . IsString string => string

instance NameOfWrapperFor Double where
  nameOfWrapperFor = "DoubleValue"

instance NameOfWrapperFor Float where
  nameOfWrapperFor = "FloatValue"

instance NameOfWrapperFor Int64 where
  nameOfWrapperFor = "Int64Value"

instance NameOfWrapperFor Word64 where
  nameOfWrapperFor = "UInt64Value"

instance NameOfWrapperFor Int32 where
  nameOfWrapperFor = "Int32Value"

instance NameOfWrapperFor Word32 where
  nameOfWrapperFor = "UInt32Value"

instance NameOfWrapperFor Bool where
  nameOfWrapperFor = "BoolValue"

instance NameOfWrapperFor (Proto3.Suite.Types.String a) where
  nameOfWrapperFor = "StringValue"

instance NameOfWrapperFor (Proto3.Suite.Types.Bytes a) where
  nameOfWrapperFor = "BytesValue"

-- | Enumerable types with finitely many values.
--
-- This class can be derived whenever a sum type is an instance of 'Generic',
-- and only consists of zero-argument constructors. The derived instance should
-- be compatible with `ProtoEnum` instances, in the sense that
--
-- > map (fromJust . toProtoEnumMay . snd) enumerate
--
-- should enumerate all values of the type without runtime errors.
class ProtoEnum a => Finite a where
  -- | Enumerate values of a finite type, along with names of constructors.
  enumerate :: IsString string => Proxy# a -> [(string, Int32)]

  default enumerate ::
    (IsString string, Generic a, GenericFinite (Rep a)) =>
    Proxy# a -> [(string, Int32)]
  enumerate _ =
    fmap (fromProtoEnum . (to :: Rep a p -> a)) <$> genericEnumerate

-- | Generate metadata for an enum type.
enum :: (Finite e, Named e) => Proxy# e -> DotProtoDefinition
enum pr = DotProtoEnum "" (Single $ nameOf pr) (map enumField $ enumerate pr)
  where
    enumField (name, value) = DotProtoEnumField (Single name) value []

class GenericFinite (f :: * -> *) where
  genericEnumerate :: IsString string => [(string, f p)]

instance ( GenericFinite f
         , GenericFinite g
         ) => GenericFinite (f :+: g) where
  genericEnumerate =
    (fmap L1 <$> genericEnumerate) <>
    (fmap R1 <$> genericEnumerate)

instance Constructor c => GenericFinite (M1 C c U1) where
  genericEnumerate = [ (fromString name, M1 U1) ]
    where
      name = conName (undefined :: M1 C c f ())

instance GenericFinite f => GenericFinite (M1 D t f) where
  genericEnumerate = fmap M1 <$> genericEnumerate

-- | This class captures those types which correspond to primitives in
-- the protocol buffers specification.
--
-- It should be possible to fully reconstruct values of these types from
-- a single 'RawPrimitive'. Notably, then, `Nested` is not `Primitive` even
-- though it can be 'embedded', since a nested message may by split up over
-- multiple 'embedded' fields.
class Primitive a where
  -- | Encode a primitive value
  encodePrimitive :: FieldNumber -> a -> Encode.MessageBuilder
  -- | Decode a primitive value
  decodePrimitive :: Parser RawPrimitive a
  -- | Get the type which represents this type inside another message.
  primType :: Proxy# a -> DotProtoPrimType

  default primType :: Named a => Proxy# a -> DotProtoPrimType
  primType pr = Named (Single (nameOf pr))

-- | Serialize a message as a lazy 'BL.ByteString'.
toLazyByteString :: Message a => a -> BL.ByteString
toLazyByteString = Encode.toLazyByteString . encodeMessage (fieldNumber 1)

-- | Parse any message that can be decoded.
fromByteString :: Message a => B.ByteString -> Either ParseError a
fromByteString = Decode.parse (decodeMessage (fieldNumber 1))

-- | As 'fromByteString', except the input bytestring is base64-encoded.
fromB64 :: Message a => B.ByteString -> Either ParseError a
fromB64 = fromByteString . B64.decodeLenient

-- | Like `coerce` but lets you avoid specifying a type constructor
-- (such a as parser) that is common to both the input and output types.
coerceOver :: forall a b f . Coercible (f a) (f b) => f a -> f b
coerceOver = coerce

-- | Like `unsafeCoerce` but lets you avoid specifying a type constructor
-- (such a as parser) that is common to both the input and output types.
unsafeCoerceOver :: forall a b f . f a -> f b
unsafeCoerceOver = unsafeCoerce

instance Primitive Int32 where
  encodePrimitive = Encode.int32
  decodePrimitive = Decode.int32
  primType _ = Int32

instance Primitive Int64 where
  encodePrimitive = Encode.int64
  decodePrimitive = Decode.int64
  primType _ = Int64

instance Primitive Word32 where
  encodePrimitive = Encode.uint32
  decodePrimitive = Decode.uint32
  primType _ = UInt32

instance Primitive Word64 where
  encodePrimitive = Encode.uint64
  decodePrimitive = Decode.uint64
  primType _ = UInt64

instance Primitive (Signed Int32) where
  encodePrimitive num = Encode.sint32 num . coerce
  decodePrimitive = coerce Decode.sint32
  primType _ = SInt32

instance Primitive (Signed Int64) where
  encodePrimitive num = Encode.sint64 num . coerce
  decodePrimitive = coerce Decode.sint64
  primType _ = SInt64

instance Primitive (Fixed Word32) where
  encodePrimitive num = Encode.fixed32 num . coerce
  decodePrimitive = coerce Decode.fixed32
  primType _ = Fixed32

instance Primitive (Fixed Word64) where
  encodePrimitive num = Encode.fixed64 num . coerce
  decodePrimitive = coerce Decode.fixed64
  primType _ = Fixed64

instance Primitive (Signed (Fixed Int32)) where
  encodePrimitive num = Encode.sfixed32 num . coerce
  decodePrimitive = coerce Decode.sfixed32
  primType _ = SFixed32

instance Primitive (Signed (Fixed Int64)) where
  encodePrimitive num = Encode.sfixed64 num . coerce
  decodePrimitive = coerce Decode.sfixed64
  primType _ = SFixed64

instance Primitive Bool where
  encodePrimitive = Encode.bool
  decodePrimitive = Decode.bool
  primType _ = Bool

instance Primitive Float where
  encodePrimitive = Encode.float
  decodePrimitive = Decode.float
  primType _ = Float

instance Primitive Double where
  encodePrimitive = Encode.double
  decodePrimitive = Decode.double
  primType _ = Double

instance Primitive T.Text where
  encodePrimitive fn = Encode.text fn . TL.fromStrict
  decodePrimitive = fmap TL.toStrict Decode.text
  primType _ = String

deriving via T.Text instance Primitive (Proto3.Suite.Types.String T.Text)

instance Primitive TL.Text where
  encodePrimitive = Encode.text
  decodePrimitive = Decode.text
  primType _ = String

deriving via TL.Text instance Primitive (Proto3.Suite.Types.String TL.Text)

instance Primitive TS.ShortText where
  encodePrimitive = Encode.shortText
  decodePrimitive = Decode.shortText
  primType _ = String

deriving via TS.ShortText instance Primitive (Proto3.Suite.Types.String TS.ShortText)

instance Primitive B.ByteString where
  encodePrimitive = Encode.byteString
  decodePrimitive = Decode.byteString
  primType _ = Bytes

deriving via B.ByteString instance Primitive (Proto3.Suite.Types.Bytes B.ByteString)

instance Primitive BL.ByteString where
  encodePrimitive = Encode.lazyByteString
  decodePrimitive = Decode.lazyByteString
  primType _ = Bytes

deriving via BL.ByteString instance Primitive (Proto3.Suite.Types.Bytes BL.ByteString)

instance forall e. (Named e, ProtoEnum e) => Primitive (Enumerated e) where
  encodePrimitive num = either (Encode.int32 num) (Encode.enum num) . enumerated
  decodePrimitive = coerce
    @(Parser RawPrimitive (Either Int32 e))
    @(Parser RawPrimitive (Enumerated e))
    Decode.enum
  primType _ = Named (Single (nameOf (proxy# :: Proxy# e)))

instance (Primitive a) => Primitive (ForceEmit a) where
  encodePrimitive num = encodePrimitive num . forceEmit
  decodePrimitive     = coerce @(Parser RawPrimitive a) @(Parser RawPrimitive (ForceEmit a)) decodePrimitive
  primType _          = primType (proxy# :: Proxy# a)

-- | This class captures those types which can appear as message fields in
-- the protocol buffers specification, i.e. 'Primitive' types, or lists of
-- 'Primitive' types
class MessageField a where
  -- | Encode a message field
  encodeMessageField :: FieldNumber -> a -> Encode.MessageBuilder
  -- | Decode a message field
  decodeMessageField :: Parser RawField a

  default encodeMessageField :: (HasDefault a, Primitive a)
                             => FieldNumber -> a -> Encode.MessageBuilder
  encodeMessageField num x
    | isDefault x = mempty
    | otherwise = encodePrimitive num x

  default decodeMessageField :: (HasDefault a, Primitive a) => Parser RawField a
  decodeMessageField = one decodePrimitive def

  -- | Get the type which represents this type inside another message.
  protoType :: Proxy# a -> DotProtoField
  default protoType :: Primitive a => Proxy# a -> DotProtoField
  protoType p = messageField (Prim $ primType p) Nothing

messageField :: DotProtoType -> Maybe Packing -> DotProtoField
messageField ty packing = DotProtoField
    { dotProtoFieldNumber = fieldNumber 1
    , dotProtoFieldType = ty
    , dotProtoFieldName = Anonymous
    , dotProtoFieldOptions = packingOption
    , dotProtoFieldComment = ""
    }
  where
    packingOption = maybe [] (toDotProtoOption . isPacked) packing

    toDotProtoOption b = [DotProtoOption (Single "packed") (BoolLit b)]

    isPacked PackedField   = True
    isPacked UnpackedField = False

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
deriving via T.Text instance MessageField (Proto3.Suite.Types.String T.Text)
instance MessageField TL.Text
deriving via TL.Text instance MessageField (Proto3.Suite.Types.String TL.Text)
instance MessageField TS.ShortText
deriving via TS.ShortText instance MessageField (Proto3.Suite.Types.String TS.ShortText)
instance MessageField B.ByteString
deriving via B.ByteString instance MessageField (Proto3.Suite.Types.Bytes B.ByteString)
instance MessageField BL.ByteString
deriving via BL.ByteString instance MessageField (Proto3.Suite.Types.Bytes BL.ByteString)
instance (Named e, ProtoEnum e) => MessageField (Enumerated e)

instance (Ord k, Primitive k, MessageField k, Primitive v, MessageField v) => MessageField (M.Map k v) where
  encodeMessageField num = foldMap (Encode.embedded num . encodeMessage (fieldNumber 1)) . M.toList

  -- Data.Map.fromList will retain the last key/value mapping. From the spec:
  --
  -- > When parsing from the wire or when merging, if there are duplicate map
  -- > keys the last key seen is used.
  decodeMessageField = M.fromList . Foldable.toList
                       <$> repeated (Decode.embedded' (decodeMessage (fieldNumber 1)))
  protoType _ = messageField (Map (primType (proxy# :: Proxy# k)) (primType (proxy# :: Proxy# v))) Nothing

instance {-# OVERLAPS #-} (Ord k, Primitive k, Named v, Message v, MessageField k) => MessageField (M.Map k (Nested v)) where
  encodeMessageField num = foldMap (Encode.embedded num . encodeMessage (fieldNumber 1)) . M.toList

  -- Data.Map.fromList will retain the last key/value mapping. From the spec:
  --
  -- > When parsing from the wire or when merging, if there are duplicate map
  -- > keys the last key seen is used.
  decodeMessageField = M.fromList . Foldable.toList
                       <$> repeated (Decode.embedded' (decodeMessage (fieldNumber 1)))
  protoType _ = messageField (Map (primType (proxy# :: Proxy# k)) (Named . Single $ nameOf (proxy# :: Proxy# v))) Nothing

instance (HasDefault a, Primitive a) => MessageField (ForceEmit a) where
  encodeMessageField = encodePrimitive

instance (Named a, Message a) => MessageField (Nested a) where
  encodeMessageField num = foldMap (Encode.embedded num . encodeMessage (fieldNumber 1))
                           . coerce @(Nested a) @(Maybe a)
  decodeMessageField = coerce @(Parser RawField (Maybe a)) @(Parser RawField (Nested a))
                       (Decode.embedded (decodeMessage (fieldNumber 1)))
  protoType _ = messageField (Prim . Named . Single $ nameOf (proxy# :: Proxy# a)) Nothing

instance Primitive a => MessageField (UnpackedVec a) where
  encodeMessageField fn = Encode.vectorMessageBuilder (encodePrimitive fn) . unpackedvec
  decodeMessageField =
    UnpackedVec . fromList . Foldable.toList <$> repeated decodePrimitive
  protoType _ = messageField (Repeated $ primType (proxy# :: Proxy# a)) (Just UnpackedField)

instance forall a. (Named a, Message a) => MessageField (NestedVec a) where
  encodeMessageField fn = Encode.vectorMessageBuilder (Encode.embedded fn . encodeMessage (fieldNumber 1)) . nestedvec
  decodeMessageField =
      fmap (coerce @(Vector a) @(NestedVec a) . fromList . Foldable.toList)
           (repeated (Decode.embedded' oneMsg))
    where
      oneMsg :: Parser RawMessage a
      oneMsg = decodeMessage (fieldNumber 1)
  protoType _ = messageField (NestedRepeated . Named . Single $ nameOf (proxy# :: Proxy# a)) Nothing

instance (Named e, ProtoEnum e) => MessageField (PackedVec (Enumerated e)) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV fromIntegral fn) . Vector.map codeFromEnumerated . packedvec
  decodeMessageField = decodePacked (map (codeToEnumerated . fromIntegral) <$> Decode.packedVarints @Word64)
  protoType _ = messageField (Repeated . Named . Single $ nameOf (proxy# :: Proxy# e)) (Just PackedField)

instance MessageField (PackedVec Bool) where
  encodeMessageField fn = omittingDefault (Encode.packedBoolsV id fn) . packedvec
  decodeMessageField = fmap (fmap toBool) (decodePacked Decode.packedVarints)
    where
      toBool :: Word64 -> Bool
      toBool 1 = True
      toBool _ = False
  protoType _ = messageField (Repeated Bool) (Just PackedField)

instance MessageField (PackedVec Word32) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV fromIntegral fn) . packedvec
  decodeMessageField = decodePacked Decode.packedVarints
  protoType _ = messageField (Repeated UInt32) (Just PackedField)

instance MessageField (PackedVec Word64) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV id fn) . packedvec
  decodeMessageField = decodePacked Decode.packedVarints
  protoType _ = messageField (Repeated UInt64) (Just PackedField)

instance MessageField (PackedVec Int32) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV fromIntegral fn) . packedvec
  decodeMessageField = decodePacked Decode.packedVarints
  protoType _ = messageField (Repeated Int32) (Just PackedField)

instance MessageField (PackedVec Int64) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV fromIntegral fn) . packedvec
  decodeMessageField = decodePacked Decode.packedVarints
  protoType _ = messageField (Repeated Int64) (Just PackedField)

instance MessageField (PackedVec (Signed Int32)) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV zigZag fn) . coerce @_ @(Vector Int32)
    where
      zigZag = fromIntegral . Encode.zigZagEncode

  decodeMessageField = decodePacked (fmap (fmap zagZig) Decode.packedVarints)
    where
      -- This type signature is important: `Decode.zigZagDecode` will not undo
      -- `Encode.zigZagEncode` if given a signed value with the high order bit
      -- set. So we don't allow GHC to infer a signed input type.
      zagZig :: Word32 -> Signed Int32
      zagZig = Signed . fromIntegral . Decode.zigZagDecode

  protoType _ = messageField (Repeated SInt32) (Just PackedField)

instance MessageField (PackedVec (Signed Int64)) where
  encodeMessageField fn = omittingDefault (Encode.packedVarintsV zigZag fn) . coerce @_ @(Vector Int64)
    where
      zigZag = fromIntegral . Encode.zigZagEncode

  decodeMessageField = decodePacked (fmap (fmap zagZig) Decode.packedVarints)
    where
      -- This type signature is important: `Decode.zigZagDecode` will not undo
      -- `Encode.zigZagEncode` if given a signed value with the high order bit
      -- set. So we don't allow GHC to infer a signed input type.
      zagZig :: Word64 -> Signed Int64
      zagZig = Signed . fromIntegral . Decode.zigZagDecode

  protoType _ = messageField (Repeated SInt64) (Just PackedField)


instance MessageField (PackedVec (Fixed Word32)) where
  encodeMessageField fn = omittingDefault (Encode.packedFixed32V id fn) . coerce @_ @(Vector Word32)
  decodeMessageField = coerce @(Parser RawField (PackedVec Word32))
                              @(Parser RawField (PackedVec (Fixed Word32)))
                              (decodePacked Decode.packedFixed32)
  protoType _ = messageField (Repeated Fixed32) (Just PackedField)

instance MessageField (PackedVec (Fixed Word64)) where
  encodeMessageField fn = omittingDefault (Encode.packedFixed64V id fn) . coerce @_ @(Vector Word64)
  decodeMessageField = coerce @(Parser RawField (PackedVec Word64))
                              @(Parser RawField (PackedVec (Fixed Word64)))
                              (decodePacked Decode.packedFixed64)
  protoType _ = messageField (Repeated Fixed64) (Just PackedField)

instance MessageField (PackedVec (Signed (Fixed Int32))) where
  encodeMessageField fn = omittingDefault (Encode.packedFixed32V fromIntegral fn) . coerce @_ @(Vector Int32)
  decodeMessageField = coerce @(Parser RawField (PackedVec Int32))
                              @(Parser RawField (PackedVec (Signed (Fixed Int32))))
                             (decodePacked Decode.packedFixed32)
  protoType _ = messageField (Repeated SFixed32) (Just PackedField)

instance MessageField (PackedVec (Signed (Fixed Int64))) where
  encodeMessageField fn = omittingDefault (Encode.packedFixed64V fromIntegral fn) . coerce @_ @(Vector Int64)
  decodeMessageField = coerce @(Parser RawField (PackedVec Int64))
                              @(Parser RawField (PackedVec (Signed (Fixed Int64))))
                              (decodePacked Decode.packedFixed64)
  protoType _ = messageField (Repeated SFixed64) (Just PackedField)

instance MessageField (PackedVec Float) where
  encodeMessageField fn = omittingDefault (Encode.packedFloatsV id fn) . packedvec
  decodeMessageField = decodePacked Decode.packedFloats
  protoType _ = messageField (Repeated Float) (Just PackedField)

instance MessageField (PackedVec Double) where
  encodeMessageField fn = omittingDefault (Encode.packedDoublesV id fn) . packedvec
  decodeMessageField = decodePacked Decode.packedDoubles
  protoType _ = messageField (Repeated Double) (Just PackedField)

instance (MessageField e, KnownSymbol comments) => MessageField (e // comments) where
  encodeMessageField fn = encodeMessageField fn . unCommented
  decodeMessageField = coerce @(Parser RawField e)
                              @(Parser RawField (Commented comments e))
                              decodeMessageField
  protoType p = (protoType (lowerProxy1 p))
                  { dotProtoFieldComment = symbolVal (lowerProxy2 p) }
    where
      lowerProxy1 :: forall k f (a :: k). Proxy# (f a) -> Proxy# a
      lowerProxy1 _ = proxy#

      lowerProxy2 :: forall k f (a :: k) b. Proxy# (f a b) -> Proxy a
      lowerProxy2 _ = Proxy

decodePacked
  :: Parser RawPrimitive [a]
  -> Parser RawField (PackedVec a)
decodePacked = Parser
             . fmap (fmap (pack . Foldable.toList))
             . TR.traverse
             . runParser
  where
    pack :: forall a. [[a]] -> PackedVec a
    pack = fromList . join . reverse


-- | This class captures those types which correspond to protocol buffer messages.
class Message a where
  -- | Encode a message
  encodeMessage :: FieldNumber -> a -> Encode.MessageBuilder
  -- | Decode a message
  decodeMessage :: FieldNumber -> Parser RawMessage a
  -- | Generate a .proto message from the type information.
  dotProto :: Proxy# a -> [DotProtoField]

  default encodeMessage :: (Generic a, GenericMessage (Rep a))
                        => FieldNumber -> a -> Encode.MessageBuilder
  encodeMessage num = genericEncodeMessage num . from

  default decodeMessage :: (Generic a, GenericMessage (Rep a))
                        => FieldNumber -> Parser RawMessage a
  decodeMessage = fmap to . genericDecodeMessage

  default dotProto :: GenericMessage (Rep a)
                   => Proxy# a -> [DotProtoField]
  dotProto _ = genericDotProto (proxy# :: Proxy# (Rep a))

instance (MessageField k, MessageField v) => Message (k, v)

instance (MessageField a, Primitive a) => Message (Wrapped a) where
  encodeMessage _ (Wrapped v) = encodeMessageField (FieldNumber 1) v
  {-# INLINABLE encodeMessage #-}
  decodeMessage _ = Wrapped <$> at decodeMessageField (FieldNumber 1)
  {-# INLINABLE decodeMessage #-}
  dotProto _ =
    [ DotProtoField
        (FieldNumber 1)
        (Prim (primType (proxy# :: Proxy# a)))
        (Single "value")
        []
        ""
    ]

-- | Generate metadata for a message type.
message :: (Message a, Named a) => Proxy# a -> DotProtoDefinition
message proxy = DotProtoMessage ""
                                (Single $ nameOf proxy)
                                (DotProtoMessageField <$> dotProto proxy)

-- * Wrapped Type Instances

encodeWrapperMessage
  :: MessageField a
  => FieldNumber
  -> a
  -> Encode.MessageBuilder
encodeWrapperMessage _ x = encodeMessageField (FieldNumber 1) x

decodeWrapperMessage
  :: MessageField a
  => FieldNumber
  -> Decode.Parser Decode.RawMessage a
decodeWrapperMessage _ = at decodeMessageField (FieldNumber 1)

dotProtoWrapper :: Primitive a => Proxy# a -> [DotProtoField]
dotProtoWrapper proxy =
  [ DotProtoField
      (FieldNumber 1)
      (Prim (primType proxy))
      (Single "value")
      []
      ""
  ]

instance Message Double where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message Float where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message Int64 where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message Word64 where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message Int32 where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message Word32 where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message Bool where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message T.Text where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

deriving via T.Text instance Message (Proto3.Suite.Types.String T.Text)

instance Message TL.Text where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

deriving via TL.Text instance Message (Proto3.Suite.Types.String TL.Text)

instance Message TS.ShortText where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

deriving via TS.ShortText instance Message (Proto3.Suite.Types.String TS.ShortText)

instance Message B.ByteString where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

instance Message BL.ByteString where
  encodeMessage = encodeWrapperMessage
  decodeMessage = decodeWrapperMessage
  dotProto = dotProtoWrapper

-- * Generic Instances

class GenericMessage (f :: * -> *) where
  type GenericFieldCount f :: Nat

  genericEncodeMessage :: FieldNumber -> f a -> Encode.MessageBuilder
  genericDecodeMessage :: FieldNumber -> Parser RawMessage (f a)
  genericDotProto      :: Proxy# f -> [DotProtoField]

instance GenericMessage U1 where
  type GenericFieldCount U1 = 0
  genericEncodeMessage _ = mempty
  genericDecodeMessage _ = pure U1
  genericDotProto _      = mempty

instance (KnownNat (GenericFieldCount f), GenericMessage f, GenericMessage g)
           => GenericMessage (f :*: g)
  where
    type GenericFieldCount (f :*: g) = GenericFieldCount f + GenericFieldCount g
    genericEncodeMessage num (x :*: y) =
        genericEncodeMessage num x <>
        genericEncodeMessage (FieldNumber (getFieldNumber num + offset)) y
      where
        offset = fromIntegral $ natVal (Proxy @(GenericFieldCount f))

    genericDecodeMessage num =
        liftA2 (:*:) (genericDecodeMessage num)
                     (genericDecodeMessage num2)
      where
        num2 = FieldNumber $ getFieldNumber num + offset
        offset = fromIntegral $ natVal (Proxy @(GenericFieldCount f))

    genericDotProto _ =
        genericDotProto (proxy# :: Proxy# f) <>
        adjust (genericDotProto (proxy# :: Proxy# g))
      where
        offset = fromIntegral $ natVal (Proxy @(GenericFieldCount f))
        adjust = map adjustPart
        adjustPart part = part
          { dotProtoFieldNumber = FieldNumber . (offset +)
                                  . getFieldNumber . dotProtoFieldNumber
                                  $ part
          }

instance MessageField c => GenericMessage (K1 i c) where
  type GenericFieldCount (K1 i c) = 1
  genericEncodeMessage num (K1 x) = encodeMessageField num x
  genericDecodeMessage num        = K1 <$> at decodeMessageField num
  genericDotProto _               = [protoType (proxy# :: Proxy# c)]

instance (Selector s, GenericMessage f) => GenericMessage (M1 S s f) where
  type GenericFieldCount (M1 S s f) = GenericFieldCount f
  genericEncodeMessage num (M1 x)   = genericEncodeMessage num x
  genericDecodeMessage num          = M1 <$> genericDecodeMessage num
  genericDotProto _                 = map applyName $ genericDotProto (proxy# :: Proxy# f)
    where
      applyName :: DotProtoField -> DotProtoField
      applyName mp = mp { dotProtoFieldName = fromMaybe Anonymous newName}
      -- [issue] this probably doesn't match the intended name generating semantics

      newName :: Maybe DotProtoIdentifier
      newName = guard (not (null name)) $> Single name
        where
          name = selName (undefined :: S1 s f ())

instance GenericMessage f => GenericMessage (M1 C t f) where
  type GenericFieldCount (M1 C t f) = GenericFieldCount f
  genericEncodeMessage num (M1 x)   = genericEncodeMessage num x
  genericDecodeMessage num          = M1 <$> genericDecodeMessage num
  genericDotProto _                 = genericDotProto (proxy# :: Proxy# f)

instance GenericMessage f => GenericMessage (M1 D t f) where
  type GenericFieldCount (M1 D t f) = GenericFieldCount f
  genericEncodeMessage num (M1 x)   = genericEncodeMessage num x
  genericDecodeMessage num          = M1 <$> genericDecodeMessage num
  genericDotProto _                 = genericDotProto (proxy# :: Proxy# f)
